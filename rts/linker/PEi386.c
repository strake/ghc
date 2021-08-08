/* --------------------------------------------------------------------------
 * PEi386(+) specifics (Win32 targets)
 * ------------------------------------------------------------------------*/

/* The information for this linker comes from
      Microsoft Portable Executable
      and Common Object File Format Specification
      revision 8.3 February 2013

   It can be found online at:

      https://msdn.microsoft.com/en-us/windows/hardware/gg463119.aspx

   Things move, so if that fails, try searching for it via

      http://www.google.com/search?q=PE+COFF+specification

   The ultimate reference for the PE format is the Winnt.h
   header file that comes with the Platform SDKs; as always,
   implementations will drift wrt their documentation.

   A good background article on the PE format is Matt Pietrek's
   March 1994 article in Microsoft System Journal (MSJ)
   (Vol.9, No. 3): "Peering Inside the PE: A Tour of the
   Win32 Portable Executable File Format." The info in there
   has recently been updated in a two part article in
   MSDN magazine, issues Feb and March 2002,
   "Inside Windows: An In-Depth Look into the Win32 Portable
   Executable File Format"

   John Levine's book "Linkers and Loaders" contains useful
   info on PE too.

   The PE specification doesn't specify how to do the actual
   relocations. For this reason, and because both PE and ELF are
   based on COFF, the relocations for the PEi386+ code is based on
   the ELF relocations for the equivalent relocation type.

   The ELF ABI can be found at

   http://www.x86-64.org/documentation/abi.pdf

   The current code is based on version 0.99.6 - October 2013
*/

#include "Rts.h"

#if defined(x86_64_HOST_ARCH)
#define USED_IF_x86_64_HOST_ARCH    /* Nothing */
#else
#define USED_IF_x86_64_HOST_ARCH    STG_UNUSED
#endif

#ifdef mingw32_HOST_OS

#include "RtsUtils.h"
#include "RtsSymbolInfo.h"
#include "GetEnv.h"
#include "linker/PEi386.h"
#include "LinkerInternals.h"

#include <windows.h>
#include <shfolder.h> /* SHGetFolderPathW */
#include <math.h>
#include <wchar.h>

static UChar *cstring_from_COFF_symbol_name(
    UChar* name,
    UChar* strtab);

static size_t makeSymbolExtra_PEi386(
    ObjectCode* oc,
    size_t s,
    char* symbol);

static void addDLLHandle(
    pathchar* dll_name,
    HINSTANCE instance);

static int verifyCOFFHeader(
    COFF_header *hdr,
    pathchar *filename);

/* Add ld symbol for PE image base. */
#if defined(__GNUC__)
#define __ImageBase __MINGW_LSYMBOL(_image_base__)
#endif

/* Get the base of the module.       */
/* This symbol is defined by ld.     */
extern IMAGE_DOS_HEADER __ImageBase;
#define __image_base (void*)((HINSTANCE)&__ImageBase)

// MingW-w64 is missing these from the implementation. So we have to look them up
typedef DLL_DIRECTORY_COOKIE(WINAPI *LPAddDLLDirectory)(PCWSTR NewDirectory);
typedef WINBOOL(WINAPI *LPRemoveDLLDirectory)(DLL_DIRECTORY_COOKIE Cookie);

void initLinker_PEi386()
{
    if (!ghciInsertSymbolTable(WSTR("(GHCi/Ld special symbols)"),
                               symhash, "__image_base__", __image_base, HS_BOOL_TRUE, NULL)) {
        barf("ghciInsertSymbolTable failed");
    }

#if defined(mingw32_HOST_OS)
    /*
     * These two libraries cause problems when added to the static link,
     * but are necessary for resolving symbols in GHCi, hence we load
     * them manually here.
     */
    addDLL(WSTR("msvcrt"));
    addDLL(WSTR("kernel32"));
    addDLLHandle(WSTR("*.exe"), GetModuleHandle(NULL));
#endif
}

/* A record for storing handles into DLLs. */
typedef
struct _OpenedDLL {
    pathchar*          name;
    struct _OpenedDLL* next;
    HINSTANCE instance;
}
    OpenedDLL;

/* A list thereof. */
static OpenedDLL* opened_dlls = NULL;

/* A record for storing indirectly linked functions from DLLs. */
typedef
struct _IndirectAddr {
    SymbolAddr*           addr;
    struct _IndirectAddr* next;
}
    IndirectAddr;

/* A list thereof. */
static IndirectAddr* indirects = NULL;

/* Adds a DLL instance to the list of DLLs in which to search for symbols. */
static void addDLLHandle(pathchar* dll_name, HINSTANCE instance) {
    OpenedDLL* o_dll;
    o_dll = stgMallocBytes( sizeof(OpenedDLL), "addDLLHandle" );
    o_dll->name     = dll_name ? pathdup(dll_name) : NULL;
    o_dll->instance = instance;
    o_dll->next     = opened_dlls;
    opened_dlls     = o_dll;
}

void freePreloadObjectFile_PEi386(ObjectCode *oc)
{
    VirtualFree(oc->image - PEi386_IMAGE_OFFSET, 0, MEM_RELEASE);

    IndirectAddr *ia, *ia_next;
    ia = indirects;
    while (ia != NULL) {
        ia_next = ia->next;
        stgFree(ia);
        ia = ia_next;
    }
    indirects = NULL;
}

const char *
addDLL_PEi386( pathchar *dll_name )
{
   /* ------------------- Win32 DLL loader ------------------- */

   pathchar*      buf;
   OpenedDLL* o_dll;
   HINSTANCE  instance;

   IF_DEBUG(linker, debugBelch("\naddDLL; dll_name = `%" PATH_FMT "'\n", dll_name));

   /* See if we've already got it, and ignore if so. */
   for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
      if (0 == pathcmp(o_dll->name, dll_name))
         return NULL;
   }

   /* The file name has no suffix (yet) so that we can try
      both foo.dll and foo.drv

      The documentation for LoadLibrary says:
        If no file name extension is specified in the lpFileName
        parameter, the default library extension .dll is
        appended. However, the file name string can include a trailing
        point character (.) to indicate that the module name has no
        extension. */

   size_t bufsize = pathlen(dll_name) + 10;
   buf = stgMallocBytes(bufsize * sizeof(wchar_t), "addDLL");

   /* These are ordered by probability of success and order we'd like them */
   const wchar_t *formats[] = { L"%ls.DLL", L"%ls.DRV", L"lib%ls.DLL", L"%ls" };
   const DWORD flags[]      = { LOAD_LIBRARY_SEARCH_USER_DIRS | LOAD_LIBRARY_SEARCH_DEFAULT_DIRS, 0 };

   int cFormat;
   int cFlag;
   int flags_start = 1; // Assume we don't support the new API

   /* Detect if newer API are available, if not, skip the first flags entry */
   if (GetProcAddress((HMODULE)LoadLibraryW(L"Kernel32.DLL"), "AddDllDirectory")) {
       flags_start = 0;
   }

   /* Iterate through the possible flags and formats */
   for (cFlag = flags_start; cFlag < 2; cFlag++)
   {
       for (cFormat = 0; cFormat < 4; cFormat++)
       {
           snwprintf(buf, bufsize, formats[cFormat], dll_name);
           instance = LoadLibraryExW(buf, NULL, flags[cFlag]);
           if (instance == NULL)
           {
               if (GetLastError() != ERROR_MOD_NOT_FOUND)
               {
                   goto error;
               }
           }
           else
           {
               break; // We're done. DLL has been loaded.
           }
       }
   }

   // Check if we managed to load the DLL
   if (instance == NULL) {
       goto error;
   }

   stgFree(buf);

   addDLLHandle(dll_name, instance);

   return NULL;

error:
   stgFree(buf);
   sysErrorBelch("addDLL: %" PATH_FMT " (Win32 error %lu)", dll_name, GetLastError());

   /* LoadLibrary failed; return a ptr to the error msg. */
   return "addDLL: could not load DLL";
}

pathchar* findSystemLibrary_PEi386( pathchar* dll_name )
{
    const unsigned int init_buf_size = 1024;
    unsigned int bufsize     = init_buf_size;
    wchar_t* result = malloc(sizeof(wchar_t) * bufsize);
    DWORD wResult   = SearchPathW(NULL, dll_name, NULL, bufsize, result, NULL);

    if (wResult > bufsize) {
        result  = realloc(result, sizeof(wchar_t) * wResult);
        wResult = SearchPathW(NULL, dll_name, NULL, wResult, result, NULL);
    }


    if (!wResult) {
        free(result);
        return NULL;
    }

    return result;
}

HsPtr addLibrarySearchPath_PEi386(pathchar* dll_path)
{
    HINSTANCE hDLL = LoadLibraryW(L"Kernel32.DLL");
    LPAddDLLDirectory AddDllDirectory = (LPAddDLLDirectory)GetProcAddress((HMODULE)hDLL, "AddDllDirectory");

    HsPtr result = NULL;

    const unsigned int init_buf_size = 4096;
    int bufsize = init_buf_size;

    // Make sure the path is an absolute path
    WCHAR* abs_path = malloc(sizeof(WCHAR) * init_buf_size);
    DWORD wResult = GetFullPathNameW(dll_path, bufsize, abs_path, NULL);
    if (!wResult){
        sysErrorBelch("addLibrarySearchPath[GetFullPathNameW]: %" PATH_FMT " (Win32 error %lu)", dll_path, GetLastError());
    }
    else if (wResult > init_buf_size) {
        abs_path = realloc(abs_path, sizeof(WCHAR) * wResult);
        if (!GetFullPathNameW(dll_path, bufsize, abs_path, NULL)) {
            sysErrorBelch("addLibrarySearchPath[GetFullPathNameW]: %" PATH_FMT " (Win32 error %lu)", dll_path, GetLastError());
        }
    }

    if (AddDllDirectory) {
        result = AddDllDirectory(abs_path);
    }
    else
    {
        warnMissingKBLibraryPaths();
        WCHAR* str = malloc(sizeof(WCHAR) * init_buf_size);
        wResult = GetEnvironmentVariableW(L"PATH", str, bufsize);

        if (wResult > init_buf_size) {
            str = realloc(str, sizeof(WCHAR) * wResult);
            bufsize = wResult;
            wResult = GetEnvironmentVariableW(L"PATH", str, bufsize);
            if (!wResult) {
                sysErrorBelch("addLibrarySearchPath[GetEnvironmentVariableW]: %" PATH_FMT " (Win32 error %lu)", dll_path, GetLastError());
            }
        }

        bufsize = wResult + 2 + pathlen(abs_path);
        wchar_t* newPath = malloc(sizeof(wchar_t) * bufsize);

        wcscpy(newPath, abs_path);
        wcscat(newPath, L";");
        wcscat(newPath, str);
        if (!SetEnvironmentVariableW(L"PATH", (LPCWSTR)newPath)) {
            sysErrorBelch("addLibrarySearchPath[SetEnvironmentVariableW]: %" PATH_FMT " (Win32 error %lu)", abs_path, GetLastError());
        }

        free(newPath);
        free(abs_path);

        return str;
    }

    if (!result) {
        sysErrorBelch("addLibrarySearchPath: %" PATH_FMT " (Win32 error %lu)", abs_path, GetLastError());
        free(abs_path);
        return NULL;
    }

    free(abs_path);
    return result;
}

HsBool removeLibrarySearchPath_PEi386(HsPtr dll_path_index)
{
    HsBool result = 0;

    if (dll_path_index != NULL) {
        HINSTANCE hDLL = LoadLibraryW(L"Kernel32.DLL");
        LPRemoveDLLDirectory RemoveDllDirectory = (LPRemoveDLLDirectory)GetProcAddress((HMODULE)hDLL, "RemoveDllDirectory");

        if (RemoveDllDirectory) {
            result = RemoveDllDirectory(dll_path_index);
            // dll_path_index is now invalid, do not use it after this point.
        }
        else
        {
            warnMissingKBLibraryPaths();
            result = SetEnvironmentVariableW(L"PATH", (LPCWSTR)dll_path_index);
            free(dll_path_index);
        }

        if (!result) {
            sysErrorBelch("removeLibrarySearchPath: (Win32 error %lu)", GetLastError());
            return HS_BOOL_FALSE;
        }
    }

    return result == 0 ? HS_BOOL_TRUE : HS_BOOL_FALSE;
}


/* We assume file pointer is right at the
   beginning of COFF object.
 */
char *
allocateImageAndTrampolines (
   pathchar* arch_name, char* member_name,
   FILE* f USED_IF_x86_64_HOST_ARCH,
   int size,
   int isThin USED_IF_x86_64_HOST_ARCH)
{
   char* image;
#if defined(x86_64_HOST_ARCH)
   if (!isThin)
   {
       /* PeCoff contains number of symbols right in it's header, so
          we can reserve the room for symbolExtras right here. */
       COFF_header hdr;
       size_t n;

       n = fread(&hdr, 1, sizeof_COFF_header, f);
       if (n != sizeof(COFF_header)) {
           errorBelch("getNumberOfSymbols: error whilst reading `%s' header in `%S'",
               member_name, arch_name);
           return NULL;
       }
       fseek(f, -sizeof_COFF_header, SEEK_CUR);

       if (!verifyCOFFHeader(&hdr, arch_name)) {
           return 0;
       }

       /* We get back 8-byte aligned memory (is that guaranteed?), but
          the offsets to the sections within the file are all 4 mod 8
          (is that guaranteed?). We therefore need to offset the image
          by 4, so that all the pointers are 8-byte aligned, so that
          pointer tagging works. */
       /* For 32-bit case we don't need this, hence we use macro PEi386_IMAGE_OFFSET,
          which equals to 4 for 64-bit case and 0 for 32-bit case. */
       /* We allocate trampolines area for all symbols right behind
          image data, aligned on 8. */
       size = ((PEi386_IMAGE_OFFSET + size + 0x7) & ~0x7)
           + hdr.NumberOfSymbols * sizeof(SymbolExtra);
   }
#endif
   image = VirtualAlloc(NULL, size,
                        MEM_RESERVE | MEM_COMMIT,
                        PAGE_EXECUTE_READWRITE);

   if (image == NULL) {
       errorBelch("%" PATH_FMT ": failed to allocate memory for image for %s",
                  arch_name, member_name);
       return NULL;
   }

   return image + PEi386_IMAGE_OFFSET;
}

int findAndLoadImportLibrary(ObjectCode* oc)
{
    int i;

    COFF_header*  hdr;
    COFF_section* sectab;
    COFF_symbol*  symtab;
    UChar*        strtab;

    hdr = (COFF_header*)(oc->image);
    sectab = (COFF_section*)(
        ((UChar*)(oc->image))
        + sizeof_COFF_header + hdr->SizeOfOptionalHeader
        );

    symtab = (COFF_symbol*)(
        ((UChar*)(oc->image))
        + hdr->PointerToSymbolTable
        );

    strtab = ((UChar*)symtab)
        + hdr->NumberOfSymbols * sizeof_COFF_symbol;

    for (i = 0; i < oc->n_sections; i++)
    {
        COFF_section* sectab_i
            = (COFF_section*)myindex(sizeof_COFF_section, sectab, i);

        char *secname = cstring_from_section_name(sectab_i->Name, strtab);

        // Find the first entry containing a valid .idata$7 section.
        if (strcmp(secname, ".idata$7") == 0) {
            /* First load the containing DLL if not loaded. */
            Section section = oc->sections[i];

            pathchar* dirName = pathdir(oc->fileName);
            HsPtr token       = addLibrarySearchPath(dirName);
            stgFree(dirName);
            char* dllName = (char*)section.start;

            if (strlen(dllName) == 0 || dllName[0] == ' ')
            {
                continue;
            }

            IF_DEBUG(linker, debugBelch("lookupSymbol: on-demand '%ls' => `%s'\n", oc->fileName, dllName));

            pathchar* dll = mkPath(dllName);
            removeLibrarySearchPath(token);

            const char* result = addDLL(dll);
            stgFree(dll);

            if (result != NULL) {
                errorBelch("Could not load `%s'. Reason: %s\n", (char*)dllName, result);
                return 0;
            }

            break;
        }

        stgFree(secname);
    }

    return 1;
}

int checkAndLoadImportLibrary( pathchar* arch_name, char* member_name, FILE* f)
{
    char* image;
    static HsBool load_dll_warn = HS_BOOL_FALSE;

    if (load_dll_warn) { return 0; }

    /* Based on Import Library specification. PE Spec section 7.1 */

    COFF_import_header hdr;
    size_t n;

    n = fread(&hdr, 1, sizeof_COFF_import_Header, f);
    if (n != sizeof(COFF_header)) {
        errorBelch("getNumberOfSymbols: error whilst reading `%s' header in `%" PATH_FMT "'\n",
            member_name, arch_name);
        return 0;
    }

    if (hdr.Sig1 != 0x0 || hdr.Sig2 != 0xFFFF) {
        fseek(f, -sizeof_COFF_import_Header, SEEK_CUR);
        IF_DEBUG(linker, debugBelch("loadArchive: Object `%s` is not an import lib. Skipping...\n", member_name));
        return 0;
    }

    IF_DEBUG(linker, debugBelch("loadArchive: reading %d bytes at %ld\n", hdr.SizeOfData, ftell(f)));

    image = malloc(hdr.SizeOfData);
    n = fread(image, 1, hdr.SizeOfData, f);
    if (n != hdr.SizeOfData) {
        errorBelch("loadArchive: error whilst reading `%s' header in `%" PATH_FMT "'. Did not read enough bytes.\n",
            member_name, arch_name);
    }

    char* symbol  = strtok(image, "\0");
    int symLen    = strlen(symbol) + 1;
    int nameLen   = n - symLen;
    char* dllName = malloc(sizeof(char) * nameLen);
    dllName       = strncpy(dllName, image + symLen, nameLen);
    pathchar* dll = malloc(sizeof(wchar_t) * nameLen);
    mbstowcs(dll, dllName, nameLen);
    free(dllName);

    IF_DEBUG(linker, debugBelch("loadArchive: read symbol %s from lib `%ls'\n", symbol, dll));
    const char* result = addDLL(dll);

    free(image);

    if (result != NULL) {
        errorBelch("Could not load `%ls'. Reason: %s\n", dll, result);
        load_dll_warn = HS_BOOL_TRUE;

        free(dll);
        fseek(f, -(n + sizeof_COFF_import_Header), SEEK_CUR);
        return 0;
    }

    free(dll);
    return 1;
}

static void
printName ( UChar* name, UChar* strtab )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      debugBelch("%s", strtab + strtab_offset );
   } else {
      int i;
      for (i = 0; i < 8; i++) {
         if (name[i] == 0) break;
         debugBelch("%c", name[i] );
      }
   }
}


static void
copyName ( UChar* name, UChar* strtab, UChar* dst, int dstSize )
{
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      strncpy ( (char*)dst, (char*)strtab+strtab_offset, dstSize );
      dst[dstSize-1] = 0;
   } else {
      int i = 0;
      while (1) {
         if (i >= 8) break;
         if (name[i] == 0) break;
         dst[i] = name[i];
         i++;
      }
      dst[i] = 0;
   }
}


static UChar *
cstring_from_COFF_symbol_name ( UChar* name, UChar* strtab )
{
   UChar* newstr;
   /* If the string is longer than 8 bytes, look in the
      string table for it -- this will be correctly zero terminated.
   */
   if (name[0]==0 && name[1]==0 && name[2]==0 && name[3]==0) {
      UInt32 strtab_offset = * (UInt32*)(name+4);
      return ((UChar*)strtab) + strtab_offset;
   }
   /* Otherwise, if shorter than 8 bytes, return the original,
      which by defn is correctly terminated.
   */
   if (name[7]==0) return name;
   /* The annoying case: 8 bytes.  Copy into a temporary
      (XXX which is never freed ...)
   */
   newstr = stgMallocBytes(9, "cstring_from_COFF_symbol_name");
   ASSERT(newstr);
   strncpy((char*)newstr,(char*)name,8);
   newstr[8] = 0;
   return newstr;
}

/* Getting the name of a section is mildly tricky, so we make a
   function for it.  Sadly, in one case we have to copy the string
   (when it is exactly 8 bytes long there's no trailing '\0'), so for
   consistency we *always* copy the string; the caller must free it
*/
char *
cstring_from_section_name (UChar* name, UChar* strtab)
{
    char *newstr;

    if (name[0]=='/') {
        int strtab_offset = strtol((char*)name+1,NULL,10);
        int len = strlen(((char*)strtab) + strtab_offset);

        newstr = stgMallocBytes(len+1, "cstring_from_section_symbol_name");
        strcpy((char*)newstr, (char*)((UChar*)strtab) + strtab_offset);
        return newstr;
    }
    else
    {
        newstr = stgMallocBytes(9, "cstring_from_section_symbol_name");
        ASSERT(newstr);
        strncpy((char*)newstr,(char*)name,8);
        newstr[8] = 0;
        return newstr;
    }
}

/* See Note [mingw-w64 name decoration scheme] */
#ifndef x86_64_HOST_ARCH
static void
zapTrailingAtSign ( UChar* sym )
{
#  define my_isdigit(c) ((c) >= '0' && (c) <= '9')
   int i, j;
   if (sym[0] == 0) return;
   i = 0;
   while (sym[i] != 0) i++;
   i--;
   j = i;
   while (j > 0 && my_isdigit(sym[j])) j--;
   if (j > 0 && sym[j] == '@' && j != i) sym[j] = 0;
#  undef my_isdigit
}
#endif

/* See Note [mingw-w64 name decoration scheme] */
#ifndef x86_64_HOST_ARCH
#define STRIP_LEADING_UNDERSCORE 1
#else
#define STRIP_LEADING_UNDERSCORE 0
#endif

/*
  Note [mingw-w64 name decoration scheme]

  What's going on with name decoration? Well, original code
  have some crufty and ad-hocish paths related mostly to very old
  mingw gcc/binutils/runtime combinations. Now mingw-w64 offers pretty
  uniform and MS-compatible decoration scheme across its tools and runtime.

  The scheme is pretty straightforward: on 32 bit objects symbols are exported
  with underscore prepended (and @ + stack size suffix appended for stdcall
  functions), on 64 bits no underscore is prepended and no suffix is appended
  because we have no stdcall convention on 64 bits.

  See #9218
*/

SymbolAddr*
lookupSymbolInDLLs ( UChar *lbl )
{
    OpenedDLL* o_dll;
    SymbolAddr* sym;

    for (o_dll = opened_dlls; o_dll != NULL; o_dll = o_dll->next) {
        /* debugBelch("look in %ls for %s\n", o_dll->name, lbl); */

        sym = GetProcAddress(o_dll->instance, (char*)(lbl+STRIP_LEADING_UNDERSCORE));
        if (sym != NULL) {
            /*debugBelch("found %s in %s\n", lbl+1,o_dll->name);*/
            return sym;
        }

        /* Ticket #2283.
           Long description: http://support.microsoft.com/kb/132044
           tl;dr:
             If C/C++ compiler sees __declspec(dllimport) ... foo ...
             it generates call *__imp_foo, and __imp_foo here has exactly
             the same semantics as in __imp_foo = GetProcAddress(..., "foo")
         */
        if (sym == NULL && strncmp ((const char*)lbl, "__imp_", 6) == 0) {
            sym = GetProcAddress(o_dll->instance, (char*)(lbl+6+STRIP_LEADING_UNDERSCORE));
            if (sym != NULL) {
                IndirectAddr* ret;
                ret = stgMallocBytes( sizeof(IndirectAddr), "lookupSymbolInDLLs" );
                ret->addr = sym;
                ret->next = indirects;
                indirects = ret;
                IF_DEBUG(linker,
                  debugBelch("warning: %s from %S is linked instead of %s\n",
                             (char*)(lbl+6+STRIP_LEADING_UNDERSCORE), o_dll->name, (char*)lbl));
                return (void*) & ret->addr;
               }
        }

        sym = GetProcAddress(o_dll->instance, (char*)lbl);
        if (sym != NULL) {
            /*debugBelch("found %s in %s\n", lbl,o_dll->name);*/
            return sym;
           }
    }
    return NULL;
}

static int
verifyCOFFHeader (COFF_header *hdr, pathchar *fileName)
{
#if defined(i386_HOST_ARCH)
   if (hdr->Machine != 0x14c) {
      errorBelch("%" PATH_FMT ": Not x86 PEi386", fileName);
      return 0;
   }
#elif defined(x86_64_HOST_ARCH)
   if (hdr->Machine != 0x8664) {
      errorBelch("%" PATH_FMT ": Not x86_64 PEi386", fileName);
      return 0;
   }
#else
   errorBelch("PEi386 not supported on this arch");
#endif

   if (hdr->SizeOfOptionalHeader != 0) {
      errorBelch("%" PATH_FMT ": PEi386 with nonempty optional header",
                 fileName);
      return 0;
   }
   if ( /* (hdr->Characteristics & MYIMAGE_FILE_RELOCS_STRIPPED) || */
        (hdr->Characteristics & MYIMAGE_FILE_EXECUTABLE_IMAGE) ||
        (hdr->Characteristics & MYIMAGE_FILE_DLL) ||
        (hdr->Characteristics & MYIMAGE_FILE_SYSTEM) ) {
      errorBelch("%" PATH_FMT ": Not a PEi386 object file", fileName);
      return 0;
   }
   if ( (hdr->Characteristics & MYIMAGE_FILE_BYTES_REVERSED_HI)
        /* || !(hdr->Characteristics & MYIMAGE_FILE_32BIT_MACHINE) */ ) {
      errorBelch("%" PATH_FMT ": Invalid PEi386 word size or endiannness: %d",
                 fileName,
                 (int)(hdr->Characteristics));
      return 0;
   }
   return 1;
}

int
ocVerifyImage_PEi386 ( ObjectCode* oc )
{
   int i;
   UInt32 j, noRelocs;
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;
   /* debugBelch("\nLOADING %s\n", oc->fileName); */
   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)symtab)
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   if (!verifyCOFFHeader(hdr, oc->fileName)) {
       return 0;
   }

   /* If the string table size is way crazy, this might indicate that
      there are more than 64k relocations, despite claims to the
      contrary.  Hence this test. */
   /* debugBelch("strtab size %d\n", * (UInt32*)strtab); */
#if 0
   if ( (*(UInt32*)strtab) > 600000 ) {
      /* Note that 600k has no special significance other than being
         big enough to handle the almost-2MB-sized lumps that
         constitute HSwin32*.o. */
      debugBelch("PEi386 object has suspiciously large string table; > 64k relocs?");
      return 0;
   }
#endif

   /* .BSS Section is initialized in ocGetNames_PEi386
      but we need the Sections array initialized here already. */
   Section *sections;
   sections = (Section*)stgCallocBytes(
       sizeof(Section),
       hdr->NumberOfSections + 1, /* +1 for the global BSS section see ocGetNames_PEi386 */
       "ocVerifyImage_PEi386(sections)");
   oc->sections = sections;
   oc->n_sections = hdr->NumberOfSections + 1;

   /* Initialize the Sections */
   for (i = 0; i < hdr->NumberOfSections; i++) {
       COFF_section* sectab_i
           = (COFF_section*)
           myindex(sizeof_COFF_section, sectab, i);

       /* Calculate the start of the data section */
       sections[i].start = oc->image + sectab_i->PointerToRawData;
   }

   /* No further verification after this point; only debug printing. */
   i = 0;
   IF_DEBUG(linker, i=1);
   if (i == 0) return 1;

   debugBelch("sectab offset = %" FMT_SizeT "\n",
              ((UChar*)sectab) - ((UChar*)hdr) );
   debugBelch("symtab offset = %" FMT_SizeT "\n",
              ((UChar*)symtab) - ((UChar*)hdr) );
   debugBelch("strtab offset = %" FMT_SizeT "\n",
              ((UChar*)strtab) - ((UChar*)hdr) );

   debugBelch("\n" );
   debugBelch( "Machine:           0x%x\n", (UInt32)(hdr->Machine) );
   debugBelch( "# sections:        %d\n",   (UInt32)(hdr->NumberOfSections) );
   debugBelch( "time/date:         0x%x\n", (UInt32)(hdr->TimeDateStamp) );
   debugBelch( "symtab offset:     %d\n",   (UInt32)(hdr->PointerToSymbolTable) );
   debugBelch( "# symbols:         %d\n",   (UInt32)(hdr->NumberOfSymbols) );
   debugBelch( "sz of opt hdr:     %d\n",   (UInt32)(hdr->SizeOfOptionalHeader) );
   debugBelch( "characteristics:   0x%x\n", (UInt32)(hdr->Characteristics) );

   /* Print the section table. */
   debugBelch("\n" );
   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_reloc* reltab;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      Section section = sections[i];
      debugBelch(
                "\n"
                "section %d\n"
                "     name `",
                i
              );
      printName ( sectab_i->Name, strtab );
      debugBelch(
                "'\n"
                "    vsize %d\n"
                "    vaddr %d\n"
                "  data sz %d\n"
                " data off 0x%p\n"
                "  num rel %d\n"
                "  off rel %d\n"
                "  ptr raw 0x%x\n",
                sectab_i->VirtualSize,
                sectab_i->VirtualAddress,
                sectab_i->SizeOfRawData,
                section.start,
                sectab_i->NumberOfRelocations,
                sectab_i->PointerToRelocations,
                sectab_i->PointerToRawData
              );
      reltab = (COFF_reloc*) (
                  ((UChar*)(oc->image)) + sectab_i->PointerToRelocations
               );

      if ( sectab_i->Characteristics & MYIMAGE_SCN_LNK_NRELOC_OVFL ) {
        /* If the relocation field (a short) has overflowed, the
         * real count can be found in the first reloc entry.
         *
         * See Section 4.1 (last para) of the PE spec (rev6.0).
         */
        COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, 0 );
        noRelocs = rel->VirtualAddress;
        j = 1;
      } else {
        noRelocs = sectab_i->NumberOfRelocations;
        j = 0;
      }

      for (; j < noRelocs; j++) {
         COFF_symbol* sym;
         COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, j );
         debugBelch(
                   "        type 0x%-4x   vaddr 0x%-8x   name `",
                   (UInt32)rel->Type,
                   rel->VirtualAddress );
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol, symtab, rel->SymbolTableIndex );
         /* Hmm..mysterious looking offset - what's it for? SOF */
         printName ( sym->Name, strtab -10 );
         debugBelch("'\n" );
      }

      debugBelch("\n" );
   }
   debugBelch("\n" );
   debugBelch("string table has size 0x%x\n", * (UInt32*)strtab );
   debugBelch("---START of string table---\n");
   for (i = 4; i < *(Int32*)strtab; i++) {
      if (strtab[i] == 0)
         debugBelch("\n"); else
         debugBelch("%c", strtab[i] );
   }
   debugBelch("--- END  of string table---\n");

   debugBelch("\n" );
   i = 0;
   while (1) {
      COFF_symbol* symtab_i;
      if (i >= (Int32)(hdr->NumberOfSymbols)) break;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, symtab, i );
      debugBelch(
                "symbol %d\n"
                "     name `",
                i
              );
      printName ( symtab_i->Name, strtab );
      debugBelch(
                "'\n"
                "    value 0x%x\n"
                "   1+sec# %d\n"
                "     type 0x%x\n"
                "   sclass 0x%x\n"
                "     nAux %d\n",
                symtab_i->Value,
                (Int32)(symtab_i->SectionNumber),
                (UInt32)symtab_i->Type,
                (UInt32)symtab_i->StorageClass,
                (UInt32)symtab_i->NumberOfAuxSymbols
              );
      i += symtab_i->NumberOfAuxSymbols;
      i++;
   }

   debugBelch("\n" );
   return 1;
}

int
ocGetNames_PEi386 ( ObjectCode* oc )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UChar*     sname;
   SymbolAddr* addr;
   int        i;

   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)(oc->image))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   /* Allocate space for any (local, anonymous) .bss sections. */

   for (i = 0; i < hdr->NumberOfSections; i++) {
      UInt32 bss_sz;
      UChar* zspace;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );

      char *secname = cstring_from_section_name(sectab_i->Name, strtab);

      if (0 != strcmp(secname, ".bss")) {
          stgFree(secname);
          continue;
      }

      stgFree(secname);

      /* sof 10/05: the PE spec text isn't too clear regarding what
       * the SizeOfRawData field is supposed to hold for object
       * file sections containing just uninitialized data -- for executables,
       * it is supposed to be zero; unclear what it's supposed to be
       * for object files. However, VirtualSize is guaranteed to be
       * zero for object files, which definitely suggests that SizeOfRawData
       * will be non-zero (where else would the size of this .bss section be
       * stored?) Looking at the COFF_section info for incoming object files,
       * this certainly appears to be the case.
       *
       * => I suspect we've been incorrectly handling .bss sections in (relocatable)
       * object files up until now. This turned out to bite us with ghc-6.4.1's use
       * of gcc-3.4.x, which has started to emit initially-zeroed-out local 'static'
       * variable decls into the .bss section. (The specific function in Q which
       * triggered this is libraries/base/cbits/dirUtils.c:__hscore_getFolderPath())
       */
      if (sectab_i->VirtualSize == 0 && sectab_i->SizeOfRawData == 0) continue;
      /* This is a non-empty .bss section.
         Allocate zeroed space for it */
      bss_sz = sectab_i->VirtualSize;
      if ( bss_sz < sectab_i->SizeOfRawData) { bss_sz = sectab_i->SizeOfRawData; }
      zspace = stgCallocBytes(1, bss_sz, "ocGetNames_PEi386(anonymous bss)");
      oc->sections[i].start = zspace;
      addProddableBlock(oc, zspace, bss_sz);
      /* debugBelch("BSS anon section at 0x%x\n", zspace); */
   }

   /* Copy section information into the ObjectCode. */

   for (i = 0; i < hdr->NumberOfSections; i++) {
      UChar* start;
      UChar* end;
      UInt32 sz;

      /* By default consider all section as CODE or DATA, which means we want to load them. */
      SectionKind kind
          = SECTIONKIND_CODE_OR_RODATA;
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      Section section = oc->sections[i];

      char *secname = cstring_from_section_name(sectab_i->Name, strtab);

      IF_DEBUG(linker, debugBelch("section name = %s\n", secname ));

      /* The PE file section flag indicates whether the section contains code or data. */
      if (sectab_i->Characteristics & MYIMAGE_SCN_CNT_CODE ||
          sectab_i->Characteristics & MYIMAGE_SCN_CNT_INITIALIZED_DATA)
         kind = SECTIONKIND_CODE_OR_RODATA;

      /* Check next if it contains any uninitialized data */
      if (sectab_i->Characteristics & MYIMAGE_SCN_CNT_UNINITIALIZED_DATA)
         kind = SECTIONKIND_RWDATA;

      /* Finally check if it can be discarded. This will also ignore .debug sections */
      if (sectab_i->Characteristics & MYIMAGE_SCN_MEM_DISCARDABLE ||
          sectab_i->Characteristics & MYIMAGE_SCN_LNK_REMOVE)
          kind = SECTIONKIND_OTHER;

      if (0==strcmp(".ctors", (char*)secname))
         kind = SECTIONKIND_INIT_ARRAY;

      ASSERT(sectab_i->SizeOfRawData == 0 || sectab_i->VirtualSize == 0);
      sz = sectab_i->SizeOfRawData;
      if (sz < sectab_i->VirtualSize) sz = sectab_i->VirtualSize;

      start = section.start;
      end   = start + sz - 1;

      if (kind != SECTIONKIND_OTHER && end >= start) {
          addSection(&oc->sections[i], kind, SECTION_NOMEM, start, sz, 0, 0, 0);
          addProddableBlock(oc, start, sz);
      }

      stgFree(secname);
   }

   /* Copy exported symbols into the ObjectCode. */

   oc->n_symbols = hdr->NumberOfSymbols;
   oc->symbols   = stgCallocBytes(sizeof(SymbolName*), oc->n_symbols,
                                  "ocGetNames_PEi386(oc->symbols)");

   /* Work out the size of the global BSS section */
   StgWord globalBssSize = 0;
   for (i=0; i < (int)hdr->NumberOfSymbols; i++) {
      COFF_symbol* symtab_i;
       symtab_i = (COFF_symbol*)
           myindex ( sizeof_COFF_symbol, symtab, i );
       if (symtab_i->SectionNumber == MYIMAGE_SYM_UNDEFINED
           && symtab_i->Value > 0
           && symtab_i->StorageClass != MYIMAGE_SYM_CLASS_SECTION) {
           globalBssSize += symtab_i->Value;
       }
       i += symtab_i->NumberOfAuxSymbols;
   }

   /* Allocate BSS space */
   SymbolAddr* bss = NULL;
   if (globalBssSize > 0) {
       bss = stgCallocBytes(1, globalBssSize,
                            "ocGetNames_PEi386(non-anonymous bss)");
       addSection(&oc->sections[oc->n_sections-1],
                  SECTIONKIND_RWDATA, SECTION_MALLOC,
                  bss, globalBssSize, 0, 0, 0);
       IF_DEBUG(linker, debugBelch("bss @ %p %" FMT_Word "\n", bss, globalBssSize));
       addProddableBlock(oc, bss, globalBssSize);
   } else {
       addSection(&oc->sections[oc->n_sections-1],
                  SECTIONKIND_OTHER, SECTION_NOMEM, NULL, 0, 0, 0, 0);
   }

   for (i = 0; i < oc->n_symbols; i++) {
      COFF_symbol* symtab_i;
      symtab_i = (COFF_symbol*)
                 myindex ( sizeof_COFF_symbol, symtab, i );

      addr  = NULL;
      HsBool isWeak = HS_BOOL_FALSE;
      if (   symtab_i->SectionNumber != MYIMAGE_SYM_UNDEFINED
          && symtab_i->SectionNumber > 0) {
         /* This symbol is global and defined, viz, exported */
         /* for MYIMAGE_SYMCLASS_EXTERNAL
                && !MYIMAGE_SYM_UNDEFINED,
            the address of the symbol is:
                address of relevant section + offset in section
         */
         COFF_section* sectabent
            = (COFF_section*) myindex ( sizeof_COFF_section,
                                        sectab,
                                        symtab_i->SectionNumber-1 );
         if (symtab_i->StorageClass == MYIMAGE_SYM_CLASS_EXTERNAL
            || (   symtab_i->StorageClass == MYIMAGE_SYM_CLASS_STATIC
                && sectabent->Characteristics & MYIMAGE_SCN_LNK_COMDAT)
            ) {
                 addr = (void*)((size_t)oc->sections[symtab_i->SectionNumber-1].start
                      + symtab_i->Value);
                 if (sectabent->Characteristics & MYIMAGE_SCN_LNK_COMDAT) {
                    isWeak = HS_BOOL_TRUE;
              }
         }
      }
      else if (symtab_i->StorageClass == MYIMAGE_SYM_CLASS_WEAK_EXTERNAL) {
          isWeak = HS_BOOL_TRUE;
      }
      else if (  symtab_i->SectionNumber == MYIMAGE_SYM_UNDEFINED
              && symtab_i->Value > 0) {
         /* This symbol isn't in any section at all, ie, global bss.
            Allocate zeroed space for it from the BSS section */
          addr = bss;
          bss = (SymbolAddr*)((StgWord)bss + (StgWord)symtab_i->Value);
          IF_DEBUG(linker, debugBelch("bss symbol @ %p %u\n", addr, symtab_i->Value));
      }

      sname = cstring_from_COFF_symbol_name(symtab_i->Name, strtab);
      if (addr != NULL || isWeak == HS_BOOL_TRUE) {

         /* debugBelch("addSymbol %p `%s' Weak:%lld \n", addr, sname, isWeak); */
         IF_DEBUG(linker, debugBelch("addSymbol %p `%s'\n", addr,sname);)
         ASSERT(i >= 0 && i < oc->n_symbols);
         /* cstring_from_COFF_symbol_name always succeeds. */
         oc->symbols[i] = (SymbolName*)sname;
         if (isWeak == HS_BOOL_TRUE) {
             setWeakSymbol(oc, sname);
         }

         if (! ghciInsertSymbolTable(oc->fileName, symhash, (SymbolName*)sname, addr,
                                     isWeak, oc)) {
             return 0;
         }
      } else {
          /* We're skipping the symbol, but if we ever load this
          object file we'll want to skip it then too. */
          oc->symbols[i] = NULL;

#        if 0
         debugBelch(
                   "IGNORING symbol %d\n"
                   "     name `",
                   i
                 );
         printName ( symtab_i->Name, strtab );
         debugBelch(
                   "'\n"
                   "    value 0x%x\n"
                   "   1+sec# %d\n"
                   "     type 0x%x\n"
                   "   sclass 0x%x\n"
                   "     nAux %d\n",
                   symtab_i->Value,
                   (Int32)(symtab_i->SectionNumber),
                   (UInt32)symtab_i->Type,
                   (UInt32)symtab_i->StorageClass,
                   (UInt32)symtab_i->NumberOfAuxSymbols
                 );
#        endif
      }

      i += symtab_i->NumberOfAuxSymbols;
   }

   return 1;
}

#if defined(x86_64_HOST_ARCH)

/* We've already reserved a room for symbol extras in loadObj,
 * so simply set correct pointer here.
 */
int
ocAllocateSymbolExtras_PEi386 ( ObjectCode* oc )
{
   oc->symbol_extras = (SymbolExtra*)(oc->image - PEi386_IMAGE_OFFSET
                                      + ((PEi386_IMAGE_OFFSET + oc->fileSize + 0x7) & ~0x7));
   oc->first_symbol_extra = 0;
   oc->n_symbol_extras = ((COFF_header*)oc->image)->NumberOfSymbols;

   return 1;
}

static size_t
makeSymbolExtra_PEi386( ObjectCode* oc, size_t s, char* symbol )
{
    unsigned int curr_thunk;
    SymbolExtra *extra;

    curr_thunk = oc->first_symbol_extra;
    if (curr_thunk >= oc->n_symbol_extras) {
      barf("Can't allocate thunk for %s", symbol);
    }

    extra = oc->symbol_extras + curr_thunk;

    // jmp *-14(%rip)
    static uint8_t jmp[] = { 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF };
    extra->addr = (uint64_t)s;
    memcpy(extra->jumpIsland, jmp, 6);

    oc->first_symbol_extra++;

    return (size_t)extra->jumpIsland;
}

#endif /* x86_64_HOST_ARCH */

int
ocResolve_PEi386 ( ObjectCode* oc )
{
   COFF_header*  hdr;
   COFF_section* sectab;
   COFF_symbol*  symtab;
   UChar*        strtab;

   UInt32        A;
   size_t        S;
   SymbolAddr*    pP;

   int i;
   UInt32 j, noRelocs;

   /* ToDo: should be variable-sized?  But is at least safe in the
      sense of buffer-overrun-proof. */
   UChar symbol[1000];
   /* debugBelch("resolving for %s\n", oc->fileName); */

   hdr = (COFF_header*)(oc->image);
   sectab = (COFF_section*) (
               ((UChar*)(oc->image))
               + sizeof_COFF_header + hdr->SizeOfOptionalHeader
            );
   symtab = (COFF_symbol*) (
               ((UChar*)(oc->image))
               + hdr->PointerToSymbolTable
            );
   strtab = ((UChar*)(oc->image))
            + hdr->PointerToSymbolTable
            + hdr->NumberOfSymbols * sizeof_COFF_symbol;

   for (i = 0; i < hdr->NumberOfSections; i++) {
      COFF_section* sectab_i
         = (COFF_section*)
           myindex ( sizeof_COFF_section, sectab, i );
      COFF_reloc* reltab
         = (COFF_reloc*) (
              ((UChar*)(oc->image)) + sectab_i->PointerToRelocations
           );
      Section section = oc->sections[i];

      char *secname = cstring_from_section_name(sectab_i->Name, strtab);

      /* Ignore sections called which contain stabs debugging information. */
      if (    0 == strcmp(".stab", (char*)secname)
           || 0 == strcmp(".stabstr", (char*)secname)
           || 0 == strncmp(".pdata", (char*)secname, 6)
           || 0 == strncmp(".xdata", (char*)secname, 6)
           || 0 == strncmp(".debug", (char*)secname, 6)
           || 0 == strcmp(".rdata$zzz", (char*)secname)) {
           stgFree(secname);
           continue;
      }

      stgFree(secname);

      if ( sectab_i->Characteristics & MYIMAGE_SCN_LNK_NRELOC_OVFL ) {
        /* If the relocation field (a short) has overflowed, the
         * real count can be found in the first reloc entry.
         *
         * See Section 4.1 (last para) of the PE spec (rev6.0).
         *
         * Nov2003 update: the GNU linker still doesn't correctly
         * handle the generation of relocatable object files with
         * overflown relocations. Hence the output to warn of potential
         * troubles.
         */
        COFF_reloc* rel = (COFF_reloc*)
                           myindex ( sizeof_COFF_reloc, reltab, 0 );
        noRelocs = rel->VirtualAddress;

        /* 10/05: we now assume (and check for) a GNU ld that is capable
         * of handling object files with (>2^16) of relocs.
         */
#if 0
        debugBelch("WARNING: Overflown relocation field (# relocs found: %u)\n",
                   noRelocs);
#endif
        j = 1;
      } else {
        noRelocs = sectab_i->NumberOfRelocations;
        j = 0;
      }

      for (; j < noRelocs; j++) {
         COFF_symbol* sym;
         COFF_reloc* reltab_j
            = (COFF_reloc*)
              myindex ( sizeof_COFF_reloc, reltab, j );

         /* the location to patch */
         pP = (void*)(
                   (size_t)section.start
                 + reltab_j->VirtualAddress
                 - sectab_i->VirtualAddress
              );
         /* the existing contents of pP */
         A = *(UInt32*)pP;
         /* the symbol to connect to */
         sym = (COFF_symbol*)
               myindex ( sizeof_COFF_symbol,
                         symtab, reltab_j->SymbolTableIndex );
         IF_DEBUG(linker,
                  debugBelch(
                            "reloc sec %2d num %3d:  type 0x%-4x   "
                            "vaddr 0x%-8x   name `",
                            i, j,
                            (UInt32)reltab_j->Type,
                            reltab_j->VirtualAddress );
                            printName ( sym->Name, strtab );
                            debugBelch("'\n" ));

         if (sym->StorageClass == MYIMAGE_SYM_CLASS_STATIC) {
            Section section = oc->sections[sym->SectionNumber-1];
            S = ((size_t)(section.start))
              + ((size_t)(sym->Value));
         } else {
            copyName ( sym->Name, strtab, symbol, 1000-1 );
            S = (size_t) lookupSymbol_( (char*)symbol );
            if ((void*)S == NULL) {

                errorBelch("%" PATH_FMT ": unknown symbol `%s'\n", oc->fileName, symbol);
                return 0;
            }
         }
         /* All supported relocations write at least 4 bytes */
         checkProddableBlock(oc, pP, 4);
         switch (reltab_j->Type) {
#if defined(i386_HOST_ARCH)
            case MYIMAGE_REL_I386_DIR32:
            case MYIMAGE_REL_I386_DIR32NB:
               *(UInt32 *)pP = ((UInt32)S) + A;
               break;
            case MYIMAGE_REL_I386_REL32:
               /* Tricky.  We have to insert a displacement at
                  pP which, when added to the PC for the _next_
                  insn, gives the address of the target (S).
                  Problem is to know the address of the next insn
                  when we only know pP.  We assume that this
                  literal field is always the last in the insn,
                  so that the address of the next insn is pP+4
                  -- hence the constant 4.
                  Also I don't know if A should be added, but so
                  far it has always been zero.

                  SOF 05/2005: 'A' (old contents of *pP) have been observed
                  to contain values other than zero (the 'wx' object file
                  that came with wxhaskell-0.9.4; dunno how it was compiled..).
                  So, add displacement to old value instead of asserting
                  A to be zero. Fixes wxhaskell-related crashes, and no other
                  ill effects have been observed.

                  Update: the reason why we're seeing these more elaborate
                  relocations is due to a switch in how the NCG compiles SRTs
                  and offsets to them from info tables. SRTs live in .(ro)data,
                  while info tables live in .text, causing GAS to emit REL32/DISP32
                  relocations with non-zero values. Adding the displacement is
                  the right thing to do.
               */
               *(UInt32 *)pP = ((UInt32)S) + A - ((UInt32)(size_t)pP) - 4;
               break;
#elif defined(x86_64_HOST_ARCH)
            case 1: /* R_X86_64_64 (ELF constant 1) - IMAGE_REL_AMD64_ADDR64 (PE constant 1) */
               {
                   UInt64 A;
                   checkProddableBlock(oc, pP, 8);
                   A = *(UInt64*)pP;
                   *(UInt64 *)pP = ((UInt64)S) + ((UInt64)A);
                   break;
               }
            case 2: /* R_X86_64_32 (ELF constant 10) - IMAGE_REL_AMD64_ADDR32 (PE constant 2) */
            case 3: /* R_X86_64_32S (ELF constant 11) - IMAGE_REL_AMD64_ADDR32NB (PE constant 3) */
            case 17: /* R_X86_64_32S ELF constant, no PE mapping. See note [ELF constant in PE file] */
               {
                   size_t v;
                   v = S + ((size_t)A);
                   if (v >> 32) {
                       copyName ( sym->Name, strtab, symbol, 1000-1 );
                       S = makeSymbolExtra_PEi386(oc, S, (char *)symbol);
                       /* And retry */
                       v = S + ((size_t)A);
                       if (v >> 32) {
                           barf("IMAGE_REL_AMD64_ADDR32[NB]: High bits are set in %zx for %s",
                                v, (char *)symbol);
                       }
                   }
                   *(UInt32 *)pP = (UInt32)v;
                   break;
               }
            case 4: /* R_X86_64_PC32 (ELF constant 2) - IMAGE_REL_AMD64_REL32 (PE constant 4) */
               {
                   intptr_t v;
                   v = ((intptr_t)S) + ((intptr_t)(Int32)A) - ((intptr_t)pP) - 4;
                   if ((v >> 32) && ((-v) >> 32)) {
                       /* Make the trampoline then */
                       copyName ( sym->Name, strtab, symbol, 1000-1 );
                       S = makeSymbolExtra_PEi386(oc, S, (char *)symbol);
                       /* And retry */
                       v = ((intptr_t)S) + ((intptr_t)(Int32)A) - ((intptr_t)pP) - 4;
                       if ((v >> 32) && ((-v) >> 32)) {
                           barf("IMAGE_REL_AMD64_REL32: High bits are set in %zx for %s",
                                v, (char *)symbol);
                       }
                   }
                   *(UInt32 *)pP = (UInt32)v;
                   break;
               }
#endif
            default:
               debugBelch("%" PATH_FMT ": unhandled PEi386 relocation type %d\n",
                     oc->fileName, reltab_j->Type);
               return 0;
         }

      }
   }

   IF_DEBUG(linker, debugBelch("completed %" PATH_FMT "\n", oc->fileName));
   return 1;
}

/*
  Note [ELF constant in PE file]

  For some reason, the PE files produced by GHC contain a linux
  relocation constant 17 (0x11) in the object files. As far as I (Phyx-) can tell
  this constant doesn't seem like it's coming from GHC, or at least I could not find
  anything in the .s output that GHC produces which specifies the relocation type.

  This leads me to believe that this is a bug in GAS. However because this constant is
  there we must deal with it. This is done by mapping it to the equivalent in behaviour PE
  relocation constant 0x03.

  See #9907
*/

int
ocRunInit_PEi386 ( ObjectCode *oc )
{
    COFF_header*  hdr;
    COFF_section* sectab;
    UChar*        strtab;
    int i;

    hdr = (COFF_header*)(oc->image);
    sectab = (COFF_section*) (
                ((UChar*)(oc->image))
                + sizeof_COFF_header + hdr->SizeOfOptionalHeader
             );
    strtab = ((UChar*)(oc->image))
             + hdr->PointerToSymbolTable
             + hdr->NumberOfSymbols * sizeof_COFF_symbol;

    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    /* TODO: This part is just looking for .ctors section. This can be optimized
       and should for objects compiled with function sections as these produce a
       large amount of sections.

       This can be done by saving the index of the .ctor section in the ObjectCode
       from ocGetNames. Then this loop isn't needed. */
    for (i = 0; i < hdr->NumberOfSections; i++) {
        COFF_section* sectab_i
            = (COFF_section*)
                myindex ( sizeof_COFF_section, sectab, i );
        Section section = oc->sections[i];
        char *secname = cstring_from_section_name(sectab_i->Name, strtab);
        if (0 == strcmp(".ctors", (char*)secname)) {
            UChar *init_startC = section.start;
            init_t *init_start, *init_end, *init;
            init_start = (init_t*)init_startC;
            init_end = (init_t*)(init_startC + sectab_i->SizeOfRawData);
            // ctors are run *backwards*!
            for (init = init_end - 1; init >= init_start; init--) {
                (*init)(argc, argv, envv);
            }
        }
    }
    freeProgEnvv(envc, envv);
    return 1;
}

SymbolAddr *lookupSymbol_PEi386(SymbolName *lbl)
{
    RtsSymbolInfo *pinfo;

    if (!ghciLookupSymbolInfo(symhash, lbl, &pinfo)) {
        IF_DEBUG(linker, debugBelch("lookupSymbol: symbol '%s' not found\n", lbl));

        SymbolAddr* sym;

/* See Note [mingw-w64 name decoration scheme] */
#ifndef x86_64_HOST_ARCH
        zapTrailingAtSign ( (unsigned char*)lbl );
#endif
        sym = lookupSymbolInDLLs((unsigned char*)lbl);
        return sym; // might be NULL if not found
    } else {
#if defined(mingw32_HOST_OS)
        // If Windows, perform initialization of uninitialized
        // Symbols from the C runtime which was loaded above.
        // We do this on lookup to prevent the hit when
        // The symbol isn't being used.
        if (pinfo->value == (void*)0xBAADF00D)
        {
            char symBuffer[50];
            sprintf(symBuffer, "_%s", lbl);
            pinfo->value = GetProcAddress(GetModuleHandle("msvcrt"), symBuffer);
        }
#endif
        return loadSymbol(lbl, pinfo);
    }
}

#endif /* mingw32_HOST_OS */
