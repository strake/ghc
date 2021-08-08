#!/usr/bin/env python

# A linter to warn when binary files are added to the repository

import sys
import os
import json

path = sys.argv[1]
warnings = []
if os.path.isfile(path):
    with open(path) as f:
        if '\0' in f.read(8000):
            warning = {
                'severity': 'warning',
                'message': 'This file appears to be a binary file; does it really belong in the repository?'
            }
            warnings.append(warning)

print json.dumps(warnings)
