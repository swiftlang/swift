#!/usr/bin/env python
"""
Renames files or directories with "module-triple-here" in their names to use
the indicated module triples instead.
"""

from __future__ import print_function

import os
import platform
import shutil
import sys

if len(sys.argv) < 3:
    print('Too few args to ' + sys.argv[0])
    print('Usage: rewrite-module-triples.py <root-dir> <module-triple>...')
    sys.exit(1)

root_dir = sys.argv[1]
triples = sys.argv[2:]


def rewritten_names(name):
    if 'module-triple-here' not in name:
        return []

    return [name.replace('module-triple-here', triple) for triple in triples]


def rewrite(parent, names, copy_fn, rm_fn):
    for name in names:
        new_names = rewritten_names(name)
        if not new_names:
            continue

        path = os.path.join(parent, name)

        for new_name in new_names:
            new_path = os.path.join(parent, new_name)
            if platform.system() == 'Windows':
                copy_fn(u'\\'.join([u'\\\\?', os.path.normpath(path)]),
                        u'\\'.join([u'\\\\?', os.path.normpath(new_path)]))
            else:
                copy_fn(path, new_path)

        if platform.system() == 'Windows':
            rm_fn(u'\\'.join([u'\\\\?', os.path.normpath(path)]))
        else:
            rm_fn(path)


for parent, dirs, files in os.walk(root_dir, topdown=False):
    rewrite(parent, dirs, shutil.copytree, shutil.rmtree)
    rewrite(parent, files, shutil.copyfile, os.remove)
