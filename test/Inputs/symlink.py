#!/usr/bin/env python
import os
import subprocess
import sys

if len(sys.argv) < 3:
    print('Too few args to ' + sys.argv[0])
    print('Usage: symlink.py <link_points_to> <link_path> [--dir | --file]')
    sys.exit(1)

points_to = sys.argv[1]
link_path = sys.argv[2]

if sys.platform == 'win32':
    points_to = points_to.replace('/', '\\')
    link_path = link_path.replace('/', '\\')
    if len(sys.argv) >= 4 and sys.argv[3] == '--dir':
        is_dir = True
    elif len(sys.argv) >= 4 and sys.argv[3] == '--file':
        is_dir = False
    else:
        is_dir = os.path.isdir(sys.argv[1])

    # Windows symlink support was introduced in python 3.2
    subprocess.check_call(['cmd.exe', '/C',
                           'mklink ' + ('/D' if is_dir else ''),
                           link_path, points_to])
else:
    os.symlink(points_to, link_path)
