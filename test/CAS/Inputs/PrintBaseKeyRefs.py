#!/usr/bin/env python3
#
# Usage: PrintBaseKeyRefs.py swift-cache-tool llvm-cas cas-path jobs.txt
#
# For the compile jobs with primary inputs in the output of `swiftc -###`,
# print the number of the jobs, the number of the unique lists of references
# in their compile job base cache keys, and the references in the first list,
# one per line.

import os
import shlex
import subprocess
import sys


def split_command_line(line):
    if os.name != 'nt':
        return shlex.split(line)
    # On Windows, the driver quotes the arguments using the Windows conventions,
    # so use CommandLineToArgvW to split the command line.
    import ctypes
    from ctypes import wintypes
    command_line_to_argv = ctypes.windll.shell32.CommandLineToArgvW
    command_line_to_argv.argtypes = [wintypes.LPCWSTR,
                                     ctypes.POINTER(ctypes.c_int)]
    command_line_to_argv.restype = ctypes.POINTER(wintypes.LPWSTR)
    argc = ctypes.c_int()
    argv = command_line_to_argv(line, ctypes.byref(argc))
    try:
        return [argv[i] for i in range(argc.value)]
    finally:
        ctypes.windll.kernel32.LocalFree(ctypes.cast(argv, ctypes.c_void_p))


cache_tool, llvm_cas, cas_path, jobs_file = sys.argv[1:5]

refs_per_job = []
with open(jobs_file, 'r') as file:
    for line in file:
        args = split_command_line(line.strip())
        if '-frontend' not in args or '-primary-file' not in args:
            continue
        base_key = subprocess.check_output(
            [cache_tool, '-cas-path', cas_path,
             '-cache-tool-action', 'print-base-key', '--'] + args,
            text=True).strip()
        refs = subprocess.check_output(
            [llvm_cas, '--cas', cas_path, '--ls-node-refs', base_key],
            text=True).split()
        refs_per_job.append(tuple(refs))

print(f'jobs: {len(refs_per_job)}')
print(f'unique: {len(set(refs_per_job))}')
for ref in refs_per_job[0] if refs_per_job else []:
    print(ref)
