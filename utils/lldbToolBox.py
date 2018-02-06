"""
LLDB Helpers for working with the swift compiler.

Load into LLDB with 'command script import /path/to/lldbToolBox.py'

This will also import LLVM data formatters as well, assuming that llvm is next
to the swift checkout.
"""

import os

REPO_BASE = os.path.abspath(os.path.join(__file__, os.pardir, os.pardir,
                                         os.pardir))
SWIFT_REPO = os.path.join(REPO_BASE, "swift")
LLVM_REPO = os.path.join(REPO_BASE, "llvm")
LLVM_DATAFORMATTER_PATH = os.path.join(LLVM_REPO, "utils",
                                       "lldbDataFormatters.py")


def import_llvm_dataformatters(debugger):
    if not os.access(LLVM_DATAFORMATTER_PATH, os.F_OK):
        print("WARNING! Could not find LLVM data formatters!")
        return
    cmd = 'command script import {}'.format(LLVM_DATAFORMATTER_PATH)
    debugger.HandleCommand(cmd)
    print("Loaded LLVM data formatters.")


def __lldb_init_module(debugger, internal_dict):
    import_llvm_dataformatters(debugger)
