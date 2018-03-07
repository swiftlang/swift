"""
LLDB Helpers for working with the swift compiler.

Load into LLDB with 'command script import /path/to/lldbToolBox.py'

This will also import LLVM data formatters as well, assuming that llvm is next
to the swift checkout.
"""

import argparse
import os
import shlex
import subprocess
import sys
import tempfile

REPO_BASE = os.path.abspath(os.path.join(__file__, os.pardir, os.pardir,
                                         os.pardir, os.pardir))
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


VIEWCFG_PATH = os.path.join(SWIFT_REPO, "utils", "viewcfg")
BLOCKIFYASM_PATH = os.path.join(SWIFT_REPO, "utils", "dev-scripts",
                                "blockifyasm")


def create_swift_disassemble_viewcfg(debugger, command, exec_ctx, result,
                                     internal_dict):
    """
    This function disassembles the current assembly frame into a temporary file
    and then uses that temporary file as input to blockifyasm | viewcfg. This
    will cause a pdf of the cfg to be opened on Darwin.
    """
    d = exec_ctx.frame.Disassemble()

    with tempfile.TemporaryFile() as f:
        f.write(d)
        f.flush()
        f.seek(0)
        p1 = subprocess.Popen([BLOCKIFYASM_PATH], stdin=f,
                              stdout=subprocess.PIPE)
        subprocess.Popen([VIEWCFG_PATH], stdin=p1.stdout)
        p1.stdout.close()  # Allow p1 to receive a SIGPIPE if p2 exits.


def disassemble_to_file(debugger, command, exec_ctx, result, internal_dict):
    """This function disassembles the current assembly frame into a file specified
    by the user.
    """
    parser = argparse.ArgumentParser(prog='disassemble-to-file', description="""
    Dump the disassembly of the current frame to the specified file.
    """)
    parser.add_argument('file', type=argparse.FileType('w'),
                        default=sys.stdout)
    args = parser.parse_args(shlex.split(command))
    args.file.write(exec_ctx.frame.disassembly)


def __lldb_init_module(debugger, internal_dict):
    import_llvm_dataformatters(debugger)
    debugger.HandleCommand('command script add disassemble-asm-cfg '
                           '-f lldbToolBox.create_swift_disassemble_viewcfg')
    debugger.HandleCommand('command script add disassemble-to-file '
                           '-f lldbToolBox.disassemble_to_file')
