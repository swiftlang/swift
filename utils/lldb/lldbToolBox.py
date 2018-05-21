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

import lldb

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


def disassemble_asm_cfg(debugger, command, exec_ctx, result, internal_dict):
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


def sequence(debugger, command, exec_ctx, result, internal_dict):
    """
    Combine multiple semicolon separated lldb commands into one command.

    This command is particularly useful for defining aliases and breakpoint
    commands. Some examples:

        # Define an alias that prints rax and also steps one instruction.
        command alias xs sequence p/x $rax; stepi

        # Breakpoint command to show the frame's info and arguments.
        breakpoint command add -o 'seq frame info; reg read arg1 arg2 arg3'

        # Override `b` to allow a condition to be specified. For example:
        #     b someMethod if someVar > 2
        command regex b
        s/(.+) if (.+)/seq _regexp-break %1; break mod -c "%2"/
        s/(.*)/_regexp-break %1/
    """
    interpreter = debugger.GetCommandInterpreter()
    for subcommand in command.split(';'):
        subcommand = subcommand.strip()
        if not subcommand:
            continue  # skip empty commands

        ret = lldb.SBCommandReturnObject()
        interpreter.HandleCommand(subcommand, exec_ctx, ret)
        if ret.GetOutput():
            print >>result, ret.GetOutput().strip()

        if not ret.Succeeded():
            result.SetError(ret.GetError())
            result.SetStatus(ret.GetStatus())
            return


def __lldb_init_module(debugger, internal_dict):
    import_llvm_dataformatters(debugger)
    debugger.HandleCommand('command script add disassemble-asm-cfg '
                           '-f lldbToolBox.disassemble_asm_cfg')
    debugger.HandleCommand('command script add disassemble-to-file '
                           '-f lldbToolBox.disassemble_to_file')
    debugger.HandleCommand('command script add sequence '
                           '-h "Run multiple semicolon separated commands" '
                           '-f lldbToolBox.sequence')
