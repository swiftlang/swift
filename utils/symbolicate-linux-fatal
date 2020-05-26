#!/usr/bin/env python
# symbolicate-linux-fatal - Symbolicate Linux stack traces -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Symbolicates fatalError stack traces on Linux. Takes the main binary
# and a log file containing a stack trace. Non-stacktrace lines are output
# unmodified. Stack trace elements are analyzed using reconstructed debug
# target matching the original process in where shared libs where mapped.
#
# TODOs:
# * verbose output
# * search symbols by name for the not <unavailable> ones
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import argparse
import datetime
import os
import subprocess
import sys

try:
    import lldb
except ImportError:
    from distutils import spawn
    swift_exec = spawn.find_executable('swift')
    if swift_exec is not None:
        site_packages = os.path.join(os.path.dirname(swift_exec),
                                     '../lib/python2.7/site-packages/')
        sys.path.append(site_packages)
        import lldb

lldb_target = None
known_memmap = {}


def print_with_flush(buff):
    print(buff)
    sys.stdout.flush()


def process_ldd(lddoutput):
    dyn_libs = {}
    for line in lddoutput.splitlines():
        ldd_tokens = line.split()
        if len(ldd_tokens) >= 2:
            lib = ldd_tokens[-2]
            dyn_libs[ldd_tokens[0]] = lib
            real_name = os.path.basename(os.path.realpath(lib))
            dyn_libs[real_name] = lib
    return dyn_libs


def setup_lldb_target(binary, memmap):
    global lldb_target
    if not lldb_target:
        lldb_debugger = lldb.SBDebugger.Create()
        lldb_target = lldb_debugger.CreateTarget(binary)
        module = lldb_target.GetModuleAtIndex(0)
    for dynlib_path in memmap:
        module = lldb_target.AddModule(
            dynlib_path, lldb.LLDB_ARCH_DEFAULT, None, None)
        text_section = module.FindSection(".text")
        slide = text_section.GetFileAddress() - text_section.GetFileOffset()
        lldb_target.SetModuleLoadAddress(module, memmap[dynlib_path] - slide)


def check_base_address(dynlib_path, dynlib_baseaddr, memmap):
    global known_memmap
    if dynlib_path in memmap or dynlib_path in known_memmap:
        if dynlib_path in memmap:
            existing_baseaddr = memmap[dynlib_path]
        else:
            existing_baseaddr = known_memmap[dynlib_path]
        if existing_baseaddr != dynlib_baseaddr:
            error_msg = "Mismatched base address for: {0:s}, " \
                        "had: {1:x}, now got {2:x}"
            error_msg = error_msg.format(
                dynlib_path, existing_baseaddr, dynlib_baseaddr)
            raise Exception(error_msg)


def symbolicate_one(frame_addr, frame_idx, dynlib_fname):
    global lldb_target
    so_addr = lldb_target.ResolveLoadAddress(frame_addr - 1)
    sym_ctx = so_addr.GetSymbolContext(lldb.eSymbolContextEverything)
    frame_fragment = "{0: <4d} {1:20s} 0x{2:016x}".format(
        frame_idx, dynlib_fname, frame_addr)
    symbol = sym_ctx.GetSymbol()
    if not symbol.IsValid():
        raise Exception("symbol isn't valid")

    symbol_base = symbol.GetStartAddress().GetLoadAddress(lldb_target)
    symbol_fragment = "{0:s} + {1:d}".format(
        symbol.GetName(), frame_addr - symbol_base)
    line_entry = sym_ctx.GetLineEntry()
    if line_entry.IsValid():
        line_fragment = "at {0:s}:{1:d}".format(
            line_entry.GetFileSpec().GetFilename(), line_entry.GetLine())
    else:
        line_fragment = ""

    return "{0:s}    {1:s} {2:s}".format(
           frame_fragment, symbol_fragment, line_fragment)


def get_processed_stack(binary, dyn_libs, stack):
    global lldb_target
    global known_memmap
    processed_stack = []
    if len(stack) == 0:
        return processed_stack
    memmap = {}
    full_stack = []
    for line in stack:
        stack_tokens = line.split()
        dynlib_fname = stack_tokens[1]
        if dynlib_fname in dyn_libs:
            dynlib_path = dyn_libs[dynlib_fname]
        elif dynlib_fname in binary:
            dynlib_path = binary
        else:
            dynlib_path = None

        try:
            framePC = int(stack_tokens[2], 16)
            symbol_offset = int(stack_tokens[-1], 10)
        except Exception:
            full_stack.append({"line": line, "framePC": 0, "dynlib_fname": ""})
            continue

        if "<unavailable>" in stack_tokens[3]:
            dynlib_baseaddr = framePC - symbol_offset
            check_base_address(dynlib_path, dynlib_baseaddr, memmap)
            known_memmap[dynlib_path] = dynlib_baseaddr
            memmap[dynlib_path] = dynlib_baseaddr
        else:
            framePC = framePC + symbol_offset
        full_stack.append(
            {"line": line, "framePC": framePC, "dynlib_fname": dynlib_fname})

    setup_lldb_target(binary, memmap)

    for frame_idx, frame in enumerate(full_stack):
        frame_addr = frame["framePC"]
        dynlib_fname = frame["dynlib_fname"]
        try:
            sym_line = symbolicate_one(frame_addr, frame_idx, dynlib_fname)
            processed_stack.append(sym_line)
        except Exception:
            processed_stack.append(frame["line"].rstrip())

    return processed_stack


def is_fatal_error(line):
    return line.startswith("Fatal error:")


def is_stack_trace_header(line):
    return line.startswith("Current stack trace:")


def should_print_previous_line(current_line, previous_line):
    return is_fatal_error(previous_line) and \
        not is_stack_trace_header(current_line)


def should_print_current_line(current_line, previous_line):
    return (not is_fatal_error(current_line) and
            not is_stack_trace_header(current_line)) or \
           (is_stack_trace_header(current_line) and
            not is_fatal_error(previous_line))


def fatal_error_with_stack_trace_found(current_line, previous_line):
    return is_stack_trace_header(current_line) and \
        is_fatal_error(previous_line)


def print_stack(fatal_error_header,
                fatal_error_stack_trace_header,
                fatal_log_format,
                processed_stack):
    if not fatal_error_header:
        for line in processed_stack:
            print_with_flush(line)
    else:
        # fatal error with a stack trace
        stack_str = fatal_error_header + fatal_error_stack_trace_header + \
            '\n'.join(processed_stack)
        formatted_output = fatal_log_format

        if "%t" in formatted_output:
            current_time = datetime.datetime.now()
            time_in_iso_format = \
                current_time.strftime('%Y-%m-%dT%H:%M:%S,%f%z')
            formatted_output = \
                formatted_output.replace("%t", time_in_iso_format)
        if "%m" in formatted_output:
            formatted_output = formatted_output.replace("%m", stack_str)

        print_with_flush(formatted_output)


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""Symbolicates stack traces in Linux log files.""")
    parser.add_argument(
        "binary", help="Executable which produced the log file")
    parser.add_argument(
        "log", nargs='?', type=argparse.FileType("rU"), default="-",
        help="Log file for symbolication. Defaults to stdin.")
    parser.add_argument(
        "--fatal-log-format", default="%m",
        help="Format for logging fatal errors. Variable %%t will be "
             "replaced with current time in ISO 8601 format, variable "
             "%%m will be replaced with the error message with a full "
             "stack trace.")
    args = parser.parse_args()

    binary = args.binary
    fatal_log_format = args.fatal_log_format

    lddoutput = subprocess.check_output(
        ['ldd', binary], stderr=subprocess.STDOUT)
    dyn_libs = process_ldd(lddoutput)

    instack = False
    previous_line = ""
    stackidx = 0
    stack = []
    fatal_error_header = ""
    fatal_error_stack_trace_header = ""

    while True:
        current_line = args.log.readline()
        if not current_line:
            break
        if instack and current_line.startswith(str(stackidx)):
            stack.append(current_line)
            stackidx = stackidx + 1
        else:
            processed_stack = get_processed_stack(binary, dyn_libs, stack)
            print_stack(fatal_error_header,
                        fatal_error_stack_trace_header,
                        fatal_log_format,
                        processed_stack)

            instack = False
            stackidx = 0
            stack = []
            fatal_error_header = ""
            fatal_error_stack_trace_header = ""

            if is_stack_trace_header(current_line):
                instack = True

            if should_print_previous_line(current_line, previous_line):
                print_with_flush(previous_line.rstrip())

            if should_print_current_line(current_line, previous_line):
                print_with_flush(current_line.rstrip())

            if fatal_error_with_stack_trace_found(current_line, previous_line):
                fatal_error_header = previous_line
                fatal_error_stack_trace_header = current_line

        previous_line = current_line
    if is_fatal_error(previous_line):
        print_with_flush(previous_line.rstrip())
    processed_stack = get_processed_stack(binary, dyn_libs, stack)
    print_stack(fatal_error_header,
                fatal_error_stack_trace_header,
                fatal_log_format,
                processed_stack)


if __name__ == '__main__':
    main()
