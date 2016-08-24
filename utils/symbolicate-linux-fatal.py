#!/usr/bin/env python

# TODOs:
# * verbose output
# * search symbols by name for the not <unavailable> ones

import sys
import lldb
from subprocess import Popen, PIPE

dynlibs = {}
def process_ldd(lddoutput):
    global dynlibs
    for line in lddoutput.splitlines():
        ldd_tokens = line.split()
        if len(ldd_tokens) >= 3:
            dynlibs[ldd_tokens[0]] = ldd_tokens[2]

def create_lldb_target(memmap):
    lldb_target = lldb_debugger.CreateTargetWithFileAndArch(binary, lldb.LLDB_ARCH_DEFAULT)
    module = lldb_target.GetModuleAtIndex(0)
    # lldb seems to treat main binary differently, slide offset must be zero
    lldb_target.SetModuleLoadAddress(module, 0)
    for dynlib_path in memmap:
        if not binary in dynlib_path:
            module = lldb_target.AddModule (dynlib_path, lldb.LLDB_ARCH_DEFAULT, None, None)
            lldb_target.SetModuleLoadAddress(module, memmap[dynlib_path])
    return lldb_target

def process_stack(stack):
    global binary
    if len(stack) == 0:
        return
    memmap = {}
    full_stack = []
    for line in stack:
        stack_tokens = line.split()
        dynlib_fname = stack_tokens[1]
        if dynlib_fname in dynlibs:
            dynlib_path = dynlibs[dynlib_fname]
        else:
            if dynlib_fname in binary:
                dynlib_path = binary
            else:
                dynlib_path = None

        if "<unavailable>" in stack_tokens[3]:
            framePC = int(stack_tokens[2], 16)
            symbol_offset = int(stack_tokens[-1], 10)
            dynlib_baseaddr = framePC - symbol_offset
            if dynlib_path in memmap:
                if memmap[dynlib_path] != dynlib_baseaddr:
                    print "Mismatched base address for: {0:s}, had: {1:x}, now got {2:x}".format(dynlib_path, memmap[dynlib_path], dynlib_baseaddr)
                    sys.exit(1)
            else:
                memmap[dynlib_path] = dynlib_baseaddr
        else:
            framePC = int(stack_tokens[2], 16) + int(stack_tokens[-1], 10)
        full_stack.append({"line": line, "framePC": framePC})

    lldb_target = create_lldb_target(memmap)
    frame_idx = 0
    for frame in full_stack:
        use_orig_line = True
        frame_addr = frame["framePC"]
        so_addr = lldb_target.ResolveLoadAddress(frame_addr-1)
        sym_ctx = so_addr.GetSymbolContext (lldb.eSymbolContextEverything)
        frame_fragment = "{0: <4d} 0x{1:016x}".format(frame_idx, frame_addr)
        symbol = sym_ctx.GetSymbol()
        if symbol.IsValid():
            symbol_fragment = "{0:s} + {1:d}".format(symbol.GetName(), frame_addr - symbol.GetStartAddress().GetLoadAddress(lldb_target))
            use_orig_line = False
        else:
            symbol_fragment = "<unavailable>"
        line_entry = sym_ctx.GetLineEntry()
        if line_entry.IsValid():
            line_fragment =  "at {0:s}:{1:d}".format(line_entry.GetFileSpec().GetFilename(), line_entry.GetLine())
        else:
            line_fragment = ""
        if use_orig_line:
            print frame["line"].rstrip()
        else:
            print "{0:s}    {1:s} {2:s}".format(frame_fragment, symbol_fragment, line_fragment)
        frame_idx = frame_idx + 1

if len(sys.argv) != 3:
    print("usage: {0:s} binary logfile".format(sys.argv[0]))
    sys.exit(1)

binary = sys.argv[1]
logfile = sys.argv[2]

lddprocess = Popen(["ldd", binary], stdout=PIPE)
(lddoutput, err) = lddprocess.communicate()
exit_code = lddprocess.wait()
if exit_code != 0:
    print("ldd failed with {0:d}".format(exit_code))
    sys.exit(1)
process_ldd(lddoutput)

lldb_debugger = lldb.SBDebugger.Create()

instack = False
stackidx = 0
stack = []
with open(logfile, 'rU') as f:
    for line in f:
        if instack and line.startswith(str(stackidx)):
            stack.append(line)
            stackidx = stackidx + 1
        else:
            instack = False
            stackidx = 0
            process_stack(stack)
            stack = []
            print line.rstrip()
        if line.startswith("Current stack trace:"):
            instack = True
process_stack(stack)
