// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -g -o %t/DwarfReaderLookups

// RUN: %yaml2obj %S/Inputs/DwarfCallSiteAttrs.yaml -o %t/callsiteattrs.o
// RUN: %target-run %t/DwarfReaderLookups %t/callsiteattrs.o 0x1018 0x1038 0x1058 | %FileCheck %s --check-prefix=ATTRS

// RUN: %yaml2obj %S/Inputs/DwarfAddrxLowPC.yaml -o %t/addrxlowpc.o
// RUN: %target-run %t/DwarfReaderLookups %t/addrxlowpc.o 0x4018 0x0018 | %FileCheck %s --check-prefix=ADDRX

// RUN: %yaml2obj %S/Inputs/DwarfNames.yaml -o %t/names.o
// RUN: %target-run %t/DwarfReaderLookups %t/names.o 0x1008 0x2008 0x3018 0x3038 | %FileCheck %s --check-prefix=NAMES

// RUN: %yaml2obj %S/Inputs/DwarfNesting.yaml -o %t/nesting.o
// RUN: %target-run %t/DwarfReaderLookups %t/nesting.o 0x1050 0x1015 0x1065 0x1085 0x1086 | %FileCheck %s --check-prefix=NESTING

// RUN: %yaml2obj %S/Inputs/DwarfArangesLine.yaml -o %t/arangesline.o
// RUN: %target-run %t/DwarfReaderLookups %t/arangesline.o 0x1008 0x1018 0x1020 0x2000 | %FileCheck %s --check-prefix=ARANGES

// RUN: %yaml2obj %S/Inputs/DwarfNoRanges.yaml -o %t/noranges.o
// RUN: %target-run %t/DwarfReaderLookups %t/noranges.o 0x1008 0x1018 0x1020 | %FileCheck %s --check-prefix=LINES

// RUN: %yaml2obj %S/Inputs/DwarfLineOnly.yaml -o %t/lineonly.o
// RUN: %target-run %t/DwarfReaderLookups %t/lineonly.o 0x1008 0x1018 0x1020 | %FileCheck %s --check-prefix=LINES

// RUN: %yaml2obj %S/Inputs/DwarfOverlap.yaml -o %t/overlap.o
// RUN: %target-run %t/DwarfReaderLookups %t/overlap.o 0x1008 0x1018 0x1028 0x1030 | %FileCheck %s --check-prefix=OVERLAP

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Regression tests for the DwarfReader's handling of function and inline
// frame lookups by address.  Each of the Inputs/Dwarf*.yaml files is a
// hand-crafted object that exercises one or more problems that we used to
// have; see the comments in those files for the layout of the DWARF.

@_spi(DwarfTest) import Runtime
#if canImport(Darwin)
import Darwin
#elseif canImport(SwiftWASILibc)
import SwiftWASILibc
#elseif canImport(ucrt)
import ucrt
#elseif canImport(SwiftGlibc)
import SwiftGlibc
#endif

@main
struct DwarfReaderLookups {
  static func main() {
    if CommandLine.argc < 3 {
      print("usage: DwarfReaderLookups <path-to-binary> <address>...")
      return
    }

    let addresses = CommandLine.arguments[2...].map {
      UInt64($0.dropFirst(2), radix: 16)!
    }

    // Compilers leave out DW_AT_call_line and DW_AT_call_column when they are
    // zero, and DW_AT_call_file when there isn't one, and a DWARF 5 compiler
    // might use DW_FORM_implicit_const for any of them.  We shouldn't ignore
    // the inlined subroutine in any of those cases.

    // ATTRS: address 0x1018:
    // ATTRS-NEXT: function: outer
    // ATTRS-NEXT: inline: callee <unknown>:5:0
    // ATTRS-NEXT: address 0x1038:
    // ATTRS-NEXT: function: outer
    // ATTRS-NEXT: inline: callee <unknown>:0:0
    // ATTRS-NEXT: address 0x1058:
    // ATTRS-NEXT: function: outer
    // ATTRS-NEXT: inline: callee <unknown>:7:9

    // If DW_AT_low_pc is a DW_FORM_addrx, it needs to be decoded before we
    // use it as the base address for range lists.

    // ADDRX: address 0x4018:
    // ADDRX-NEXT: function: outer
    // ADDRX-NEXT: inline: callee <unknown>:10:1
    // ADDRX-NEXT: address 0x18:
    // ADDRX-NEXT: function: <none>
    // ADDRX-NOT: inline:

    // Attributes on a DIE override those of its DW_AT_specification (so we
    // get C2, not C4), and a DIE with a DW_AT_abstract_origin gets its name
    // from there.

    // NAMES: address 0x1008:
    // NAMES-NEXT: function: _ZN1AC2Ev
    // NAMES-NEXT: address 0x2008:
    // NAMES-NEXT: function: abstract_fn
    // NAMES-NEXT: address 0x3018:
    // NAMES-NEXT: function: caller
    // NAMES-NEXT: inline: _ZN1AC2Ev <unknown>:3:4
    // NAMES-NEXT: address 0x3038:
    // NAMES-NEXT: function: caller
    // NAMES-NEXT: inline: abstract_fn <unknown>:5:6

    // Inlined subroutines nest.  We should find all of those that contain an
    // address, even if there are others that start between them, and we
    // should return them innermost first, even if the outer ones have more
    // than one range.

    // NESTING: address 0x1050:
    // NESTING-NEXT: function: outer
    // NESTING-NEXT: inline: calleeX <unknown>:1:1
    // NESTING-NEXT: address 0x1015:
    // NESTING-NEXT: function: outer
    // NESTING-NEXT: inline: calleeY <unknown>:2:1
    // NESTING-NEXT: inline: calleeX <unknown>:1:1
    // NESTING-NEXT: address 0x1065:
    // NESTING-NEXT: function: outer
    // NESTING-NEXT: inline: calleeZ <unknown>:3:1
    // NESTING-NEXT: inline: calleeX <unknown>:1:1
    // NESTING-NEXT: address 0x1085:
    // NESTING-NEXT: function: outer
    // NESTING-NEXT: inline: calleeW <unknown>:4:1
    // NESTING-NEXT: inline: calleeZ <unknown>:3:1
    // NESTING-NEXT: inline: calleeX <unknown>:1:1
    // NESTING-NEXT: address 0x1086:
    // NESTING-NEXT: function: outer
    // NESTING-NEXT: inline: calleeW <unknown>:4:1
    // NESTING-NEXT: inline: calleeZ <unknown>:3:1
    // NESTING-NEXT: inline: calleeX <unknown>:1:1

    // Units that we find using .debug_aranges haven't been resolved, so we
    // need to do that before we can find their line number programs.  The
    // address that we look up first has to be one that we haven't looked up a
    // function for, because that would resolve the unit too.

    // ARANGES: address 0x1008:
    // ARANGES-NEXT: location: /src/file.c:42:0
    // ARANGES-NEXT: function: <none>
    // ARANGES-NEXT: address 0x1018:
    // ARANGES-NEXT: location: /src/file.c:50:0
    // ARANGES-NEXT: function: <none>
    // ARANGES-NEXT: address 0x1020:
    // ARANGES-NEXT: function: <none>
    // ARANGES-NEXT: address 0x2000:
    // ARANGES-NEXT: function: <none>

    // A unit with no address ranges, or no DIEs at all, can still be found
    // using its line number program, which is all that we get from
    // -gline-tables-only on some producers.

    // LINES: address 0x1008:
    // LINES-NEXT: location: /src/file.c:42:0
    // LINES-NEXT: function: <none>
    // LINES-NEXT: address 0x1018:
    // LINES-NEXT: location: /src/file.c:50:0
    // LINES-NEXT: function: <none>
    // LINES-NEXT: address 0x1020:
    // LINES-NEXT: function: <none>

    // Units whose ranges overlap are all candidates for an address.

    // OVERLAP: address 0x1008:
    // OVERLAP-NEXT: location: /src/file.c:10:0
    // OVERLAP-NEXT: function: <none>
    // OVERLAP-NEXT: address 0x1018:
    // OVERLAP-NEXT: location: /src/file.c:10:0
    // OVERLAP-NEXT: function: <none>
    // OVERLAP-NEXT: address 0x1028:
    // OVERLAP-NEXT: location: /src/file.c:20:0
    // OVERLAP-NEXT: function: <none>
    // OVERLAP-NEXT: address 0x1030:
    // OVERLAP-NEXT: function: <none>

    if !testDwarfLookupsFor(path: CommandLine.arguments[1],
                            addresses: addresses) {
      exit(1)
    }
  }
}
