// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Multi-threaded WMO: emit one LLVM module per source file. The macro-attached
// type (and thus the synthesized nested `Cases` type) lives in a file that is
// *not* the primary/first file. The nested type's nominal type context
// descriptor must be emitted into the same output module as the conformance
// records that reference it directly; otherwise the descriptor is emitted into
// the primary module while its references live in the parent file's module,
// producing an invalid cross-object subtraction relocation on x86_64.
// rdar://185645710

// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name Repro -wmo -num-threads 2 \
// RUN:   -load-plugin-library %t/%target-library-name(MacroDefinition) \
// RUN:   %t/a_first.swift %t/def.swift \
// RUN:   -o %t/a_first.ll -o %t/def.ll
// RUN: %FileCheck %s < %t/def.ll

//--- a_first.swift
// This file is first, so it is the "primary" IRGen output module. The nested
// type's descriptor must NOT be pulled into here.
public func marker() {}

//--- def.swift
public protocol MyProtocol {}

@attached(extension, conformances: MyProtocol, names: arbitrary)
macro NestedCases() = #externalMacro(module: "MacroDefinition", type: "NestedCasesViaExtensionMacro")

@NestedCases
public enum MyUnion: Sendable {
  case a(Int)
  case b(String)
}

// The nested type's context descriptor is defined here (co-located with the
// conformance descriptors that reference it), not left as an external reference
// to a definition in the primary module.
// CHECK: @"$s5Repro7MyUnionO5CasesOMn" ={{.*}}constant
// CHECK-NOT: @"$s5Repro7MyUnionO5CasesOMn" = external
