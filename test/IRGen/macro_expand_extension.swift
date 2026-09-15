// REQUIRES: swift_swift_parser
// REQUIRES: executable_test

// dynamic library with wasm is not supported yet
// UNSUPPORTED: CPU=wasm32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t/src
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Make sure we emit extension macros correctly for both single-threaded and multi-threaded WMO, as well
// as regular single primary mode.

// RUN: %target-swift-frontend -emit-ir -module-name main %t/src/main.swift %t/src/a.swift %t/src/b.swift -load-plugin-library %t/%target-library-name(MacroDefinition) | %FileCheck --check-prefix CHECK-ALL %s

// RUN: %target-swift-frontend -emit-ir -module-name main %t/src/main.swift -o %t/main.ll %t/src/a.swift -o %t/a.ll %t/src/b.swift -o %t/b.ll -num-threads 3 -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %FileCheck -check-prefix CHECK-A %s < %t/a.ll
// RUN: %FileCheck -check-prefix NOT-IN-A %s < %t/a.ll
// RUN: %FileCheck -check-prefix CHECK-B %s < %t/b.ll
// RUN: %FileCheck -check-prefix NOT-IN-B %s < %t/b.ll

// RUN: %target-swift-frontend -emit-ir -module-name main %t/src/main.swift -primary-file %t/src/a.swift -o %t/a1.ll %t/src/b.swift -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %FileCheck -check-prefix CHECK-A %s < %t/a1.ll

// RUN: %target-swift-frontend -emit-ir -module-name main %t/src/main.swift %t/src/a.swift -primary-file %t/src/b.swift -o %t/b1.ll -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %FileCheck -check-prefix CHECK-B %s < %t/b1.ll

// RUN: %target-swift-frontend -c -module-name main %t/src/main.swift -o %t/main.o %t/src/a.swift -o %t/a.o %t/src/b.swift -o %t/b.o -num-threads 3 -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %target-build-swift %t/main.o %t/a.o %t/b.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck --check-prefix CHECK-OUT %s

//--- a.swift
public protocol MyProtocol {}

// Adds `extension <Type>: MyProtocol { func macroAddedFunc(); struct MacroAddedNested {} }`.
@attached(extension, conformances: MyProtocol,
          names: named(macroAddedFunc), named(MacroAddedNested))
macro AddMembersAndConformance() = #externalMacro(module: "MacroDefinition", type: "ExtensionWithMembersMacro")

// Adds `extension <Type> { @AddMyProtocol struct Nested {} }`.
@attached(extension, names: named(Nested))
macro AddNested() = #externalMacro(module: "MacroDefinition", type: "NestedConformingExtensionMacro")

// Adds `extension <Type>: MyProtocol {}`
@attached(extension, conformances: MyProtocol)
macro AddMyProtocol() = #externalMacro(module: "MacroDefinition", type: "ConformanceViaExtensionMacro")

@AddMembersAndConformance
public struct TopLevelA {
  public init() {}
}

public struct OuterA {
  @AddMembersAndConformance
  public struct InnerA {
    public init() {}
  }
}

@AddNested
public struct ChainedA {
  public init() {}
}

// Conformance descriptors and metadata should be in the same translation unit.
// CHECK-A-DAG: @"$s4main9TopLevelAVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-A-DAG: @"$s4main6OuterAV6InnerAVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-A-DAG: @"$s4main8ChainedAV6NestedVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-A-DAG: @"$s4main9TopLevelAV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-A-DAG: @"$s4main6OuterAV6InnerAV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-A-DAG: @"$s4main9TopLevelAVMn" = {{(dllexport )?}}{{(protected )?}}constant

// No non-`external` definitions from b.swift
// NOT-IN-A-NOT: $s4main9TopLevelBV{{.*}} = {{[^e]}}
// NOT-IN-A-NOT: $s4main6OuterBV{{.*}} = {{[^e]}}
// NOT-IN-A-NOT: $s4main8ChainedBV{{.*}} = {{[^e]}}

// CHECK-ALL-DAG: @"$s4main9TopLevelAVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main6OuterAV6InnerAVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main8ChainedAV6NestedVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main9TopLevelAV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main6OuterAV6InnerAV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main9TopLevelAVMn" = {{(dllexport )?}}{{(protected )?}}constant

//--- b.swift

@AddMembersAndConformance
public struct TopLevelB {
  public init() {}
}

public struct OuterB {
  @AddMembersAndConformance
  public struct InnerB {
    public init() {}
  }
}

@AddNested
public struct ChainedB {
  public init() {}
}

// CHECK-B-DAG: @"$s4main9TopLevelBVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-B-DAG: @"$s4main6OuterBV6InnerBVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-B-DAG: @"$s4main8ChainedBV6NestedVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-B-DAG: @"$s4main9TopLevelBV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-B-DAG: @"$s4main6OuterBV6InnerBV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-B-DAG: @"$s4main9TopLevelBVMn" = {{(dllexport )?}}{{(protected )?}}constant

// No non-`external` definitions from a.swift
// NOT-IN-B-NOT: $s4main9TopLevelAV{{.*}} = {{[^e]}}
// NOT-IN-B-NOT: $s4main6OuterAV{{.*}} = {{[^e]}}
// NOT-IN-B-NOT: $s4main8ChainedAV{{.*}} = {{[^e]}}

// CHECK-ALL-DAG: @"$s4main9TopLevelBVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main6OuterBV6InnerBVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main8ChainedBV6NestedVAA10MyProtocolAAMc" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main9TopLevelBV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main6OuterBV6InnerBV16MacroAddedNestedVMn" = {{(dllexport )?}}{{(protected )?}}constant
// CHECK-ALL-DAG: @"$s4main9TopLevelBVMn" = {{(dllexport )?}}{{(protected )?}}constant

//--- main.swift

@_optimize(none)
func conformsToMyProtocol(_ value: Any) -> Bool { value is MyProtocol }

TopLevelA().macroAddedFunc()
TopLevelB().macroAddedFunc()
OuterA.InnerA().macroAddedFunc()
OuterB.InnerB().macroAddedFunc()

print(conformsToMyProtocol(TopLevelA()))
// CHECK-OUT: true
print(conformsToMyProtocol(TopLevelB()))
// CHECK-OUT: true
print(conformsToMyProtocol(OuterA.InnerA()))
// CHECK-OUT: true
print(conformsToMyProtocol(OuterB.InnerB()))
// CHECK-OUT: true
print(conformsToMyProtocol(ChainedA.Nested()))
// CHECK-OUT: true
print(conformsToMyProtocol(ChainedB.Nested()))
// CHECK-OUT: true

// Types declared inside a macro-expanded extension need their metadata emitted.
print(TopLevelA.MacroAddedNested.self)
// CHECK-OUT: MacroAddedNested
print(TopLevelB.MacroAddedNested.self)
// CHECK-OUT: MacroAddedNested
print(OuterA.InnerA.MacroAddedNested.self)
// CHECK-OUT: MacroAddedNested
print(OuterB.InnerB.MacroAddedNested.self)
// CHECK-OUT: MacroAddedNested
