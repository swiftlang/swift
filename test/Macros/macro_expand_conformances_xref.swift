// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Check for errors first
// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %s -I %t -disable-availability-checking

// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) %s -I %t -disable-availability-checking -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt


protocol P1 {}
protocol P2 {}

@attached(extension, conformances: P1, P2)
@attached(member, conformances: P1, P2, names: named(conformances))
macro ListConformances() = #externalMacro(module: "MacroDefinition", type: "ListConformancesMacro")


// CHECK-DUMP: [ "Root": [ "P1", "P2" ] ]
// CHECK-DUMP: extension Root: P1
// CHECK-DUMP: extension Root: P2
@ListConformances
class Root {
// CHECK-DUMP: extension OtherRoot: P1
// CHECK-DUMP: extension OtherRoot: P2
  var other: OtherRoot?
}

// CHECK-DUMP: [ "P1Root": [ "P2" ] ]
// CHECK-DUMP-NOT: extension P1Root: P1
// CHECK-DUMP: extension P1Root: P2
@ListConformances
class P1Root: P1 { }

// CHECK-DUMP: [ "OtherRoot": [ "P1", "P2" ] ]
@ListConformances
class OtherRoot {
// CHECK-DUMP-NOT: extension OtherP1Root: P1
// CHECK-DUMP: extension OtherP1Root: P2
  var other: OtherP1Root?
}

// CHECK-DUMP: [ "OtherP1Root": [ "P2" ] ]
@ListConformances
class OtherP1Root: P1 { }

