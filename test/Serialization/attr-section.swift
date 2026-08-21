
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module-path %t/a.swiftmodule -module-name a -emit-module-interface-path %t/a.swiftinterface -enable-library-evolution -swift-version 5 %s
// RUN: %llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s
// RUN: %FileCheck --check-prefix INTERFACE-CHECK %s < %t/a.swiftinterface

// BC-CHECK: <Section_DECL_ATTR

// MODULE-CHECK: @section("SOME_SECT") let Constant: Int
@section("SOME_SECT")
public let Constant = 321

// MODULE-CHECK: @section(default) func defaultSection()
@section(default)
public func defaultSection() {}

// MODULE-CHECK: @section("SOME_TEXT_SECT") func function()
@section("SOME_TEXT_SECT")
public func function() {}

public struct HasAccessors {
  public var value: Int {
    @section("SOME_TEXT_SECT") get { 0 }
    @section("SOME_MUT_SECT") set {}
  }
}

// The '@section' on each explicitly-written accessor round-trips; the sections
// of the synthesized accessors are re-inferred from these.
// INTERFACE-CHECK: public var value: Swift::Int {
// INTERFACE-CHECK-NEXT: @section("SOME_TEXT_SECT") get
// INTERFACE-CHECK-NEXT: @section("SOME_MUT_SECT") set
