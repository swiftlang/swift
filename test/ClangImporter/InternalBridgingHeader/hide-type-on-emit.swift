// REQUIRES: swift_feature_AbstractStoredPropertyLayout
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// 1. Build Library.swiftmodule with the internal bridging header.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-module -module-name Library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   %t/Library.swift

// 2a. Writer's view (AST level): -print-ast of Library's source must still
//     show the real Clang-imported types on the stored properties. If the
//     substitution had mutated the AST in place, these would print as
//     @_hidden("...") instead.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -print-ast -module-name Library \
// RUN:   %t/Library.swift | %FileCheck --check-prefix=AST %s

// 2b. Writer's view (SIL level): emit Library's SILGen and confirm the stored
//     property types reference the real imported types ($sSo7Wrappera /
//     $sSo10BigWrappera). If the substitution had mutated the AST, SILGen
//     would either fail or emit HiddenType references here.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-silgen -module-name Library \
// RUN:   %t/Library.swift | %FileCheck --check-prefix=SIL %s

// 3. Reader's view: print Library from a context that imports it without the
//    bridging header. Stored properties of public types must appear as
//    @_hidden("...") with the mangled name of the original Clang type.
// RUN: %target-swift-ide-test -print-module -module-to-print Library \
// RUN:   -I %t -source-filename=%t/Client.swift \
// RUN:   | %FileCheck --check-prefix=READER %s

//--- Utility.h
typedef struct {
  int value;
} Wrapper;

typedef struct {
  double d;
} BigWrapper;

//--- Library.swift
public struct S {
  private var w: Wrapper
  public init(value: Int32) {
    w = Wrapper(value: value)
  }
}

public struct S2 {
  private var a: Wrapper
  private var b: BigWrapper
  public init() {
    self.a = Wrapper(value: 0)
    self.b = BigWrapper(d: 0)
  }
}

//--- Client.swift
import Library

// Writer's view (AST): -print-ast of the source under compilation shows the
// real imported types on the stored properties, not @_hidden placeholders.
// AST-NOT: @_hidden
// AST: public struct S {
// AST:   private var w: Wrapper
// AST: public struct S2 {
// AST:   private var a: Wrapper
// AST:   private var b: BigWrapper

// Writer's view (SIL): SILGen still references the real Clang struct types via
// their canonical mangled forms; no @_hidden appears in SIL.
// SIL-NOT: @_hidden
// SIL-DAG: $sSo7Wrappera
// SIL-DAG: $sSo10BigWrappera

// Reader's view: the printed module shows the substituted placeholders for
// the hidden stored properties, anchored to the correct containing struct.
// READER:      struct S {
// READER-NEXT:   var w: @_hidden("$sSo7Wrappera")
// READER-NEXT:   init(value: Int32)
// READER-NEXT: }
// READER:      struct S2 {
// READER-NEXT:   var a: @_hidden("$sSo7Wrappera")
// READER-NEXT:   var b: @_hidden("$sSo10BigWrappera")
// READER-NEXT:   init()
// READER-NEXT: }
