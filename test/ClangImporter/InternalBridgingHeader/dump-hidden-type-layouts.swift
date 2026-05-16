// Test that -dump-hidden-type-layouts prints hidden-type layouts both for
// the locally-compiled module and for any imported module that carries a
// HIDDEN_TYPE_LAYOUTS_BLOCK in its .swiftmodule.

// REQUIRES: swift_feature_AbstractStoredPropertyLayout
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// First, build Library.swiftmodule. This embeds the HIDDEN_TYPE_LAYOUTS_BLOCK
// with one entry per hidden C type referenced by Library's stored properties.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-module -module-name Library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   %t/Library.swift

// Local case: dump Library's hidden-type layouts during its own compilation.
// The entries flow into the ModuleDecl through Sema's exportability check
// (encapsulatedAsHiddenStoredProperty) and are visible to the dump path.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -typecheck -module-name Library \
// RUN:   -dump-hidden-type-layouts \
// RUN:   %t/Library.swift | %FileCheck --check-prefix=LOCAL %s

// Imported case: Client imports Library but has no bridging header itself.
// Library's hidden-type layouts must round-trip from the .swiftmodule's
// HIDDEN_TYPE_LAYOUTS_BLOCK into the loaded ModuleDecl, where the dump
// path (which iterates all loaded modules) picks them up.
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -typecheck -module-name Client \
// RUN:   -I %t \
// RUN:   -dump-hidden-type-layouts \
// RUN:   %t/Client.swift | %FileCheck --check-prefix=IMPORTED %s

//--- Utility.h
typedef struct {
  int value;
} Wrapper;

typedef struct {
  long a;
  long b;
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
    self.b = BigWrapper(a: 0, b: 0)
  }
}

// LOCAL: Module: Library
// LOCAL-NEXT: $sSo10BigWrappera: size=16, alignment=8, stride=16, bitwiseCopyable=true, opaque=false
// LOCAL-NEXT: $sSo7Wrappera: size=4, alignment=4, stride=4, bitwiseCopyable=true, opaque=false

//--- Client.swift
import Library

// IMPORTED: Module: Library
// IMPORTED-NEXT: $sSo10BigWrappera: size=16, alignment=8, stride=16, bitwiseCopyable=true, opaque=false
// IMPORTED-NEXT: $sSo7Wrappera: size=4, alignment=4, stride=4, bitwiseCopyable=true, opaque=false
