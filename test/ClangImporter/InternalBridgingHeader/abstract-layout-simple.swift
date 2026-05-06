// Test that a public struct can have a private stored property of a C type
// imported via an internal bridging header when AbstractStoredPropertyLayout is enabled.

// REQUIRES: swift_feature_AbstractStoredPropertyLayout

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature AbstractStoredPropertyLayout \
// RUN:   -emit-module -module-name Library \
// RUN:   -o %t/Library.swiftmodule \
// RUN:   %t/Library.swift

//--- Utility.h

typedef struct { int value; } Wrapper;

//--- Library.swift

public struct S {
  private var w: Wrapper

  public init(value: Int32) {
    self.w = Wrapper(value: value)
  }

  public var storedValue: Int32 { w.value }
}
