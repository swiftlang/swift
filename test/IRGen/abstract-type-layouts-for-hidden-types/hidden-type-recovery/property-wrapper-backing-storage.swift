// Test that recovery preserves property-wrapper backing storage whose layout
// depends on a hidden Clang type.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Hidden.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o /dev/null -module-name Client \
// RUN:   -I %t %t/Client.swift

//--- Hidden.h
typedef struct {
  long long first;
  long long second;
} HiddenCStruct;

//--- Library.swift
@propertyWrapper
private struct Wrapper {
  var wrappedValue: HiddenCStruct
}

public struct PublicValue {
  @Wrapper private var hidden: HiddenCStruct
  public var visible: Int64

  public init() {
    hidden = HiddenCStruct(first: 0, second: 0)
    visible = 0
  }
}

//--- Client.swift
import Library

public func passThrough(_ value: PublicValue) -> PublicValue {
  value
}
