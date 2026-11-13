// Test recovery of lazy backing storage whose layout depends on a hidden
// Clang type.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// XFAIL: *

// FIXME: The synthesized lazy backing declaration does not retain the import
// access context needed to classify its hidden bridging-header field type.

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
public struct PublicValue {
  private lazy var hidden = HiddenCStruct(first: 0, second: 0)
  public var visible: Int64

  public init() {
    visible = 0
  }
}

//--- Client.swift
import Library

public func passThrough(_ value: PublicValue) -> PublicValue {
  value
}
