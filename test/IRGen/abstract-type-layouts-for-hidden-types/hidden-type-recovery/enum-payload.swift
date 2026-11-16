// Test that a missing enum case retains the layout of its hidden payload.

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
// RUN:   -verify-type-layout VerifiableWrapper \
// RUN:   -emit-ir -o /dev/null -module-name Client \
// RUN:   -I %t %t/Client.swift

//--- Hidden.h
typedef struct {
  long long first;
  long long second;
} HiddenCStruct;

//--- Library.swift
private enum Storage {
  case hidden(HiddenCStruct)
  case visible(Int64)
}

public struct Wrapper {
  private var storage: Storage
  public var visible: Int64

  public init(_ value: Int64) {
    storage = .visible(value)
    visible = value
  }
}

//--- Client.swift
import Library

typealias VerifiableWrapper = Wrapper

public func passThrough(_ value: Wrapper) -> Wrapper {
  value
}
