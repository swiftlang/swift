// Test that recovered members retain the availability state used by lowering.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Hidden.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -unavailable-decl-optimization=complete \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -unavailable-decl-optimization=complete \
// RUN:   -verify-type-layout VerifiableEnumWrapper \
// RUN:   -emit-ir -o /dev/null -module-name Client \
// RUN:   -I %t %t/Client.swift

//--- Hidden.h
typedef struct {
  long long first;
  long long second;
} HiddenCStruct;

//--- Library.swift
private enum EnumStorage {
  @available(*, unavailable)
  case hidden(HiddenCStruct)
  case visible(Int64)
}

public struct EnumWrapper {
  private var storage: EnumStorage
  public var visible: Int64
}

//--- Client.swift
import Library

typealias VerifiableEnumWrapper = EnumWrapper
