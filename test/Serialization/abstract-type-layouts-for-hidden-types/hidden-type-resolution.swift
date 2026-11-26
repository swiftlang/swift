// Test that hidden declarations referenced by a stored closure signature get
// fallback representations even though they do not contribute to its layout.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -Rhidden-type-layout-serialization -verify \
// RUN:   -internal-import-bridging-header %t/HiddenTypes.h \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift
// RUN: %llvm-bcanalyzer -dump %t/Library.swiftmodule \
// RUN:   | %FileCheck %s --check-prefix TYPE-RESOLUTION-FALLBACK

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o /dev/null -module-name Client -I %t %t/Client.swift

// TYPE-RESOLUTION-FALLBACK: <HIDDEN_TYPE_LAYOUT_INFO
// TYPE-RESOLUTION-FALLBACK: <HIDDEN_TYPE_FALLBACK_TABLE

//--- HiddenTypes.h
typedef struct {
  int value;
} HiddenCStruct;

//--- Library.swift
public struct Wrapper {
  // expected-remark@+1 {{serializing abstract layout for hidden type 'HiddenCStruct' because it was imported through '-internal-import-bridging-header' and is referenced by a type in the ABI-exposed layout of struct 'Wrapper' through property 'callback'}}
  private var callback: (HiddenCStruct) -> Void

  public init() {
    callback = { _ in }
  }
}

//--- Client.swift
import Library

public func passThrough(_ value: Wrapper) -> Wrapper {
  value
}
