// Test that a hidden Clang record XREF resolves to the visible type when the
// client imports the Clang module that defines it.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library -I %t %t/Library.swift

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   -I %t %t/Client.swift
// RUN: %FileCheck %s < %t/Client.ll

//--- module.modulemap
module HiddenDependency {
  header "HiddenDependency.h"
  export *
}

//--- HiddenDependency.h
typedef struct {
  long long first;
  long long second;
} RecoveredCStruct;

//--- Library.swift
@_implementationOnly import HiddenDependency

public struct Wrapper {
  var hiddenField: RecoveredCStruct
  public var visibleField: Int64
}

//--- Client.swift
import HiddenDependency
import Library

public func use(_ value: Wrapper) -> Wrapper {
  value
}

// The hidden XREF resolves because HiddenDependency is visible to the client.
// The wrapper therefore contains the real imported Clang type rather than a
// HiddenType reconstructed from its fallback layout.
// CHECK-DAG: %TSo16RecoveredCStructa = type <{ %Ts5Int64V, %Ts5Int64V }>
// CHECK-DAG: %T7Library7WrapperV = type <{ %TSo16RecoveredCStructa, %Ts5Int64V }>
