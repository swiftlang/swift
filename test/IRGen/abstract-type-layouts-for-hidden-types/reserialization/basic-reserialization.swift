// Test that a recovered hidden type can be serialized again when it
// contributes to the ABI of another module.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library hides a Clang record in the layout of LibraryWrapper.
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library -I %t %t/Library.swift

// Intermediate cannot import HiddenDependency. It must recover the record's
// hidden layout from Library and serialize it again as part of its own ABI.
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-module \
// RUN:   -emit-module-path %t/Intermediate.swiftmodule \
// RUN:   -module-name Intermediate -I %t %t/Intermediate.swift

// The client imports neither hidden dependency and must use Intermediate's
// reserialized layouts to lower IntermediateWrapper.
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o /dev/null -module-name Client \
// RUN:   -I %t %t/Client.swift

//--- module.modulemap
module HiddenDependency {
  header "HiddenDependency.h"
  export *
}

//--- HiddenDependency.h
typedef struct {
  long long first;
  long long second;
} HiddenCStruct;

//--- Library.swift
@_implementationOnly import HiddenDependency

public struct LibraryWrapper {
  var hiddenField: HiddenCStruct
  public var libraryValue: Int64
}

//--- Intermediate.swift
@_implementationOnly import Library

public struct IntermediateWrapper {
  var hiddenField: LibraryWrapper
  public var intermediateValue: Int64
}

//--- Client.swift
import Intermediate

public func use(_ value: IntermediateWrapper) -> IntermediateWrapper {
  value
}
