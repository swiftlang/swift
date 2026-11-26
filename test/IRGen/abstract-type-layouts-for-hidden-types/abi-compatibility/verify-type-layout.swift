// Test the runtime type-layout verifier against a stored property recovered
// from its serialized abstract layout.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-object \
// RUN:   -emit-module -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift \
// RUN:   -o %t/Library.o

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -verify-type-layout VerifiableWrapper \
// RUN:   -emit-ir -o - -module-name Client \
// RUN:   -I %t %t/Client.swift | %FileCheck %s

// RUN: %target-build-swift \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -Xfrontend -verify-type-layout \
// RUN:   -Xfrontend VerifiableWrapper \
// RUN:   -I %t -o %t/client %t/Client.swift %t/Library.o
// RUN: %target-codesign %t/client
// RUN: %target-run %t/client

//--- Utility.h
typedef struct {
  int count;
  double value;
} HiddenCStruct;

//--- Library.swift
public struct VerifiedHiddenCStructWrapper {
  var hiddenField: HiddenCStruct
  public var visibleField: Int64

  public init(count: Int32, value: Double, visibleField: Int64) {
    hiddenField = HiddenCStruct(count: count, value: value)
    self.visibleField = visibleField
  }
}

//--- Client.swift
import Library

typealias VerifiableWrapper = VerifiedHiddenCStructWrapper

let value = VerifiedHiddenCStructWrapper(
    count: 42, value: 2.5, visibleField: 100)
assert(value.visibleField == 100)

// CHECK: c"offset of struct field hiddenField\00"
