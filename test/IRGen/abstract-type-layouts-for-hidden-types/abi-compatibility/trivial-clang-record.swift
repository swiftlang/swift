// Test that a trivial Clang record has the same ABI when lowered from its
// visible definition and from its serialized hidden representation.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the library and its module with the hidden Clang record visible.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -parse-as-library -emit-object \
// RUN:   -emit-module -emit-module-path %t/Library.swiftmodule \
// RUN:   -module-name Library %t/Library.swift \
// RUN:   -o %t/Library.o

// Emit the library IR while the Clang definition is visible.
// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Library.ll -module-name Library -parse-as-library \
// RUN:   %t/Library.swift
// RUN: %FileCheck --check-prefix=LIBRARY-SIDE %s < %t/Library.ll

// Emit the client IR without access to the Clang definition.
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Client.ll -module-name Client \
// RUN:   -I %t %t/Client.swift
// RUN: %FileCheck --check-prefix=CLIENT-SIDE %s < %t/Client.ll

// Build and run the client without exposing the bridging header.
// RUN: %target-build-swift \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -I %t -o %t/client %t/Client.swift %t/Library.o
// RUN: %target-codesign %t/client
// RUN: %target-run %t/client

//--- Utility.h
typedef struct {
  int count;
  double value;
} TrivialHiddenCStruct;

//--- Library.swift
public struct TrivialHiddenCStructWrapper {
  var hiddenField: TrivialHiddenCStruct
  public var visibleField: Int64

  public init(count: Int32, value: Double, visibleField: Int64) {
    hiddenField = TrivialHiddenCStruct(count: count, value: value)
    self.visibleField = visibleField
  }

  public var hiddenCount: Int32 { hiddenField.count }
  public var hiddenValue: Double { hiddenField.value }
}

public func passThrough(_ value: TrivialHiddenCStructWrapper)
    -> TrivialHiddenCStructWrapper {
  value
}

//--- Client.swift
import Library

public func use(_ value: TrivialHiddenCStructWrapper)
    -> TrivialHiddenCStructWrapper {
  passThrough(value)
}

let input = TrivialHiddenCStructWrapper(
    count: 42, value: 2.5, visibleField: 100)
let output = use(input)
assert(output.hiddenCount == 42)
assert(output.hiddenValue == 2.5)
assert(output.visibleField == 100)

// The library derives this signature with the Clang AST available. The client
// must derive the same signature from the serialized TypeInfo representation.
// LIBRARY-SIDE-DAG: define {{.*}}swiftcc { i32, double, i64 } @"$s7Library11passThroughyAA27TrivialHiddenCStructWrapperVADF"(i32 {{.*}}, double {{.*}}, i64 {{.*}})
// CLIENT-SIDE-DAG: declare {{.*}}swiftcc { i32, double, i64 } @"$s7Library11passThroughyAA27TrivialHiddenCStructWrapperVADF"(i32, double, i64)

// CLIENT-SIDE-DAG: %T7Library27TrivialHiddenCStructWrapperV = type <{ <{ <{ i32 }>, [4 x i8], <{ double }> }>, %Ts5Int64V }>
