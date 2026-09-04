// Test that a client can reconstruct a loadable Clang record TypeInfo when the
// internal bridging header that defined the record is unavailable.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Utility.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-module -emit-module-path %t/Library.swiftmodule \
// RUN:   -emit-ir -o %t/Library.ll -module-name Library -parse-as-library \
// RUN:   %t/Library.swift
// RUN: %FileCheck --check-prefix=LIBRARY %s < %t/Library.ll

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -o %t/Client.ll -module-name Client -parse-as-library \
// RUN:   -I %t %t/Client.swift
// RUN: %FileCheck --check-prefix=CLIENT %s < %t/Client.ll

//--- Utility.h
typedef struct {
  int count;
  double value;
} TrivialHiddenCStruct;

//--- Library.swift
public struct TrivialHiddenCStructWrapper {
  var hiddenField: TrivialHiddenCStruct
  public var visibleField: Int
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

// The library derives this signature with the Clang AST available. The client
// must derive the same signature from the serialized TypeInfo representation.
// LIBRARY-DAG: define swiftcc { i32, double, i64 } @"$s7Library11passThrough
// CLIENT-DAG: declare swiftcc { i32, double, i64 } @"$s7Library11passThrough{{.*}}"(i32, double, i64)

// CLIENT-DAG: %T7Library27TrivialHiddenCStructWrapperV = type <{ <{ <{ i32 }>, [4 x i8], <{ double }> }>, %TSi }>
// CLIENT-NOT: Utility.h
