// RUN: %swift-ide-test -dynamic-lookup-completion -source-filename %s > %t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.txt

// CHECK: Begin members

// Dictionary<K, V>
// CHECK-DAG: var elements
// CHECK-DAG: var size
// CHECK-DAG: func getBucketCount() -> Int
// CHECK-DAG: func add(k : KeyType, v : ValueType) -> Bool
// CHECK-DAG: func find(k : KeyType) -> ValueType?
// CHECK-DAG: subscript (k : KeyType) -> ValueType

// ArrayBound
// CHECK-DAG: func getArrayBoundValue() -> ArrayBoundType

// CHECK: End members


// No static functions
// NEGATIVE-NOT: convertFromDictionaryLiteral

// Nothing that can't be accessed through a class instance
// NEGATIVE-NOT: typealias
// NEGATIVE-NOT: constructor
// NEGATIVE-NOT: case

// Nothing from a struct or struct extension
// NEGATIVE-NOT: var value : Builtin.Int64
// NEGATIVE-NOT: func succ() -> Int64
