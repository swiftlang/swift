// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/def_class.swift
// RUN: %swift-ide-test -dynamic-lookup-completion -source-filename %s -I=%t > %t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.txt

import class def_class.MyClass

class LocalClass {
  func foo(x : Float) {}
  var magic : ()
  
  typealias Invisible = LocalClass
}

var globalVar : Int
func globalFunc() {}


// CHECK: Begin members

// swift.Dictionary<K, V>
// CHECK-DAG: var elements
// CHECK-DAG: var size
// CHECK-DAG: func getBucketCount() -> Int
// CHECK-DAG: func add(k : KeyType, v : ValueType) -> Bool
// CHECK-DAG: func find(k : KeyType) -> ValueType?
// CHECK-DAG: subscript (k : KeyType) -> ValueType

// def_class.MyClass
// CHECK-DAG: func foo(x : Int, y : Int)
// CHECK-DAG: var bar : Int

// LocalClass:
// CHECK-DAG: func foo(x : Float)
// CHECK-DAG: var magic : ()

// CHECK: End members


// NEGATIVE-NOT: globalVar
// NEGATIVE-NOT: globalFunc
// NEGATIVE-NOT: Invisible
// NEGATIVE-NOT: hiddenFunc
