// RUN: %swift -parse -primary-file %s %S/Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - > %t.swiftdeps
// RUN: FileCheck %s < %t.swiftdeps
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t.swiftdeps

// CHECK-LABEL: {{^top-level:$}}

// CHECK-DAG: Comparable{{$}}
struct IntWrapper: Comparable {
  // CHECK-DAG: Int{{$}}
  var value: Int
}

// CHECK-DAG: IntWrapper{{$}}
// CHECK-DAG: Bool{{$}}
func ==(lhs: IntWrapper, rhs: IntWrapper) -> Bool {
  return lhs.value == rhs.value
}

func <(lhs: IntWrapper, rhs: IntWrapper) -> Bool {
  return lhs.value < rhs.value
}

// CHECK-DAG: ClassFromOtherFile{{$}}
class Subclass : ClassFromOtherFile {}

prefix operator ^^^ {}

// CHECK-DAG: Array{{$}}
typealias MyArray = Array<Bool>

// CHECK-DAG: IntegerLiteralType{{$}}
let someGlobal = 42

func lookUpManyTopLevelNames() {
  // CHECK-DAG: Dictionary{{$}}
  let _: Dictionary = [1:1]

  // CHECK-DAG: UInt{{$}}
  // CHECK-DAG: reduce{{$}}
  // CHECK-DAG: +{{$}}
  let _: UInt = reduce([1,2], 0, +)

  // CHECK-DAG: AliasFromOtherFile{{$}}
  let _: AliasFromOtherFile = 1

  // CHECK-DAG: funcFromOtherFile{{$}}
  funcFromOtherFile()

  // "ForwardIndex" is not used as a top-level name here.
  // CHECK-DAG: StringLiteralType{{$}}
  // NEGATIVE-NOT: ForwardIndex{{$}}
  let ForwardIndex = "abc"
  println(ForwardIndex)
}

// String is not used anywhere in this file.
// NEGATIVE-NOT: String{{$}}
// Int16 is used by the other file in this module, but not by this one.
// NEGATIVE-NOT: Int16{{$}}

let eof: () = ()
