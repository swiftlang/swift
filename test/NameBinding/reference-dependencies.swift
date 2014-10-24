// RUN: %swift -parse -primary-file %s %S/Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - > %t.swiftdeps
// RUN: FileCheck %s < %t.swiftdeps
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t.swiftdeps

// CHECK-LABEL: {{^provides:$}}
// CHECK-NEXT: "IntWrapper"
// CHECK-NEXT: "=="
// CHECK-NEXT: "<"
// CHECK-NEXT: "Subclass"
// CHECK-NEXT: "^^^"
// CHECK-NEXT: "MyArray"
// CHECK-NEXT: "someGlobal"
// CHECK-NEXT: "lookUpManyTopLevelNames"
// CHECK-NEXT: "eof"

// CHECK-LABEL: {{^top-level:$}}

// CHECK-DAG: "Comparable"
struct IntWrapper: Comparable {
  // CHECK-DAG: "Int"
  var value: Int
}

// CHECK-DAG: "IntWrapper"
// CHECK-DAG: "Bool"
func ==(lhs: IntWrapper, rhs: IntWrapper) -> Bool {
  // CHECK-DAG: "=="
  return lhs.value == rhs.value
}

func <(lhs: IntWrapper, rhs: IntWrapper) -> Bool {
  // CHECK-DAG: "<"
  return lhs.value < rhs.value
}

// CHECK-DAG: "ClassFromOtherFile"
class Subclass : ClassFromOtherFile {}

prefix operator ^^^ {}

// CHECK-DAG: "Array"
typealias MyArray = Array<Bool>

// CHECK-DAG: "IntegerLiteralType"
let someGlobal = 42

func lookUpManyTopLevelNames() {
  // CHECK-DAG: "Dictionary"
  let _: Dictionary = [1:1]

  // CHECK-DAG: "UInt"
  // CHECK-DAG: "reduce"
  // CHECK-DAG: "+"
  let _: UInt = reduce([1,2], 0, +)

  // CHECK-DAG: "AliasFromOtherFile"
  let _: AliasFromOtherFile = 1

  // CHECK-DAG: "funcFromOtherFile"
  funcFromOtherFile()

  // "CInt" is not used as a top-level name here.
  // CHECK-DAG: "StringLiteralType"
  // NEGATIVE-NOT: "CInt"
  let CInt = "abc"
  // CHECK-DAG: "println"
  println(CInt)

  // NEGATIVE-NOT: "max"
  println(Int.max)

  // NEGATIVE-NOT: "Stride"
  let _: Int.Stride = 0

  // CHECK-DAG: "OtherFileOuterType"
  _ = OtherFileOuterType.InnerType.sharedConstant

  // CHECK-DAG: "OtherFileAliasForSecret"
  _ = OtherFileAliasForSecret.constant
}

// NEGATIVE-NOT: "privateFunc"
private func privateFunc() {}

// CHECK-LABEL: {{^member-access:$}}
// CHECK-DAG: "V4main10IntWrapper"
// CHECK-DAG: "C4main18ClassFromOtherFile"
// CHECK-DAG: "C4main8Subclass"
// CHECK-DAG: "Si"
// CHECK-DAG: "PSs10Strideable"
// CHECK-DAG: "V4main18OtherFileOuterType"
// CHECK-DAG: "VV4main18OtherFileOuterType9InnerType"
// CHECK-DAG: "VV4main26OtherFileSecretTypeWrapper10SecretType"

// String is not used anywhere in this file, though a string literal is.
// NEGATIVE-NOT: "String"
// Int16 is used by the other file in this module, but not by this one.
// NEGATIVE-NOT: "Int16"

// OtherFileSecretTypeWrapper is never used directly in this file.
// NEGATIVE-NOT: "OtherFileSecretTypeWrapper"
// NEGATIVE-NOT: "V4main26OtherFileSecretTypeWrapper"

let eof: () = ()
