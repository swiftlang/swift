// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -parse -primary-file %t/main.swift %S/Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - > %t.swiftdeps
// RUN: FileCheck %s < %t.swiftdeps
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t.swiftdeps

// CHECK-LABEL: {{^provides:$}}
// CHECK-NEXT: "IntWrapper"
// CHECK-NEXT: "=="
// CHECK-NEXT: "<"
// CHECK-NEXT: "***"
// CHECK-NEXT: "^^^"
// CHECK-NEXT: "Subclass"
// CHECK-NEXT: "MyArray"
// CHECK-NEXT: "someGlobal"
// CHECK-NEXT: "ExtraFloatLiteralConvertible"
// CHECK-NEXT: "~~~"
// CHECK-NEXT: "ThreeTildeType"
// CHECK-NEXT: "overloadedOnProto"
// CHECK-NEXT: "overloadedOnProto"
// CHECK-NEXT: "lookUpManyTopLevelNames"
// CHECK-NEXT: "Outer"
// CHECK: "eof"

// CHECK-LABEL: {{^nominals:$}}
// CHECK-NEXT: "V4main10IntWrapper"
// CHECK-NEXT: "VV4main10IntWrapper16InnerForNoReason"
// CHECK-NEXT: "C4main8Subclass"
// CHECK-NEXT: "Si"
// CHECK-NEXT: "VE4mainSi10InnerToInt"

// CHECK-LABEL: {{^top-level:$}}

// CHECK-DAG: - "Comparable"
struct IntWrapper: Comparable {
  // CHECK-DAG: - "Int"
  var value: Int

  struct InnerForNoReason {}
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

// Test operator lookup without a use of the same operator.
// This is declared in the other file.
// CHECK-DAG: "***"
prefix func ***(lhs: IntWrapper) {}

// This is provided as an operator but not implemented here.
prefix operator ^^^ {}

// CHECK-DAG: "ClassFromOtherFile"
class Subclass : ClassFromOtherFile {}

// CHECK-DAG: "Array"
typealias MyArray = Array<Bool>

// CHECK-DAG: "IntegerLiteralType"
let someGlobal = 42

extension Int {
  struct InnerToInt {}
}

// CHECK-DAG: - "OtherFileAliasForFloatLiteralConvertible"
protocol ExtraFloatLiteralConvertible
    : OtherFileAliasForFloatLiteralConvertible {
}
// CHECK-DAG: !private "UnicodeScalarLiteralConvertible"
private protocol ExtraCharLiteralConvertible : UnicodeScalarLiteralConvertible {
}

prefix operator ~~~ {}
protocol ThreeTildeType {
  prefix func ~~~(lhs: Self)
}

private struct ThreeTildeTypeImpl : ThreeTildeType {
}

func overloadedOnProto<T>(_: T) {}
func overloadedOnProto<T: ThreeTildeType>(_: T) {}

// CHECK-DAG: !private "~~~"
private prefix func ~~~(_: ThreeTildeTypeImpl) {}

func lookUpManyTopLevelNames() {
  // CHECK-DAG: !private "Dictionary"
  let _: Dictionary = [1:1]

  // CHECK-DAG: !private "UInt"
  // CHECK-DAG: !private "reduce"
  // CHECK-DAG: !private "+"
  let _: UInt = reduce([1,2], 0, +)
  
  // CHECK-DAG: !private "-"
  let _: UInt = 3 - 2 - 1

  // CHECK-DAG: !private "AliasFromOtherFile"
  let _: AliasFromOtherFile = 1

  // CHECK-DAG: !private "funcFromOtherFile"
  funcFromOtherFile()

  // "CInt" is not used as a top-level name here.
  // CHECK-DAG: !private "StringLiteralType"
  // NEGATIVE-NOT: "CInt"
  let CInt = "abc"
  // CHECK-DAG: !private "println"
  println(CInt)

  // NEGATIVE-NOT: "max"
  println(Int.max)

  // NEGATIVE-NOT: "Stride"
  let _: Int.Stride = 0

  // CHECK-DAG: !private "OtherFileOuterType"
  _ = OtherFileOuterType.InnerType.sharedConstant
  _ = OtherFileOuterType.InnerType()

  // CHECK-DAG: !private "OtherFileAliasForSecret"
  _ = OtherFileAliasForSecret.constant  

  // CHECK-DAG: !private "otherFileUse"
  // CHECK-DAG: !private "otherFileGetImpl"
  otherFileUse(otherFileGetImpl())

  // CHECK-DAG: !private "otherFileUseGeneric"
  // CHECK-DAG: !private "otherFileGetImpl2"
  otherFileUseGeneric(otherFileGetImpl2())
  
  // CHECK-DAG: !private "getOtherFileIntArray"
  for _ in getOtherFileIntArray() {}
  
  // CHECK-DAG: !private "getOtherFileEnum"
  switch getOtherFileEnum() {
  case .Value:
    break
  }

  let _ = .Value as OtherFileEnumWrapper.Enum
  let _: OtherFileEnumWrapper.Enum = .Value
  let _ = OtherFileEnumWrapper.Enum.Value

  let _ = { (_: PrivateTopLevelStruct.ValueType) -> PrivateTopLevelStruct2.ValueType? in
    return nil
  }
  
  typealias X = OtherFileEnumWrapper.Enum
  
  let value: Any = .Value as X
  switch value {
  case is OtherFileEnumWrapper.Enum:
    break
  }

  // CHECK-DAG: !private "~="
  switch 42 {
  case 50:
    break
  }
  
  for _: OtherFileEnumWrapper.Enum in EmptyGenerator<X>() {}
  
  // CHECK-DAG: !private "otherFileGetNonImpl"
  overloadedOnProto(otherFileGetNonImpl())
}

struct Outer {
  struct Inner {
    func method() {
      // CHECK-DAG: !private "CUnsignedInt"
      let _: CUnsignedInt = 5
    }
  }
}

// NEGATIVE-NOT: "privateFunc"
private func privateFunc() {}

// CHECK-DAG: - "topLevel1"
var use1 = topLevel1()
// CHECK-DAG: - "topLevel2"
var use2 = { topLevel2() }
// CHECK-DAG: - "topLevel3"
var use3 = { ({ topLevel3() })() }
// CHECK-DAG: - "topLevel4"
// CHECK-DAG: - "TopLevelProto1"
struct Use4 : TopLevelProto1 {
  var use4 = topLevel4()
}

// CHECK-DAG: - "*"
print(42 * 30)

// FIXME: Incorrectly marked non-private dependencies
// CHECK-DAG: - "topLevel6"
print(topLevel6())
// CHECK-DAG: - "topLevel7"
private var use7 = topLevel7()
// CHECK-DAG: - "topLevel8"
var use8: Int = topLevel8()
// CHECK-DAG: - "topLevel9"
var use9 = { () -> Int in return topLevel9() }


// CHECK-DAG: - "TopLevelTy1"
func useTy1(x: TopLevelTy1) {}
// CHECK-DAG: - "TopLevelTy2"
func useTy2() -> TopLevelTy2 {}
// CHECK-DAG: - "TopLevelTy3"
// CHECK-DAG: - "TopLevelProto2"
extension Use4 : TopLevelProto2 {
  var useTy3: TopLevelTy3? { return nil }
}

let useTy4 = { (_: TopLevelStruct.ValueType) -> TopLevelStruct2.ValueType? in
  return nil
}
// CHECK-DAG: - "TopLevelStruct"
typealias useTy5 = TopLevelStruct3.ValueType
let useTy6: TopLevelStruct4.ValueType = 0


// CHECK-DAG: !private "privateTopLevel1"
func private1(a: Int = privateTopLevel1()) {}
// CHECK-DAG: !private "privateTopLevel2"
// CHECK-DAG: !private "PrivateProto1"
private struct Private2 : PrivateProto1 {
  var private2 = privateTopLevel2()
}
// CHECK-DAG: !private "privateTopLevel3"
func outerPrivate3() {
  let private3 = { privateTopLevel3() }
}

// CHECK-DAG: !private "PrivateTopLevelTy1"
private extension Use4 {
  var privateTy1: PrivateTopLevelTy1? { return nil }
} 
// CHECK-DAG: !private "PrivateTopLevelTy2"
// CHECK-DAG: "PrivateProto2"
extension Private2 : PrivateProto2 {
  var privateTy2: PrivateTopLevelTy2? { return nil }
}
// CHECK-DAG: !private "PrivateTopLevelTy3"
func outerPrivateTy3() {
  func inner(a: PrivateTopLevelTy3?) {}
  inner(nil)
}
// CHECK-DAG: !private "PrivateTopLevelStruct3"
private typealias PrivateTy4 = PrivateTopLevelStruct3.ValueType
// CHECK-DAG: !private "PrivateTopLevelStruct4"
private func privateTy5(x: PrivateTopLevelStruct4.ValueType) -> PrivateTopLevelStruct4.ValueType {
  return x
}

// Deliberately empty.
private struct PrivateTy6 {}
// CHECK-DAG: - "PrivateProto3"
extension PrivateTy6 : PrivateProto3 {}


// CHECK-LABEL: {{^member-access:$}}
// CHECK-DAG: - "V4main10IntWrapper"
// CHECK-DAG: - "PSs10Comparable"
// CHECK-DAG: - "C4main18ClassFromOtherFile"
// CHECK-DAG: - "C4main8Subclass"
// CHECK-DAG: !private "Si"
// CHECK-DAG: - "PSs23FloatLiteralConvertible"
// CHECK-DAG: !private "PSs31UnicodeScalarLiteralConvertible"
// CHECK-DAG: !private "PSs10Strideable"
// CHECK-DAG: !private "Sa"
// CHECK-DAG: !private "Sb"
// CHECK-DAG: !private "VSs10Dictionary"
// CHECK-DAG: !private "V4main17OtherFileIntArray"
// CHECK-DAG: !private "V4main18OtherFileOuterType"
// CHECK-DAG: !private "VV4main18OtherFileOuterType9InnerType"
// CHECK-DAG: !private "VV4main26OtherFileSecretTypeWrapper10SecretType"
// CHECK-DAG: !private "V4main25OtherFileProtoImplementor"
// CHECK-DAG: !private "V4main26OtherFileProtoImplementor2"
// CHECK-DAG: !private "V4main28OtherFileProtoNonImplementor"
// CHECK-DAG: !private "VSs14EmptyGenerator"
// CHECK-DAG: !private "VSs17IndexingGenerator"
// CHECK-DAG: - "O4main13OtherFileEnum"
// CHECK-DAG: !private "V4main20OtherFileEnumWrapper"
// CHECK-DAG: !private "V4main20OtherFileEnumWrapper"
// CHECK-DAG: !private "V4main20OtherFileEnumWrapper"

// CHECK-DAG: - "V4main14TopLevelStruct"
// CHECK-DAG: - "V4main15TopLevelStruct2"
// CHECK-DAG: - "V4main15TopLevelStruct3"
// CHECK-DAG: - "V4main15TopLevelStruct4"
// CHECK-DAG: !private "V4main21PrivateTopLevelStruct"
// CHECK-DAG: !private "V4main22PrivateTopLevelStruct2"
// CHECK-DAG: !private "V4main22PrivateTopLevelStruct3"
// CHECK-DAG: !private "V4main22PrivateTopLevelStruct4"

// CHECK-DAG: - "P4main14TopLevelProto1"
// CHECK-DAG: - "P4main14TopLevelProto2"
// CHECK-DAG: !private "P4main13PrivateProto1"
// CHECK-DAG: !private "P4main13PrivateProto2"
// CHECK-DAG: !private "P4main13PrivateProto3"

// String is not used anywhere in this file, though a string literal is.
// NEGATIVE-NOT: "String"
// NEGATIVE-NOT: "SS"
// These are used by the other file in this module, but not by this one.
// NEGATIVE-NOT: "FloatLiteralConvertible"
// NEGATIVE-NOT: "Int16"
// NEGATIVE-NOT: "OtherFileProto"
// NEGATIVE-NOT: "OtherFileProtoImplementor"
// NEGATIVE-NOT: "OtherFileProto2"
// NEGATIVE-NOT: "OtherFileProtoImplementor2"

// OtherFileSecretTypeWrapper is never used directly in this file.
// NEGATIVE-NOT: "OtherFileSecretTypeWrapper"
// NEGATIVE-NOT: "V4main26OtherFileSecretTypeWrapper"

let eof: () = ()
