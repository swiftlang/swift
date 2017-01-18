// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - > %t.swiftdeps
// RUN: %FileCheck %s < %t.swiftdeps
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.swiftdeps

// Check that the output is deterministic.
// RUN: %target-swift-frontend -typecheck -primary-file %t/main.swift %S/Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - > %t-2.swiftdeps
// RUN: diff %t.swiftdeps %t-2.swiftdeps

// CHECK-LABEL: {{^provides-top-level:$}}
// CHECK-NEXT: "IntWrapper"
// CHECK-NEXT: "=="
// CHECK-NEXT: "<"
// CHECK-NEXT: "***"
// CHECK-NEXT: "^^^"
// CHECK-NEXT: "Subclass"
// CHECK-NEXT: "MyArray"
// CHECK-NEXT: "someGlobal"
// CHECK-NEXT: "ExpressibleByExtraFloatLiteral"
// CHECK-NEXT: "~~~"
// CHECK-NEXT: "ThreeTilde"
// CHECK-NEXT: "overloadedOnProto"
// CHECK-NEXT: "overloadedOnProto"
// CHECK-NEXT: "~~~~"
// CHECK-NEXT: "FourTilde"
// CHECK-NEXT: "FourTildeImpl"
// CHECK-NEXT: "FiveTildeImpl"
// CHECK-NEXT: "topLevelComputedProperty"
// CHECK-NEXT: "lookUpManyTopLevelNames"
// CHECK-NEXT: "testOperators"
// CHECK-NEXT: "TopLevelForMemberLookup"
// CHECK-NEXT: "lookUpMembers"
// CHECK-NEXT: "publicUseOfMember"
// CHECK-NEXT: "Outer"
// CHECK: "eof"
// CHECK-NEXT: "~~~"
// CHECK-NEXT: "~~~~"
// CHECK-NEXT: "~~~~"
// CHECK-NEXT: "~~~~~"

// CHECK-LABEL: {{^provides-nominal:$}}
// CHECK-NEXT: "V4main10IntWrapper"
// CHECK-NEXT: "VV4main10IntWrapper16InnerForNoReason"
// CHECK-NEXT: "C4main8Subclass"
// CHECK-NEXT: "VE4mainSb11InnerToBool"
// CHECK: "V4main9Sentinel1"
// CHECK-NEXT: "V4main9Sentinel2"

// CHECK-LABEL: {{^provides-member:$}}
// CHECK-NEXT: - ["V4main10IntWrapper", ""]
// CHECK-NEXT: - ["VV4main10IntWrapper16InnerForNoReason", ""]
// CHECK-NEXT: - ["C4main8Subclass", ""]
// CHECK-NEXT: - ["Ps25ExpressibleByArrayLiteral", ""]
// CHECK-NEXT: - ["Sb", ""]
// CHECK-NEXT: - ["VE4mainSb11InnerToBool", ""]
// CHECK: - ["V4main9Sentinel1", ""]
// CHECK-NEXT: - ["V4main9Sentinel2", ""]
// CHECK: - ["Ps25ExpressibleByArrayLiteral", "useless"]
// CHECK-NEXT: - ["Ps25ExpressibleByArrayLiteral", "useless2"]
// CHECK-NEXT: - ["Sb", "InnerToBool"]
// CHECK-NEXT: - ["{{.*[0-9]}}FourTildeImpl", "~~~~"]
// CHECK-NEXT: - ["{{.*[0-9]}}FiveTildeImpl", "~~~~~"]

// CHECK-LABEL: {{^depends-top-level:$}}

// CHECK-DAG: - "Comparable"
struct IntWrapper: Comparable {
  // CHECK-DAG: - "Int"
  var value: Int

  struct InnerForNoReason {}

  // CHECK-DAG: - "TypeReferencedOnlyBySubscript"
  subscript(_: TypeReferencedOnlyBySubscript) -> Void { return () }

  // CHECK-DAG: - "TypeReferencedOnlyByPrivateSubscript"
  // FIXME: This should be marked "!private".
  private subscript(_: TypeReferencedOnlyByPrivateSubscript) -> Void { return () }
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
prefix operator ^^^

// CHECK-DAG: "ClassFromOtherFile"
class Subclass : ClassFromOtherFile {}

// CHECK-DAG: "Array"
typealias MyArray = Array<Bool>

// CHECK-DAG: "ExpressibleByArrayLiteral"
extension ExpressibleByArrayLiteral {
  final func useless() {}
}

// CHECK-DAG: OtherFileElementType
extension ExpressibleByArrayLiteral where Element == OtherFileElementType {
  final func useless2() {}
}

// CHECK-DAG: "IntegerLiteralType"
let someGlobal = 42

extension Bool {
  struct InnerToBool {}
}

// CHECK-DAG: - "ExpressibleByOtherFileAliasForFloatLiteral"
protocol ExpressibleByExtraFloatLiteral
    : ExpressibleByOtherFileAliasForFloatLiteral {
}
// CHECK-DAG: !private "ExpressibleByUnicodeScalarLiteral"
private protocol ExpressibleByExtraCharLiteral : ExpressibleByUnicodeScalarLiteral {
}

prefix operator ~~~
protocol ThreeTilde {
  prefix static func ~~~(lhs: Self)
}

private struct ThreeTildeTypeImpl : ThreeTilde {
}

func overloadedOnProto<T>(_: T) {}
func overloadedOnProto<T: ThreeTilde>(_: T) {}

// CHECK-DAG: - "~~~"
private prefix func ~~~(_: ThreeTildeTypeImpl) {}

// CHECK-DAG: - "~~~~"
prefix operator ~~~~
protocol FourTilde {
  prefix static func ~~~~(arg: Self)
}
struct FourTildeImpl : FourTilde {}
extension FourTildeImpl {
  prefix static func ~~~~(arg: FourTildeImpl) {}
}

// CHECK-DAG: - "~~~~~"
// ~~~~~ is declared in the other file.
struct FiveTildeImpl {}
extension FiveTildeImpl {
  prefix static func ~~~~~(arg: FiveTildeImpl) {}
}

var topLevelComputedProperty: Bool {
  return true
}

func lookUpManyTopLevelNames() {
  // CHECK-DAG: !private "Dictionary"
  let _: Dictionary = [1:1]

  // CHECK-DAG: !private "UInt"
  // CHECK-DAG: !private "+"
  let _: UInt = [1, 2].reduce(0, +)
  
  // CHECK-DAG: !private "-"
  let _: UInt = 3 - 2 - 1

  // CHECK-DAG: !private "AliasFromOtherFile"
  let _: AliasFromOtherFile = 1

  // CHECK-DAG: !private "funcFromOtherFile"
  funcFromOtherFile()

  // "CInt" is not used as a top-level name here.
  // CHECK-DAG: !private "StringLiteralType"
  // NEGATIVE-NOT: "CInt"
  _ = "abc"

  // NEGATIVE-NOT: - "max"
  print(Int.max)

  // NEGATIVE-NOT: - "Stride"
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

  _ = .Value as OtherFileEnumWrapper.Enum
  let _: OtherFileEnumWrapper.Enum = .Value
  _ = OtherFileEnumWrapper.Enum.Value

  _ = { (_: PrivateTopLevelStruct.ValueType) -> PrivateTopLevelStruct2.ValueType? in
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
  
  for _: OtherFileEnumWrapper.Enum in EmptyIterator<X>() {}
  
  // CHECK-DAG: !private "otherFileGetNonImpl"
  overloadedOnProto(otherFileGetNonImpl())
}

func testOperators<T: Starry>(generic: T, specific: Flyswatter) {
  // CHECK-DAG: !private "****"
  // CHECK-DAG: !private "*****"
  // CHECK-DAG: !private "******"
  ****generic
  generic*****0
  0******generic

  ****specific
  specific*****0
  0******specific
}

struct TopLevelForMemberLookup {
  static func m1() {}
  static func m2() {}
  static func m3() {}
}

func lookUpMembers() {
  TopLevelForMemberLookup.m1()
  TopLevelForMemberLookup.m3()
}
public let publicUseOfMember: () = TopLevelForMemberLookup.m2()

struct Outer {
  struct Inner {
    func method() {
      // CHECK-DAG: !private "CUnsignedInt"
      let _: CUnsignedInt = 5
    }
  }
}

// CHECK-DAG: !private "privateFunc"
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
_ = 42 * 30

// FIXME: Incorrectly marked non-private dependencies
// CHECK-DAG: - "topLevel6"
_ = topLevel6()
// CHECK-DAG: - "topLevel7"
private var use7 = topLevel7()
// CHECK-DAG: - "topLevel8"
var use8: Int = topLevel8()
// CHECK-DAG: - "topLevel9"
var use9 = { () -> Int in return topLevel9() }


// CHECK-DAG: - "TopLevelTy1"
func useTy1(_ x: TopLevelTy1) {}
// CHECK-DAG: - "TopLevelTy2"
func useTy2() -> TopLevelTy2 {}
// CHECK-DAG: - "TopLevelTy3"
// CHECK-DAG: - "TopLevelProto2"
extension Use4 : TopLevelProto2 {
  var useTy3: TopLevelTy3? { return nil }
}

// CHECK-DAG: - "TopLevelStruct"
// CHECK-DAG: - "TopLevelStruct2"
let useTy4 = { (_: TopLevelStruct.ValueType) -> TopLevelStruct2.ValueType? in
  return nil
}
// CHECK-DAG: - "TopLevelStruct3"
// CHECK-DAG: - "TopLevelStruct4"
typealias useTy5 = TopLevelStruct3.ValueType
let useTy6: TopLevelStruct4.ValueType = 0

struct StructForDeclaringProperties {
  // CHECK-DAG: - "TopLevelStruct5"
  var prop: TopLevelStruct5.ValueType { return 0 }
}

// CHECK-DAG: !private "privateTopLevel1"
func private1(_ a: Int = privateTopLevel1()) {}
// CHECK-DAG: !private "privateTopLevel2"
// CHECK-DAG: !private "PrivateProto1"
private struct Private2 : PrivateProto1 {
  var private2 = privateTopLevel2()
}
// CHECK-DAG: !private "privateTopLevel3"
func outerPrivate3() {
  let _ = { privateTopLevel3() }
}

// CHECK-DAG: !private "PrivateTopLevelTy1"
private extension Use4 {
  var privateTy1: PrivateTopLevelTy1? { return nil }
} 
// CHECK-DAG: !private "PrivateTopLevelTy2"
// CHECK-DAG: "PrivateProto2"
extension Private2 : PrivateProto2 {
  // FIXME: This test is supposed to check that we get this behavior /without/
  // marking the property private, just from the base type.
  private var privateTy2: PrivateTopLevelTy2? { return nil }
}
// CHECK-DAG: !private "PrivateTopLevelTy3"
func outerPrivateTy3() {
  func inner(_ a: PrivateTopLevelTy3?) {}
  inner(nil)
}
// CHECK-DAG: !private "PrivateTopLevelStruct3"
private typealias PrivateTy4 = PrivateTopLevelStruct3.ValueType
// CHECK-DAG: !private "PrivateTopLevelStruct4"
private func privateTy5(_ x: PrivateTopLevelStruct4.ValueType) -> PrivateTopLevelStruct4.ValueType {
  return x
}

// Deliberately empty.
private struct PrivateTy6 {}
// CHECK-DAG: !private "PrivateProto3"
extension PrivateTy6 : PrivateProto3 {}

// CHECK-DAG: - "ProtoReferencedOnlyInGeneric"
func genericTest<T: ProtoReferencedOnlyInGeneric>(_: T) {}
// CHECK-DAG: !private "ProtoReferencedOnlyInPrivateGeneric"
private func privateGenericTest<T: ProtoReferencedOnlyInPrivateGeneric>(_: T) {}

struct PrivateStoredProperty {
  // CHECK-DAG: - "TypeReferencedOnlyByPrivateVar"
  private var value: TypeReferencedOnlyByPrivateVar
}
class PrivateStoredPropertyRef {
  // CHECK-DAG: - "TypeReferencedOnlyByPrivateClassVar"
  private var value: TypeReferencedOnlyByPrivateClassVar?
}

struct Sentinel1 {}

private protocol ExtensionProto {}
extension OtherFileTypeToBeExtended : ExtensionProto {
  private func foo() {}
}
private extension OtherFileTypeToBeExtended {
  var bar: Bool { return false }
}

struct Sentinel2 {}


// CHECK-LABEL: {{^depends-member:$}}
// CHECK-DAG: - ["V4main10IntWrapper", "Int"]
// CHECK-DAG: - ["V4main10IntWrapper", "deinit"]
// CHECK-DAG: - ["Ps10Comparable", ""]
// CHECK-DAG: - ["C4main18ClassFromOtherFile", ""]
// CHECK-DAG: - !private ["Si", "max"]
// CHECK-DAG: - ["Ps25ExpressibleByFloatLiteral", ""]
// CHECK-DAG: - !private ["Ps33ExpressibleByUnicodeScalarLiteral", ""]
// CHECK-DAG: - !private ["Ps10Strideable", "Stride"]
// CHECK-DAG: - !private ["Sa", "reduce"]
// CHECK-DAG: - !private ["Sb", "_getBuiltinLogicValue"]
// CHECK-DAG: - ["Sb", "InnerToBool"]
// CHECK-DAG: - !private ["V4main17OtherFileIntArray", "deinit"]
// CHECK-DAG: - !private ["V4main18OtherFileOuterType", "InnerType"]
// CHECK-DAG: - !private ["VV4main18OtherFileOuterType9InnerType", "init"]
// CHECK-DAG: - !private ["VV4main18OtherFileOuterType9InnerType", "sharedConstant"]
// CHECK-DAG: - !private ["VV4main26OtherFileSecretTypeWrapper10SecretType", "constant"]
// CHECK-DAG: - !private ["V4main25OtherFileProtoImplementor", "deinit"]
// CHECK-DAG: - !private ["V4main26OtherFileProtoImplementor2", "deinit"]
// CHECK-DAG: - !private ["Vs13EmptyIterator", "init"]
// CHECK-DAG: - ["O4main13OtherFileEnum", "Value"]
// CHECK-DAG: - !private ["V4main20OtherFileEnumWrapper", "Enum"]

// CHECK-DAG: - ["V4main14TopLevelStruct", "ValueType"]
// CHECK-DAG: - ["V4main15TopLevelStruct2", "ValueType"]
// CHECK-DAG: - ["V4main15TopLevelStruct3", "ValueType"]
// CHECK-DAG: - ["V4main15TopLevelStruct4", "ValueType"]
// CHECK-DAG: - ["V4main15TopLevelStruct5", "ValueType"]
// CHECK-DAG: - !private ["V4main21PrivateTopLevelStruct", "ValueType"]
// CHECK-DAG: - !private ["V4main22PrivateTopLevelStruct2", "ValueType"]
// CHECK-DAG: - !private ["V4main22PrivateTopLevelStruct3", "ValueType"]
// CHECK-DAG: - !private ["V4main22PrivateTopLevelStruct4", "ValueType"]

// CHECK-DAG: - ["P4main14TopLevelProto1", ""]
// CHECK-DAG: - ["P4main14TopLevelProto2", ""]
// CHECK-DAG: - !private ["P4main13PrivateProto1", ""]
// CHECK-DAG: - !private ["P4main13PrivateProto2", ""]
// CHECK-DAG: - !private ["P4main13PrivateProto3", ""]

// CHECK-LABEL: {{^depends-nominal:$}}
// We're checking order here to make sure the names are sorted.
// CHECK: - !private "Sa"
// CHECK: - "Sb"
// CHECK: - "C4main18ClassFromOtherFile"
// CHECK: - "Ps10Comparable"
// CHECK: - !private "Vs13EmptyIterator"
// CHECK: - "Ps25ExpressibleByFloatLiteral"
// CHECK: - !private "Ps33ExpressibleByUnicodeScalarLiteral"
// CHECK: - !private "VV4main18OtherFileOuterType9InnerType"
// CHECK: - !private "Si"
// CHECK: - "V4main10IntWrapper"
// CHECK: - "O4main13OtherFileEnum"
// CHECK: - !private "V4main20OtherFileEnumWrapper"
// CHECK: - !private "V4main17OtherFileIntArray"
// CHECK: - !private "V4main18OtherFileOuterType"
// CHECK: - !private "V4main25OtherFileProtoImplementor"
// CHECK: - !private "V4main26OtherFileProtoImplementor2"
// CHECK: - !private "P4main13PrivateProto1"
// CHECK: - !private "P4main13PrivateProto2"
// CHECK: - !private "P4main13PrivateProto3"
// CHECK: - !private "V4main21PrivateTopLevelStruct"
// CHECK: - !private "V4main22PrivateTopLevelStruct2"
// CHECK: - !private "V4main22PrivateTopLevelStruct3"
// CHECK: - !private "V4main22PrivateTopLevelStruct4"
// CHECK: - !private "VV4main26OtherFileSecretTypeWrapper10SecretType"
// CHECK: - !private "Ps10Strideable"
// CHECK: - "V4main23TopLevelForMemberLookup"
// CHECK: - "P4main14TopLevelProto1"
// CHECK: - "P4main14TopLevelProto2"
// CHECK: - "V4main14TopLevelStruct"
// CHECK: - "V4main15TopLevelStruct2"
// CHECK: - "V4main15TopLevelStruct3"
// CHECK: - "V4main15TopLevelStruct4"
// CHECK: - "V4main15TopLevelStruct5"

// String is not used anywhere in this file, though a string literal is.
// NEGATIVE-NOT: "String"
// These are used by the other file in this module, but not by this one.
// NEGATIVE-NOT: "ExpressibleByFloatLiteral"
// NEGATIVE-NOT: "Int16"
// NEGATIVE-NOT: "OtherFileProto"
// NEGATIVE-NOT: "OtherFileProtoImplementor"
// NEGATIVE-NOT: "OtherFileProto2"
// NEGATIVE-NOT: "OtherFileProtoImplementor2"

// OtherFileSecretTypeWrapper is never used directly in this file.
// NEGATIVE-NOT: "OtherFileSecretTypeWrapper"
// NEGATIVE-NOT: "V4main26OtherFileSecretTypeWrapper"

let eof: () = ()
