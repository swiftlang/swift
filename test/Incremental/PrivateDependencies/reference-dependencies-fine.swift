// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Need -fine-grained-dependency-include-intrafile to be invarient wrt type-body-fingerprints enabled/disabled
// RUN: %target-swift-frontend -fine-grained-dependency-include-intrafile -typecheck -primary-file %t/main.swift %S/../Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - -enable-direct-intramodule-dependencies > %t.swiftdeps
// Check that the output is deterministic.
// RUN: %target-swift-frontend  -fine-grained-dependency-include-intrafile -typecheck -primary-file %t/main.swift %S/../Inputs/reference-dependencies-helper.swift -emit-reference-dependencies-path - -enable-direct-intramodule-dependencies > %t-2.swiftdeps

// Merge each entry onto one line and sort to overcome order differences
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t-2.swiftdeps %t-2-processed.swiftdeps
// RUN: diff %t-processed.swiftdeps %t-2-processed.swiftdeps

// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t-processed.swiftdeps
// RUN: %FileCheck %s -check-prefix=CHECK-TOPLEVEL < %t-processed.swiftdeps
// RUN: %FileCheck %s -check-prefix=CHECK-MEMBER < %t-processed.swiftdeps
// RUN: %FileCheck %s -check-prefix=CHECK-NOMINAL < %t-processed.swiftdeps
// RUN: %FileCheck %s -check-prefix=CHECK-NOMINAL-2 < %t-processed.swiftdeps
// RUN: %FileCheck %s -check-prefix=CHECK-POTENTIALMEMBER < %t-processed.swiftdeps


// CHECK-TOPLEVEL-DAG: topLevel interface '' IntWrapper true
// CHECK-TOPLEVEL-DAG: topLevel interface '' '==' true
// CHECK-TOPLEVEL-DAG: topLevel interface '' '<' true
// CHECK-TOPLEVEL-DAG: topLevel interface '' '***' true
// CHECK-TOPLEVEL-DAG: topLevel interface '' ^^^ true
// CHECK-TOPLEVEL-DAG: topLevel interface '' Subclass true
// CHECK-TOPLEVEL-DAG: topLevel interface '' MyArray true
// CHECK-TOPLEVEL-DAG: topLevel interface '' someGlobal true
// CHECK-TOPLEVEL-DAG: topLevel interface '' ExpressibleByExtraFloatLiteral true
// CHECK-TOPLEVEL-DAG: topLevel interface '' ThreeTilde true
// CHECK-TOPLEVEL-DAG: topLevel interface '' overloadedOnProto true
// CHECK-TOPLEVEL-DAG: topLevel interface '' FourTilde true
// CHECK-TOPLEVEL-DAG: topLevel interface '' FourTildeImpl true
// CHECK-TOPLEVEL-DAG: topLevel interface '' FiveTildeImpl true
// CHECK-TOPLEVEL-DAG: topLevel interface '' topLevelComputedProperty true
// CHECK-TOPLEVEL-DAG: topLevel interface '' lookUpManyTopLevelNames true
// CHECK-TOPLEVEL-DAG: topLevel interface '' testOperators true
// CHECK-TOPLEVEL-DAG: topLevel interface '' TopLevelForMemberLookup true
// CHECK-TOPLEVEL-DAG: topLevel interface '' lookUpMembers true
// CHECK-TOPLEVEL-DAG: topLevel interface '' publicUseOfMember true
// CHECK-TOPLEVEL-DAG: topLevel interface '' Outer true
// CHECK-TOPLEVEL-DAG: topLevel interface '' eof true
// CHECK-TOPLEVEL-DAG: topLevel interface '' '~~~' true
// CHECK-TOPLEVEL-DAG: topLevel interface '' '~~~~' true
// CHECK-TOPLEVEL-DAG: topLevel interface '' '~~~~~' true

// CHECK-TOPLEVEL-DAG: topLevel implementation  '' IntWrapper true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' '==' true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' '<' true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' '***' true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' ^^^ true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' Subclass true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' MyArray true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' someGlobal true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' ExpressibleByExtraFloatLiteral true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' ThreeTilde true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' overloadedOnProto true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' FourTilde true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' FourTildeImpl true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' FiveTildeImpl true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' topLevelComputedProperty true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' lookUpManyTopLevelNames true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' testOperators true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' TopLevelForMemberLookup true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' lookUpMembers true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' publicUseOfMember true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' Outer true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' eof true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' '~~~' true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' '~~~~' true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' '~~~~~' true


// CHECK-NOMINAL-DAG: nominal interface 4main10IntWrapperV '' true
// CHECK-NOMINAL-DAG: nominal interface 4main10IntWrapperV16InnerForNoReasonV '' true
// CHECK-NOMINAL-DAG: nominal interface 4main8SubclassC '' true
// CHECK-NOMINAL-DAG: nominal interface Sb4mainE11InnerToBoolV '' true
// CHECK-NOMINAL-DAG: nominal interface 4main9Sentinel1V '' true
// CHECK-NOMINAL-DAG: nominal interface 4main9Sentinel2V '' true

// CHECK-NOMINAL-DAG: nominal implementation  4main10IntWrapperV '' true
// CHECK-NOMINAL-DAG: nominal implementation  4main10IntWrapperV16InnerForNoReasonV '' true
// CHECK-NOMINAL-DAG: nominal implementation  4main8SubclassC '' true
// CHECK-NOMINAL-DAG: nominal implementation  Sb4mainE11InnerToBoolV '' true
// CHECK-NOMINAL-DAG: nominal implementation  4main9Sentinel1V '' true
// CHECK-NOMINAL-DAG: nominal implementation  4main9Sentinel2V '' true


// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main10IntWrapperV '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main10IntWrapperV16InnerForNoReasonV '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main8SubclassC '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface s25ExpressibleByArrayLiteralP '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface Sb '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface Sb4mainE11InnerToBoolV '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main9Sentinel1V '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main9Sentinel2V '' true

// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  4main10IntWrapperV '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  4main10IntWrapperV16InnerForNoReasonV '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  4main8SubclassC '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  s25ExpressibleByArrayLiteralP '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  Sb '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  Sb4mainE11InnerToBoolV '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  4main9Sentinel1V '' true
// CHECK-POTENTIALMEMBER-DAG: potentialMember implementation  4main9Sentinel2V '' true


// CHECK-MEMBER-DAG: member interface s25ExpressibleByArrayLiteralP useless true
// CHECK-MEMBER-DAG: member interface s25ExpressibleByArrayLiteralP useless2 true
// CHECK-MEMBER-DAG: member interface Sb InnerToBool true
// CHECK-MEMBER-DAG: member interface {{.*[0-9]}}FourTildeImplV '~~~~' true
// CHECK-MEMBER-DAG: member interface {{.*[0-9]}}FiveTildeImplV '~~~~~' true

// CHECK-MEMBER-DAG: member implementation  s25ExpressibleByArrayLiteralP useless true
// CHECK-MEMBER-DAG: member implementation  s25ExpressibleByArrayLiteralP useless2 true
// CHECK-MEMBER-DAG: member implementation  Sb InnerToBool true
// CHECK-MEMBER-DAG: member implementation  {{.*[0-9]}}FourTildeImplV '~~~~' true
// CHECK-MEMBER-DAG: member implementation  {{.*[0-9]}}FiveTildeImplV '~~~~~' true


// CHECK-TOPLEVEL-DAG: topLevel interface  '' Comparable false

struct IntWrapper: Comparable {
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' Int false
  var value: Int

  struct InnerForNoReason {}

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' TypeReferencedOnlyBySubscript false
  subscript(_: TypeReferencedOnlyBySubscript) -> Void { return () }

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' TypeReferencedOnlyByPrivateSubscript false
  private subscript(_: TypeReferencedOnlyByPrivateSubscript) -> Void { return () }
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' Bool false
func ==(lhs: IntWrapper, rhs: IntWrapper) -> Bool {
  return lhs.value == rhs.value
}

func <(lhs: IntWrapper, rhs: IntWrapper) -> Bool {
  return lhs.value < rhs.value
}

// Test operator lookup without a use of the same operator.
// This is declared in the other file.
prefix func ***(lhs: IntWrapper) {}

// This is provided as an operator but not implemented here.
prefix operator ^^^

// CHECK-TOPLEVEL-DAG: topLevel interface  '' ClassFromOtherFile false
class Subclass : ClassFromOtherFile {}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' Array false
typealias MyArray = Array<Bool>

// CHECK-TOPLEVEL-DAG: topLevel interface  '' ExpressibleByArrayLiteral false
extension ExpressibleByArrayLiteral {
  func useless() {}
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' OtherFileElementType false
extension ExpressibleByArrayLiteral where ArrayLiteralElement == OtherFileElementType {
  func useless2() {}
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' IntegerLiteralType false
let someGlobal = 42

extension Bool {
  struct InnerToBool {}
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' ExpressibleByOtherFileAliasForFloatLiteral false
protocol ExpressibleByExtraFloatLiteral
    : ExpressibleByOtherFileAliasForFloatLiteral {
}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' ExpressibleByUnicodeScalarLiteral false
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

private prefix func ~~~(_: ThreeTildeTypeImpl) {}

prefix operator ~~~~
protocol FourTilde {
  prefix static func ~~~~(arg: Self)
}
struct FourTildeImpl : FourTilde {}
extension FourTildeImpl {
  prefix static func ~~~~(arg: FourTildeImpl) {}
}

// ~~~~~ is declared in the other file.
struct FiveTildeImpl {}
extension FiveTildeImpl {
  prefix static func ~~~~~(arg: FiveTildeImpl) {}
}

var topLevelComputedProperty: Bool {
  return true
}

func lookUpManyTopLevelNames() {
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' Dictionary false
  let _: Dictionary = [1:1]

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' UInt false
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' '+' false
  let _: UInt = [1, 2].reduce(0, +)
  
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' '-' false
  let _: UInt = 3 - 2 - 1

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' AliasFromOtherFile false
  let _: AliasFromOtherFile = 1

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' funcFromOtherFile false
  funcFromOtherFile()

  // "CInt" is not used as a top-level name here.
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' StringLiteralType false
  // NEGATIVE-NOT: "CInt"
  _ = "abc"

  // NEGATIVE-NOT: - "max"
  print(Int.max)

  // NEGATIVE-NOT: - "Stride"
  let _: Int.Stride = 0

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' OtherFileOuterType false
  _ = OtherFileOuterType.InnerType.sharedConstant
  _ = OtherFileOuterType.InnerType()

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' OtherFileAliasForSecret false
  _ = OtherFileAliasForSecret.constant

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' otherFileUse false
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' otherFileGetImpl false
  otherFileUse(otherFileGetImpl())

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' otherFileUseGeneric false
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' otherFileGetImpl2 false
  otherFileUseGeneric(otherFileGetImpl2())
  
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' getOtherFileIntArray false
  for _ in getOtherFileIntArray() {}
  
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' getOtherFileEnum false
  switch getOtherFileEnum() {
  case .Value:
    break
  default:
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
  default:
    break
  }

  // CHECK-TOPLEVEL-DAG: topLevel interface  '' '~=' false
  switch 42 {
  case 50:
    break
  default:
    break
  }
  
  for _: OtherFileEnumWrapper.Enum in EmptyIterator<X>() {}
  
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' otherFileGetNonImpl false
  overloadedOnProto(otherFileGetNonImpl())
}

func testOperators<T: Starry>(generic: T, specific: Flyswatter) {
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' '****' false
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' '*****' false
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' '******' false
  ****generic
  generic*****0
  0******generic

  ****specific
  specific*****0
  0******specific
}

// CHECK-NOMINAL-DAG: nominal interface 4main23TopLevelForMemberLookupV '' true
// CHECK-NOMINAL-DAG: nominal implementation  4main23TopLevelForMemberLookupV '' true

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
      // CHECK-TOPLEVEL-DAG: topLevel interface  '' CUnsignedInt false
      let _: CUnsignedInt = 5
    }
  }
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' privateFunc true
// CHECK-TOPLEVEL-DAG: topLevel implementation  '' privateFunc true
private func privateFunc() {}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel1 false
var use1 = topLevel1()
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel2 false
var use2 = { topLevel2() }
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel3 false
var use3 = { ({ topLevel3() })() }
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel4 false
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelProto1 false
struct Use4 : TopLevelProto1 {
  var use4 = topLevel4()
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' '*' false
_ = 42 * 30

// FIXME: Incorrectly marked non-private dependencies
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel6 false
_ = topLevel6()
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel7 false
private var use7 = topLevel7()
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel8 false
var use8: Int = topLevel8()
// CHECK-TOPLEVEL-DAG: topLevel interface  '' topLevel9 false
var use9 = { () -> Int in return topLevel9() }


// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelTy1 false
func useTy1(_ x: TopLevelTy1) {}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelTy2 false
func useTy2() -> TopLevelTy2 {}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelTy3 false
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelProto2 false
extension Use4 : TopLevelProto2 {
  var useTy3: TopLevelTy3? { return nil }
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelStruct false
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelStruct2 false
let useTy4 = { (_: TopLevelStruct.ValueType) -> TopLevelStruct2.ValueType? in
  return nil
}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelStruct3 false
// CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelStruct4 false
typealias useTy5 = TopLevelStruct3.ValueType
let useTy6: TopLevelStruct4.ValueType = 0

struct StructForDeclaringProperties {
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' TopLevelStruct5 false
  var prop: TopLevelStruct5.ValueType { return 0 }
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' privateTopLevel1 false
func private1(_ a: Int = privateTopLevel1()) {}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' privateTopLevel2 false
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateProto1 false
private struct Private2 : PrivateProto1 {
  var private2 = privateTopLevel2()
}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' privateTopLevel3 false
func outerPrivate3() {
  let _ = { privateTopLevel3() }
}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateTopLevelTy1 false
private extension Use4 {
  var privateTy1: PrivateTopLevelTy1? { return nil }
} 
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateTopLevelTy2 false
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateProto2 false
extension Private2 : PrivateProto2 {
  // FIXME: This test is supposed to check that we get this behavior /without/
  // marking the property private, just from the base type.
  private var privateTy2: PrivateTopLevelTy2? { return nil }
}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateTopLevelTy3 false
func outerPrivateTy3() {
  func inner(_ a: PrivateTopLevelTy3?) {}
  inner(nil)
}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateTopLevelStruct3 false
private typealias PrivateTy4 = PrivateTopLevelStruct3.ValueType
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateTopLevelStruct4 false
private func privateTy5(_ x: PrivateTopLevelStruct4.ValueType) -> PrivateTopLevelStruct4.ValueType {
  return x
}

// Deliberately empty.
private struct PrivateTy6 {}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' PrivateProto3 false
extension PrivateTy6 : PrivateProto3 {}

// CHECK-TOPLEVEL-DAG: topLevel interface  '' ProtoReferencedOnlyInGeneric false
func genericTest<T: ProtoReferencedOnlyInGeneric>(_: T) {}
// CHECK-TOPLEVEL-DAG: topLevel interface  '' ProtoReferencedOnlyInPrivateGeneric false
private func privateGenericTest<T: ProtoReferencedOnlyInPrivateGeneric>(_: T) {}

struct PrivateStoredProperty {
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' TypeReferencedOnlyByPrivateVar false
  private var value: TypeReferencedOnlyByPrivateVar
}
class PrivateStoredPropertyRef {
  // CHECK-TOPLEVEL-DAG: topLevel interface  '' TypeReferencedOnlyByPrivateClassVar false
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



// CHECK-MEMBER-DAG: member interface  4main10IntWrapperV Int false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  SL '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main18ClassFromOtherFileC '' false
// CHECK-MEMBER-DAG: member interface  Si max false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  s25ExpressibleByFloatLiteralP '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  s33ExpressibleByUnicodeScalarLiteralP '' false
// CHECK-MEMBER-DAG: member interface  Sx Stride false
// CHECK-MEMBER-DAG: member interface  Sa reduce false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main17OtherFileIntArrayV '' false
// CHECK-MEMBER-DAG: member interface  4main18OtherFileOuterTypeV InnerType false
// CHECK-MEMBER-DAG: member interface  4main18OtherFileOuterTypeV05InnerE0V init false
// CHECK-MEMBER-DAG: member interface  4main18OtherFileOuterTypeV05InnerE0V sharedConstant false
// CHECK-MEMBER-DAG: member interface  4main26OtherFileSecretTypeWrapperV0dE0V constant false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface 4main25OtherFileProtoImplementorV '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main26OtherFileProtoImplementor2V '' false
// CHECK-MEMBER-DAG: member interface  s15EmptyCollectionV8IteratorV init false
// CHECK-MEMBER-DAG: member interface  4main13OtherFileEnumO Value false
// CHECK-MEMBER-DAG: member interface  4main20OtherFileEnumWrapperV Enum false

// CHECK-MEMBER-DAG: member interface  4main14TopLevelStructV ValueType false
// CHECK-MEMBER-DAG: member interface  4main15TopLevelStruct2V ValueType false
// CHECK-MEMBER-DAG: member interface  4main15TopLevelStruct3V ValueType false
// CHECK-MEMBER-DAG: member interface  4main15TopLevelStruct4V ValueType false
// CHECK-MEMBER-DAG: member interface  4main15TopLevelStruct5V ValueType false
// CHECK-MEMBER-DAG: member interface  4main21PrivateTopLevelStructV ValueType false
// CHECK-MEMBER-DAG: member interface  4main22PrivateTopLevelStruct2V ValueType false
// CHECK-MEMBER-DAG: member interface  4main22PrivateTopLevelStruct3V ValueType false
// CHECK-MEMBER-DAG: member interface  4main22PrivateTopLevelStruct4V ValueType false

// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main14TopLevelProto1P '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main14TopLevelProto2P '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main13PrivateProto1P '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main13PrivateProto2P '' false
// CHECK-POTENTIALMEMBER-DAG: potentialMember interface  4main13PrivateProto3P '' false

// CHECK-NOMINAL-2-DAG: nominal interface  Sa '' false
// CHECK-NOMINAL-2-DAG: nominal interface  Sb '' true
// CHECK-NOMINAL-2-DAG: nominal implementation  Sb '' true
// CHECK-NOMINAL-2-DAG: nominal interface  4main18ClassFromOtherFileC '' false
// CHECK-NOMINAL-2-DAG: nominal interface  SL '' false
// CHECK-NOMINAL-2-DAG: nominal interface  s25ExpressibleByFloatLiteralP '' false
// CHECK-NOMINAL-2-DAG: nominal interface  s33ExpressibleByUnicodeScalarLiteralP '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main18OtherFileOuterTypeV05InnerE0V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  Si '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main13OtherFileEnumO '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main20OtherFileEnumWrapperV '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main17OtherFileIntArrayV '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main18OtherFileOuterTypeV '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main25OtherFileProtoImplementorV '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main26OtherFileProtoImplementor2V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main13PrivateProto1P '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main13PrivateProto2P '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main13PrivateProto3P '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main21PrivateTopLevelStructV '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main22PrivateTopLevelStruct2V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main22PrivateTopLevelStruct3V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main22PrivateTopLevelStruct4V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main26OtherFileSecretTypeWrapperV0dE0V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  Sx '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main14TopLevelProto1P '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main14TopLevelProto2P '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main14TopLevelStructV '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main15TopLevelStruct2V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main15TopLevelStruct3V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main15TopLevelStruct4V '' false
// CHECK-NOMINAL-2-DAG: nominal interface  4main15TopLevelStruct5V '' false

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
// NEGATIVE-NOT: "4main26OtherFileSecretTypeWrapperV"

let eof: () = ()
