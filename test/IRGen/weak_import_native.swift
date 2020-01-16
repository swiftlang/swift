// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_import_native_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_native_helper.swift -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

import weak_import_native_helper

// CHECK-DAG-LABEL: @"$s25weak_import_native_helper23ProtocolWithWeakMembersP1TAC_AA05OtherE0Tn" = extern_weak global %swift.protocol_requirement
// CHECK-DAG-LABEL: @"$s1T25weak_import_native_helper23ProtocolWithWeakMembersPTl" = extern_weak global %swift.protocol_requirement
// CHECK-DAG-LABEL: @"$s25weak_import_native_helper23ProtocolWithWeakMembersP1fyyFTq" = extern_weak global %swift.method_descriptor
// CHECK-DAG-LABEL: declare extern_weak swiftcc void @"$s25weak_import_native_helper23ProtocolWithWeakMembersPAAE1fyyF"(%swift.type*, i8**, %swift.opaque* noalias nocapture swiftself)
struct ConformsToProtocolWithWeakMembers : ProtocolWithWeakMembers {}

func testTopLevel() {
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper2fnyyF"()
  fn()

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper12globalStoredSivg"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper12globalStoredSivs"
  let x = globalStored
  globalStored = x
  globalStored += 1

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper14globalComputedSivg"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper14globalComputedSivs"
  let y = globalComputed
  globalComputed = y
  globalComputed += 1
}

func testStruct() {
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVACycfC"
  var s = S()

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV2fnyyF"
  s.fn()

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV10storedPropSivg"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV10storedPropSivs"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV10storedPropSivM"
  let x = s.storedProp
  s.storedProp = x
  s.storedProp += 1

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV12computedPropSivg"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV12computedPropSivs"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV12computedPropSivM"
  let y = s.computedProp
  s.computedProp = y
  s.computedProp += 1

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVyS2icig"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVyS2icis"
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVyS2iciM"
  let z = s[0]
  s[0] = z
  s[0] += 1

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper5WeakSV0A6MemberyyF"
  let w = WeakS()
  w.weakMember()
}

func testEnum() {
  // CHECK-DAG-LABEL: @"$s25weak_import_native_helper1EO6strongyA2CmFWC" = external constant i32
  _ = E.strong

  // CHECK-DAG-LABEL: @"$s25weak_import_native_helper1EO0A0yA2CmFWC" = extern_weak constant i32
  _ = E.weak

  // CHECK-DAG-LABEL: @"$s25weak_import_native_helper1EO11strongAssocyACSicACmFWC" = external constant i32
  _ = E.strongAssoc(0)

  // CHECK-DAG-LABEL: @"$s25weak_import_native_helper1EO0A5AssocyACSicACmFWC" = extern_weak constant i32
  _ = E.weakAssoc(0)
}

func testClass() {
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1CCACycfC"
  let c = C()

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1CC2fnyyFTj"
  c.fn()

  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CC10storedPropSivgTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CC10storedPropSivsTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CC10storedPropSivMTj"
  let x = c.storedProp
  c.storedProp = x
  c.storedProp += 1

  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CC12computedPropSivgTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CC12computedPropSivsTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CC12computedPropSivMTj"
  let y = c.computedProp
  c.computedProp = y
  c.computedProp += 1

  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CCyS2icigTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CCyS2icisTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1CCyS2iciMTj"
  let z = c[0]
  c[0] = z
  c[0] += 1
}

class Sub : C {
  deinit {
    // This is correctly a strong symbol reference; the class is not declared 
    // weak.
    // CHECK-DAG-LABEL: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CCfd"
  }
}

func testProtocolExistential(_ p: P) {
  var mutP = p

  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1PP2fnyyFTj"
  p.fn()

  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1PP4propSivgTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1PP4propSivsTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1PP4propSivMTj"
  let x = p.prop
  mutP.prop = x
  mutP.prop += 1

  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1PPyS2icigTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1PPyS2icisTj"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper1PPyS2iciMTj"
  let z = p[0]
  mutP[0] = z
  mutP[0] += 1
}

func testProtocolGeneric<Impl: P>(_ type: Impl.Type) {
  // CHECK-DAG-LABEL: declare extern_weak {{.+}} @"$s25weak_import_native_helper1PPxycfCTj"
  var mutP = type.init()

  mutP.fn()

  let x = mutP.prop
  mutP.prop = x
  mutP.prop += 1

  let z = mutP[0]
  mutP[0] = z
  mutP[0] += 1
}

func testWeakTypes() -> [Any.Type] {
  // CHECK-DAG-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s25weak_import_native_helper5WeakSVMa"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s25weak_import_native_helper5WeakEOMa"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s25weak_import_native_helper5WeakCCMa"
  // CHECK-DAG-LABEL: @"$s25weak_import_native_helper5WeakPMp" = extern_weak global %swift.protocol
  // CHECK-DAG-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s25weak_import_native_helper8GenericSVMa"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s25weak_import_native_helper8GenericEOMa"
  // CHECK-DAG-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s25weak_import_native_helper8GenericCCMa"
  return [WeakS.self, WeakE.self, WeakC.self, WeakP.self, GenericS<Int>.self, GenericE<Int>.self, GenericC<Int>.self]
}

class WeakSub: WeakC {
  deinit {
    // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper5WeakCCfd"
  }
}

class WeakGenericSub: GenericC<Int> {
  deinit {
    // CHECK-DAG-LABEL: declare extern_weak swiftcc {{.+}} @"$s25weak_import_native_helper8GenericCCfd"
  }
}

protocol RefinesP : BaseP {}

// CHECK-DAG-LABEL: @"$s25weak_import_native_helper1SVAA5BasePAAWP" = extern_weak global i8*
extension S : RefinesP {}

// We should not hoist the metadata accessor accross the version check.

// CHECK-LABEL: define{{.*}} void @"$s18weak_import_native28test_not_hoist_weakly_linkedyyF"()
// CHECK-NOT: 15ResilientStructVMa
// CHECK: getVersion
// CHECK: br
// CHECK: 15ResilientStructVMa
// CHECK: ret

public func test_not_hoist_weakly_linked() {
  if getVersion() == 1 {
     var _ = ResilientStruct()
  }
}

// CHECK-LABEL: define{{.*}} void @"$s18weak_import_native29test_not_hoist_weakly_linked2yyF"()
// CHECK-NOT: 15ResilientStructVMa
// CHECK: getVersion
// CHECK: br
// CHECK: 15ResilientStructVMa
// CHECK: ret
public func test_not_hoist_weakly_linked2() {
  if getVersion() == 1 {
     var _ = (ResilientStruct(), 1)
  }
}

struct One<T> {
  var elt : T?
}

// CHECK-LABEL: define{{.*}} void @"$s18weak_import_native29test_not_hoist_weakly_linked3yyF"()
// CHECK-NOT: 15ResilientStructVMa
// CHECK: getVersion
// CHECK: br
// CHECK: 15ResilientStructVMa
// CHECK: ret
public func test_not_hoist_weakly_linked3() {
  if getVersion() == 1 {
     var _ = One(elt:ResilientStruct())
  }
}

// CHECK-LABEL: define{{.*}} void @"$s18weak_import_native29test_not_hoist_weakly_linked4yyF"()
// CHECK-NOT: 15ResilientStructVMa
// CHECK: getVersion
// CHECK: br
// CHECK: 15ResilientStructVMa
// CHECK: ret
public func test_not_hoist_weakly_linked4() {
  if getVersion() == 1 {
     var _ = One(elt:(ResilientStruct(), 1))
  }
}

// CHECK-LABEL: define{{.*}} @"$s18weak_import_native29test_weakly_linked_enum_cases1eSi0a1_b1_C7_helper1EO_t
// CHECK:  [[TAG:%.*]] = call i32 %getEnumTag(
// CHECK:  [[STRONG_CASE:%.*]] = load i32, i32* @"$s25weak_import_native_helper1EO6strongyA2CmFWC"
// CHECK:  [[IS_STRONG:%.*]] = icmp eq i32 [[TAG]], [[STRONG_CASE]]
// CHECK:  br i1 [[IS_STRONG]], label %[[BB0:[0-9]+]], label %[[BB1:[0-9]+]]
//
// CHECK:  [[BB1]]:
// CHECK:  br i1 icmp eq ({{.*}} ptrtoint (i32* @"$s25weak_import_native_helper1EO0A0yA2CmFWC" to {{.*}}), {{.*}} 0), label %[[BB2:[0-9]+]], label %[[BB3:[0-9]+]]
//
// CHECK:  [[BB3]]:
// CHECK:  [[WEAK_CASE:%.*]] = load i32, i32* @"$s25weak_import_native_helper1EO0A0yA2CmFWC"
// CHECK:  [[IS_WEAK:%.*]] = icmp eq i32 [[TAG]], [[WEAK_CASE]]
// CHECK:  br label %[[BB2]]
//
// CHECK:  [[BB2]]:
// CHECK:  = phi i1 [ false, %[[BB1]] ], [ [[IS_WEAK]], %[[BB3]] ]
public func test_weakly_linked_enum_cases(e: E) -> Int {
  switch e {
    case .strong:
      return 1
    case .weak:
      return 2
    default:
      return 3
  }
}
