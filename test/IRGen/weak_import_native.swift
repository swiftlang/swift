// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_import_native_helper.swiftmodule %S/Inputs/weak_import_native_helper.swift -enable-resilience
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir | %FileCheck %s

import weak_import_native_helper

func testTopLevel() {
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper2fnyyF"()
  fn()

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper12globalStoredSivg"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper12globalStoredSivs"
  let x = globalStored
  globalStored = x
  globalStored += 1

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper14globalComputedSivg"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper14globalComputedSivs"
  let y = globalComputed
  globalComputed = y
  globalComputed += 1
}

func testStruct() {
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVACycfC"
  var s = S()

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV2fnyyF"
  s.fn()

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV10storedPropSivg"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV10storedPropSivs"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV10storedPropSivM"
  let x = s.storedProp
  s.storedProp = x
  s.storedProp += 1

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV12computedPropSivg"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV12computedPropSivs"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SV12computedPropSivM"
  let y = s.computedProp
  s.computedProp = y
  s.computedProp += 1

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVyS2icig"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVyS2icis"
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1SVyS2iciM"
  let z = s[0]
  s[0] = z
  s[0] += 1
}

func testEnum() {
  // FIXME: We're still referencing tags directly.
  _ = E.strong
  _ = E.weak
  _ = E.strongAssoc(0)
  _ = E.weakAssoc(0)
}

func testClass() {
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1CCACycfC"
  let c = C()

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1CC2fnyyFTj"
  c.fn()

  // FIXME: vtable dispatch functions for accessors should also be weak

  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CC10storedPropSivgTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CC10storedPropSivsTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CC10storedPropSivMTj"
  let x = c.storedProp
  c.storedProp = x
  c.storedProp += 1

  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CC12computedPropSivgTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CC12computedPropSivsTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CC12computedPropSivMTj"
  let y = c.computedProp
  c.computedProp = y
  c.computedProp += 1

  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CCyS2icigTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CCyS2icisTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CCyS2iciMTj"
  let z = c[0]
  c[0] = z
  c[0] += 1
}

class Sub : C {
  deinit {
    // This is correctly a strong symbol reference; the class is not declared 
    // weak.
    // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1CCfd"
  }
}

func testProtocolExistential(_ p: P) {
  var mutP = p

  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1PP2fnyyFTj"
  p.fn()

  // FIXME: witness table dispatch functions for accessors should also be weak

  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1PP4propSivgTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1PP4propSivsTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1PP4propSivMTj"
  let x = p.prop
  mutP.prop = x
  mutP.prop += 1

  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1PPyS2icigTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1PPyS2icisTj"
  // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper1PPyS2iciMTj"
  let z = p[0]
  mutP[0] = z
  mutP[0] += 1
}

func testProtocolGeneric<Impl: P>(_ type: Impl.Type) {
  // CHECK-DAG: declare extern_weak {{.+}} @"$s25weak_import_native_helper1PPxycfCTj"
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
  // FIXME: These should be weak.
  // CHECK-DAG: declare swiftcc %swift.metadata_response @"$s25weak_import_native_helper5WeakSVMa"
  // CHECK-DAG: declare swiftcc %swift.metadata_response @"$s25weak_import_native_helper5WeakEOMa"
  // CHECK-DAG: declare swiftcc %swift.metadata_response @"$s25weak_import_native_helper5WeakCCMa"
  // CHECK-DAG: @"$s25weak_import_native_helper5WeakPMp" = extern_weak global %swift.protocol
  // CHECK-DAG: declare swiftcc %swift.metadata_response @"$s25weak_import_native_helper8GenericSVMa"
  // CHECK-DAG: declare swiftcc %swift.metadata_response @"$s25weak_import_native_helper8GenericEOMa"
  // CHECK-DAG: declare swiftcc %swift.metadata_response @"$s25weak_import_native_helper8GenericCCMa"
  return [WeakS.self, WeakE.self, WeakC.self, WeakP.self, GenericS<Int>.self, GenericE<Int>.self, GenericC<Int>.self]
}

class WeakSub: WeakC {
  deinit {
    // FIXME: This should be weak.
    // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper5WeakCCfd"
  }
}

class WeakGenericSub: GenericC<Int> {
  deinit {
    // FIXME: This should be weak.
    // CHECK-DAG: declare swiftcc {{.+}} @"$s25weak_import_native_helper8GenericCCfd"
  }
}
