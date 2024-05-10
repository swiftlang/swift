// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-non-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-COMMON,CHECK-NONRES < %t/Lib-non-res.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil -O %t/main.swift -o %t/Main-non-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-MAIN-COMMON,CHECK-MAIN-NONRES < %t/Main-non-res.sil

// RUN: rm -rf %t/Lib.swiftmodule

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-COMMON,CHECK-RES < %t/Lib-res.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil -O %t/main.swift -o %t/Main-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-MAIN-COMMON,CHECK-MAIN-RES < %t/Main-res.sil

// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck %s --check-prefix=CHECK-BC
// CHECK-BC: SERIALIZE_PACKAGE_ENABLED

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib

// CHECK-MAIN-COMMON-NOT: s3Lib9PubStructVyACSicfC
// CHECK-MAIN-COMMON-NOT: s3Lib9PubStructV6fooVarSivg
// CHECK-MAIN-COMMON-NOT: s3Lib9PubStructV6fooVarSivs
// CHECK-MAIN-COMMON-NOT: s3Lib6runPubySiAA0C6StructVF
// CHECK-MAIN-COMMON-NOT: s3Lib9PkgStructVyACSicfC
// CHECK-MAIN-COMMON-NOT: s3Lib9PkgStructV6fooVarSivg
// CHECK-MAIN-COMMON-NOT: s3Lib9PkgStructV6fooVarSivs
// CHECK-MAIN-COMMON-NOT: s3Lib6runPkgySiAA0C6StructVF

// CHECK-MAIN-COMMON: [[PUB_INIT:%.*]] = struct $PubStruct
// CHECK-MAIN-COMMON: store [[PUB_INIT]] to {{.*}} : $*PubStruct
// CHECK-MAIN-COMMON: [[PUB_ELEM_ADDR:%.*]] = struct_element_addr {{.*}} : $*PubStruct, #PubStruct.fooVar
// CHECK-MAIN-COMMON: [[PUB_GET:%.*]] = load [[PUB_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON: store [[PUB_GET]] to {{.*}} : $*Int

// CHECK-MAIN-COMMON: [[FRPUB_INIT:%.*]] = struct $FrPubStruct
// CHECK-MAIN-COMMON: store [[FRPUB_INIT]] to {{.*}} : $*FrPubStruct
// CHECK-MAIN-COMMON: [[FRPUB_ELEM_ADDR:%.*]] = struct_element_addr {{.*}} : $*FrPubStruct, #FrPubStruct.fooVar
// CHECK-MAIN-COMMON: [[FRPUB_GET:%.*]] = load [[FRPUB_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON: store [[FRPUB_GET]] to {{.*}} : $*Int

// CHECK-MAIN-COMMON: [[PKG_INIT:%.*]] = struct $PkgStruct
// CHECK-MAIN-COMMON: store [[PKG_INIT]] to {{.*}} : $*PkgStruct
// CHECK-MAIN-COMMON: [[PKG_ELEM_ADDR:%.*]] = struct_element_addr {{.*}} : $*PkgStruct, #PkgStruct.fooVar
// CHECK-MAIN-COMMON: [[PKG_GET:%.*]] = load [[PKG_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON: store [[PKG_GET]] to {{.*}} : $*Int

// CHECK-MAIN-COMMON: [[PUB_ALLOC:%.*]] = alloc_ref $PubKlass
// CHECK-MAIN-COMMON-NEXT: [[PUB_INIT:%.*]] = end_init_let_ref [[PUB_ALLOC]] : $PubKlass
// CHECK-MAIN-COMMON-NEXT: [[PUB_REF_ELEM_ADDR:%.*]] = ref_element_addr [[PUB_INIT]] : $PubKlass, #PubKlass.data
// CHECK-MAIN-COMMON-NEXT: store {{.*}} to [[PUB_REF_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON-NEXT: store [[PUB_INIT]] to {{.*}} : $*PubKlass
  
// CHECK-MAIN-COMMON: [[PUBK_GET:%.*]] = load {{.*}} : $*PubKlass
// CHECK-MAIN-COMMON: class_method [[PUBK_GET]] : $PubKlass, #PubKlass.data!getter : (PubKlass) -> () -> Int, $@convention(method) (@guaranteed PubKlass) -> Int
// CHECK-MAIN-COMMON: [[PUBK_SET:%.*]] = load {{.*}} : $*PubKlass
// CHECK-MAIN-COMMON: class_method [[PUBK_SET]] : $PubKlass, #PubKlass.data!setter : (PubKlass) -> (Int) -> (), $@convention(method) (Int, @guaranteed PubKlass) -> ()
  
// CHECK-MAIN-COMMON: [[PKG_ALLOC:%.*]] = alloc_ref $PkgKlass
// CHECK-MAIN-COMMON-NEXT: [[PKG_INIT:%.*]] = end_init_let_ref [[PKG_ALLOC]] : $PkgKlass
// CHECK-MAIN-COMMON-NEXT: [[PKG_REF_ELEM_ADDR:%.*]] = ref_element_addr [[PKG_INIT]] : $PkgKlass, #PkgKlass.data
// CHECK-MAIN-COMMON-NEXT: store {{.*}} to [[PKG_REF_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON-NEXT: store [[PKG_INIT]] to {{.*}} : $*PkgKlass

// CHECK-MAIN-COMMON: [[PKGK_GET:%.*]] = load {{.*}} : $*PkgKlass
// CHECK-MAIN-COMMON: class_method [[PKGK_GET]] : $PkgKlass, #PkgKlass.data!getter : (PkgKlass) -> () -> Int, $@convention(method) (@guaranteed PkgKlass) -> Int

// CHECK-MAIN-COMMON: [[PKGK_SET:%.*]] = load {{.*}} : $*PkgKlass
// CHECK-MAIN-COMMON: class_method [[PKGK_SET]] : $PkgKlass, #PkgKlass.data!setter : (PkgKlass) -> (Int) -> (), $@convention(method) (Int, @guaranteed PkgKlass) -> ()

// CHECK-MAIN-COMMON: [[FNL_PUB_ALLOC:%.*]] = alloc_ref $FinalPubKlass
// CHECK-MAIN-COMMON-NEXT: [[FNL_PUB_INIT:%.*]] = end_init_let_ref [[FNL_PUB_ALLOC]] : $FinalPubKlass
// CHECK-MAIN-COMMON-NEXT: [[FNL_PUB_REF_ELEM_ADDR:%.*]] = ref_element_addr [[FNL_PUB_INIT]] : $FinalPubKlass, #FinalPubKlass.data
// CHECK-MAIN-COMMON-NEXT: store {{.*}} to [[FNL_PUB_REF_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON: store [[FNL_PUB_INIT]] to {{.*}} : $*FinalPubKlass
  
// CHECK-MAIN-COMMON: [[FNL_PUB_GET:%.*]] = load {{.*}} : $*FinalPubKlass
// CHECK-MAIN-COMMON: [[FNL_PUB_REF:%.*]] = ref_element_addr [[FNL_PUB_GET]] : $FinalPubKlass, #FinalPubKlass.data
// CHECK-MAIN-COMMON-NEXT: [[FNL_PUB_ACCESS:%.*]] = begin_access {{.*}} [[FNL_PUB_REF]] : $*Int
// CHECK-MAIN-COMMON-NEXT: [[FNL_PUB_LOAD:%.*]] = load [[FNL_PUB_ACCESS]] : $*Int
// CHECK-MAIN-COMMON-NEXT: store [[FNL_PUB_LOAD]] to {{.*}} : $*Int
  
// CHECK-MAIN-COMMON: [[FNL_PKG_ALLOC:%.*]] = alloc_ref $FinalPkgKlass
// CHECK-MAIN-COMMON-NEXT: [[FNL_PKG_INIT:%.*]] = end_init_let_ref [[FNL_PKG_ALLOC]] : $FinalPkgKlass
// CHECK-MAIN-COMMON-NEXT: [[FNL_PKG_REF_ELEM_ADDR:%.*]] = ref_element_addr [[FNL_PKG_INIT]] : $FinalPkgKlass, #FinalPkgKlass.data
// CHECK-MAIN-COMMON-NEXT: store {{.*}} to [[FNL_PKG_REF_ELEM_ADDR]] : $*Int
// CHECK-MAIN-COMMON: store [[FNL_PKG_INIT]] to {{.*}} : $*FinalPkgKlass
  
// CHECK-MAIN-COMMON: [[FNL_PKG_GET:%.*]] = load {{.*}} : $*FinalPkgKlass
// CHECK-MAIN-COMMON: [[FNL_PKG_REF:%.*]] = ref_element_addr [[FNL_PKG_GET]] : $FinalPkgKlass, #FinalPkgKlass.data
// CHECK-MAIN-COMMON-NEXT: [[FNL_PKG_ACCESS:%.*]] = begin_access {{.*}} [[FNL_PKG_REF]] : $*Int
// CHECK-MAIN-COMMON-NEXT: [[FNL_PKG_LOAD:%.*]] = load [[FNL_PKG_ACCESS]] : $*Int
// CHECK-MAIN-COMMON-NEXT: store [[FNL_PKG_LOAD]] to {{.*}} : $*Int
  
// CHECK-MAIN-RES-DAG: sil public_external [serialized_for_package] @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass {
// CHECK-MAIN-NONRES-DAG: sil public_external @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass {

// CHECK-MAIN-RES-DAG: sil package_external [serialized_for_package] @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {
// CHECK-MAIN-NONRES-DAG: sil package_external @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {

// CHECK-MAIN-RES-DAG: sil public_external [serialized_for_package] @$s3Lib13FinalPubKlassCyACSicfC : $@convention(method) (Int, @thick FinalPubKlass.Type) -> @owned FinalPubKlass {
// CHECK-MAIN-NONRES-DAG: sil public_external @$s3Lib13FinalPubKlassCyACSicfC : $@convention(method) (Int, @thick FinalPubKlass.Type) -> @owned FinalPubKlass {

// CHECK-MAIN-RES-DAG: sil public_external [serialized_for_package] @$s3Lib8PubKlassC4dataSivs : $@convention(method) (Int, @guaranteed PubKlass) -> () {
// CHECK-MAIN-NONRES-DAG: sil public_external [transparent] @$s3Lib8PubKlassC4dataSivs : $@convention(method) (Int, @guaranteed PubKlass) -> () {

// CHECK-MAIN-RES-DAG: sil package_external [serialized_for_package] @$s3Lib13FinalPkgKlassCyACSicfC : $@convention(method) (Int, @thick FinalPkgKlass.Type) -> @owned FinalPkgKlass {
// CHECK-MAIN-NONRES-DAG: sil package_external @$s3Lib13FinalPkgKlassCyACSicfC : $@convention(method) (Int, @thick FinalPkgKlass.Type) -> @owned FinalPkgKlass {

// CHECK-MAIN-RES-DAG: sil package_external [serialized_for_package] @$s3Lib8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
// CHECK-MAIN-NONRES-DAG: sil package_external [transparent] @$s3Lib8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {

// CHECK-MAIN-COMMON: sil_vtable PubKlass {
// CHECK-MAIN-COMMON: sil_vtable PkgKlass {
// CHECK-MAIN-COMMON: sil_vtable FinalPubKlass {
// CHECK-MAIN-COMMON: sil_vtable FinalPkgKlass {

var pub = PubStruct(1)
let prevPub = pub.fooVar
pub.fooVar = 3
let a = runPub(pub)

var frpub = FrPubStruct(1)
let prevFrPub = frpub.fooVar
frpub.fooVar = 3
let b = runFrPub(frpub)

var pkg = PkgStruct(1)
let prevPkg = pkg.fooVar
pkg.fooVar = 3
let c = runPkg(pkg)

var pubKlass = PubKlass(5)
let prevPubData = pubKlass.data
pubKlass.data = 7
let x = runPubKlass(pubKlass)

var pkgKlass = PkgKlass(7)
let prevPkgData = pkgKlass.data
pkgKlass.data = 7
let y = runPkgKlass(pkgKlass)

var fnlPubKlass = FinalPubKlass(9)
let prevPubDataFnl = fnlPubKlass.data
fnlPubKlass.data = 11

var fnlPkgKlass = FinalPkgKlass(9)
let prevPkgDataFnl = fnlPkgKlass.data
fnlPkgKlass.data = 11

print(a, b, c, x, y, 
      prevPub, prevFrPub, prevPkg, prevPubData,
      prevPkgData, prevPubDataFnl, prevPkgDataFnl)

public func mainPub() {
  print(PubStruct(1))
}

@inlinable
public func mainPubInlinable() {
  print(PubStruct(2))
}

public func mainPubArgRet(_ arg: PubKlass) -> PubStruct {
  return PubStruct(arg.data)
}

@inlinable
public func mainPubArgRetInlinable(_ arg: PubKlass) -> PubStruct {
  return PubStruct(arg.data)
}

//--- Lib.swift

public struct PubStruct {
  // PubStruct.foovar.getter
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (PubStruct) -> Int
  // CHECK-RES-DAG: [[PUB_GET:%.*]] = struct_element_addr {{.*}} : $*PubStruct, #PubStruct.fooVar
  // CHECK-RES-DAG: load [[PUB_GET]] : $*Int
  // CHECK-NONRES-DAG = struct_extract {{.*}} : $PubStruct, #PubStruct.fooVar

  // PubStruct.foovar.setter
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> () {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> () {

  /// NOTE: `struct $PubStruct` in [serialized] function is legal only if package serialization is enabled.
  // CHECK-COMMON-DAG:  [[PUB_SET:%.*]] = struct $PubStruct
  // CHECK-RES-DAG:  store [[PUB_SET]] to {{.*}} : $*PubStruct
  // CHECK-NONRES-DAG:  store [[PUB_SET]] to [trivial] {{.*}} : $*PubStruct

  // PubStruct.foovar.modify
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib9PubStructV6fooVarSivM : $@yield_once @convention(method) (@inout PubStruct) -> @yields @inout Int {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV6fooVarSivM : $@yield_once @convention(method) (@inout PubStruct) -> @yields @inout Int {
  // CHECK-COMMON-DAG: [[PUB_MODIFY:%.*]] = struct_element_addr {{.*}} : $*PubStruct, #PubStruct.fooVar
  // CHECK-COMMON-DAG: yield [[PUB_MODIFY]]
  public var fooVar: Int

  public init(_ arg: Int) {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct {
    // CHECK-COMMON-DAG: [[PUB_INIT:%.*]] = struct $PubStruct
    // CHECK-RES-DAG: store [[PUB_INIT]] to {{.*}} : $*PubStruct
    // CHECK-NONRES-DAG: return [[PUB_INIT]] : $PubStruct
    fooVar = arg
  }
  public func f() -> Int {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib9PubStructV1fSiyF : $@convention(method) (@in_guaranteed PubStruct) -> Int {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib9PubStructV1fSiyF : $@convention(method) (PubStruct) -> Int {
    return fooVar > 7 ? fooVar : fooVar + 11
  }
}

public func runPub(_ arg: PubStruct) -> Int {
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib6runPubySiAA0C6StructVF : $@convention(thin) (@in_guaranteed PubStruct) -> Int {
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib6runPubySiAA0C6StructVF : $@convention(thin) (PubStruct) -> Int {
  return arg.f() > arg.fooVar ? arg.f() : arg.fooVar
}

@inlinable
public func runPubInlinable(_ arg: Int) -> PubStruct {
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib15runPubInlinableyAA0C6StructVSiF : $@convention(thin) (Int) -> @out PubStruct {
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib15runPubInlinableyAA0C6StructVSiF : $@convention(thin) (Int) -> PubStruct {
  // CHECK-RES-DAG: alloc_stack [var_decl] $PubStruct
  // CHECK-RES-DAG: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct
  // CHECK-NONRES-DAG: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct
  var x = PubStruct(1)
  x.fooVar = arg > 11 ? arg + 13 : arg + 17
  return x
}

@frozen
public struct FrPubStruct {
  // FrPubStruct.fooVar.getter
  // CHECK-RES-DAG: sil [transparent] [serialized] [serialized_for_package] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivg : $@convention(method) (FrPubStruct) -> Int {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivg : $@convention(method) (FrPubStruct) -> Int {
  // CHECK-COMMON-DAG: [[FR_GET:%.*]] = struct_extract {{.*}} : $FrPubStruct, #FrPubStruct.fooVar
  // CHECK-COMMON-DAG: return [[FR_GET]] : $Int

  // FrPubStruct.fooVar.setter
  // CHECK-RES-DAG: sil [transparent] [serialized] [serialized_for_package] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivs : $@convention(method) (Int, @inout FrPubStruct) -> () {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivs : $@convention(method) (Int, @inout FrPubStruct) -> () {
  // CHECK-COMMON-DAG:  [[FR_SET:%.*]] = struct $FrPubStruct
  // CHECK-COMMON-DAG:  store [[FR_SET]] to [trivial] {{.*}} : $*FrPubStruct

  // FrPubStruct.fooVar.modify
  // CHECK-RES-DAG: sil [transparent] [serialized] [serialized_for_package] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivM : $@yield_once @convention(method) (@inout FrPubStruct) -> @yields @inout Int {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivM : $@yield_once @convention(method) (@inout FrPubStruct) -> @yields @inout Int {
  // CHECK-COMMON-DAG: [[FIELD:%.*]] = struct_element_addr {{.*}} : $*FrPubStruct, #FrPubStruct.fooVar
  // CHECK-COMMON-DAG: yield [[FIELD]]
  public var fooVar: Int

  public init(_ arg: Int) {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib11FrPubStructVyACSicfC : $@convention(method) (Int, @thin FrPubStruct.Type) -> FrPubStruct {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib11FrPubStructVyACSicfC : $@convention(method) (Int, @thin FrPubStruct.Type) -> FrPubStruct {
    // CHECK-COMMON-DAG: [[FR_MODIFY:%.*]] = struct $FrPubStruct
    // CHECK-COMMON-DAG: return [[FR_MODIFY]] : $FrPubStruct
    fooVar = arg
  }
  public func f() -> Int {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib11FrPubStructV1fSiyF : $@convention(method) (FrPubStruct) -> Int {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib11FrPubStructV1fSiyF : $@convention(method) (FrPubStruct) -> Int {
    return fooVar > 13 ? fooVar : fooVar + 17
  }
}

public func runFrPub(_ arg: FrPubStruct) -> Int {
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8runFrPubySiAA0cD6StructVF : $@convention(thin) (FrPubStruct) -> Int {
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib8runFrPubySiAA0cD6StructVF : $@convention(thin) (FrPubStruct) -> Int {
  return arg.f() > arg.fooVar ? arg.f() : arg.fooVar
}

package struct PkgStruct {
  // PkgStruct.fooVar.getter
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (PkgStruct) -> Int {
  // CHECK-COMMON-DAG: [[PKG_GET:%.*]] = struct_element_addr {{.*}} : $*PkgStruct, #PkgStruct.fooVar
  // CHECK-RES-DAG: load [[PKG_GET]] : $*Int
  // CHECK-NONRES-DAG = struct_extract {{.*}} : $PkgStruct, #PkgStruct.fooVar

  // PkgStruct.fooVar.setter
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> () {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> () {
  // CHECK-COMMON-DAG:  [[PKG_SET:%.*]] = struct $PkgStruct
  // CHECK-RES-DAG:  store [[PKG_SET]] to {{.*}} : $*PkgStruct
  // CHECK-NONRES-DAG:  store [[PKG_SET]] to [trivial] {{.*}} : $*PkgStruct

  // PkgStruct.fooVar.modify
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib9PkgStructV6fooVarSivM : $@yield_once @convention(method) (@inout PkgStruct) -> @yields @inout Int {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV6fooVarSivM : $@yield_once @convention(method) (@inout PkgStruct) -> @yields @inout Int {
  package var fooVar: Int

  package init(_ arg: Int) {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> @out PkgStruct {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> PkgStruct {
    // CHECK-COMMON-DAG: [[PKG_INIT:%.*]] = struct $PkgStruct
    // CHECK-RES-DAG: store [[PKG_INIT]] to {{.*}} : $*PkgStruct
    // CHECK-NONRES-DAG: return [[PKG_INIT]] : $PkgStruct
    fooVar = arg
  }
  package func f() -> Int {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib9PkgStructV1fSiyF : $@convention(method) (@in_guaranteed PkgStruct) -> Int {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib9PkgStructV1fSiyF : $@convention(method) (PkgStruct) -> Int {
    return fooVar > 19 ? fooVar : fooVar + 23
  }
}

package func runPkg(_ arg: PkgStruct) -> Int {
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib6runPkgySiAA0C6StructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> Int {
  // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib6runPkgySiAA0C6StructVF : $@convention(thin) (PkgStruct) -> Int {
  return arg.f() > arg.fooVar ? arg.f() : arg.fooVar
}

public protocol PubProto {
  var data: Int { get set }
  func pubfunc(_ arg: Int) -> Int
}

public class PubKlass: PubProto {
  // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PubProto) (@in_guaranteed PubKlass) -> Int {
  // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PubProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PubKlass> {
  // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PubProto) (Int, @inout PubKlass) -> () {
  // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PubProto) (@in_guaranteed PubKlass) -> Int {
  // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PubProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PubKlass> {
  // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PubProto) (Int, @inout PubKlass) -> () {
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8PubKlassC4dataSivg : $@convention(method) (@guaranteed PubKlass) -> Int
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib8PubKlassC4dataSivg : $@convention(method) (@guaranteed PubKlass) -> Int {
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8PubKlassC4dataSivs : $@convention(method) (Int, @guaranteed PubKlass) -> () {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib8PubKlassC4dataSivs : $@convention(method) (Int, @guaranteed PubKlass) -> () {
  public var data: Int
  public init(_ arg: Int = 1) {
    // FIXME: default argument 0 of PubKlass.init(_:) gets non_abi so is not considered
    // for package serialization, but should it?
    // sil non_abi [serialized] [canonical] @$s3Lib8PubKlassCyACSicfcfA_ : $@convention(thin) () -> Int {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8PubKlassCyACSicfc : $@convention(method) (Int, @owned PubKlass) -> @owned PubKlass {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [exact_self_class] [canonical] @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8PubKlassCfD : $@convention(method) (@owned PubKlass) -> () {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8PubKlassCfd : $@convention(method) (@guaranteed PubKlass) -> @owned Builtin.NativeObject {
    // default argument 0 of PubKlass.init(_:)
    // CHECK-NONRES-DAG: sil non_abi [serialized] [canonical] @$s3Lib8PubKlassCyACSicfcfA_ : $@convention(thin) () -> Int {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCyACSicfc : $@convention(method) (Int, @owned PubKlass) -> @owned PubKlass {
    // CHECK-NONRES-DAG: sil [serialized] [exact_self_class] [canonical] @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCfD : $@convention(method) (@owned PubKlass) -> () {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCfd : $@convention(method) (@guaranteed PubKlass) -> @owned Builtin.NativeObject {
    self.data = arg
  }
  public func pubfunc(_ arg: Int) -> Int {
    // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP7pubfuncyS2iFTW : $@convention(witness_method: PubProto) (Int, @in_guaranteed PubKlass) -> Int {
    // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib8PubKlassC7pubfuncyS2iF : $@convention(method) (Int, @guaranteed PubKlass) -> Int {
    // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP7pubfuncyS2iFTW : $@convention(witness_method: PubProto) (Int, @in_guaranteed PubKlass) -> Int {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassC7pubfuncyS2iF : $@convention(method) (Int, @guaranteed PubKlass) -> Int {
    return data + arg
  }
}

public func runPubKlass(_ arg: PubKlass) -> Int {
  // CHECK-RES-DAG: sil [serialized] [serialized_for_package] [canonical] @$s3Lib11runPubKlassySiAA0cD0CF : $@convention(thin) (@guaranteed PubKlass) -> Int
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib11runPubKlassySiAA0cD0CF : $@convention(thin) (@guaranteed PubKlass) -> Int {
  arg.data += 29
  return arg.pubfunc(31)
}

final public class FinalPubKlass {
  public var data = 1
  public init(_ arg: Int) {
    data = arg
  }
  public func fnlPubFunc(_ arg: Int) -> Int {
    data + arg
  }
}

package protocol PkgProto {
  var data: Int { get set }
  func pkgfunc(_ arg: Int) -> Int
}

package class PkgKlass: PkgProto {
  // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlass) -> Int {
  // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PkgProto) (Int, @inout PkgKlass) -> () {
  // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PkgProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PkgKlass> {
  // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlass) -> Int {
  // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PkgProto) (Int, @inout PkgKlass) -> () {
  // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PkgProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PkgKlass> {
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
  package var data: Int

  package init(_ arg: Int = 1) {
    // FIXME: package -> package_non_abi for default argument 0 of PkgKlass.init(_:)
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassCyACSicfcfA_ : $@convention(thin) () -> Int {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassCyACSicfc : $@convention(method) (Int, @owned PkgKlass) -> @owned PkgKlass {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [exact_self_class] [canonical] @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassCfd : $@convention(method) (@guaranteed PkgKlass) -> @owned Builtin.NativeObject {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassCfD : $@convention(method) (@owned PkgKlass) -> ()
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCyACSicfcfA_ : $@convention(thin) () -> Int {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCyACSicfc : $@convention(method) (Int, @owned PkgKlass) -> @owned PkgKlass {
    // CHECK-NONRES-DAG: sil package [serialized] [exact_self_class] [canonical] @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCfd : $@convention(method) (@guaranteed PkgKlass) -> @owned Builtin.NativeObject {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCfD : $@convention(method) (@owned PkgKlass) -> ()
    self.data = arg
  }

  package func pkgfunc(_ arg: Int) -> Int {
    // CHECK-RES-DAG: sil shared [transparent] [serialized] [serialized_for_package] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW : $@convention(witness_method: PkgProto) (Int, @in_guaranteed PkgKlass) -> Int {
    // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib8PkgKlassC7pkgfuncyS2iF : $@convention(method) (Int, @guaranteed PkgKlass) -> Int {
    // CHECK-NONRES-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW : $@convention(witness_method: PkgProto) (Int, @in_guaranteed PkgKlass) -> Int {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC7pkgfuncyS2iF : $@convention(method) (Int, @guaranteed PkgKlass) -> Int {
    return data + arg
  }
}

package func runPkgKlass(_ arg: PkgKlass) -> Int {
  // CHECK-RES-DAG: sil package [serialized] [serialized_for_package] [canonical] @$s3Lib11runPkgKlassySiAA0cD0CF : $@convention(thin) (@guaranteed PkgKlass) -> Int
  // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib11runPkgKlassySiAA0cD0CF : $@convention(thin) (@guaranteed PkgKlass) -> Int {
  arg.data += 37
  return arg.pkgfunc(41)
}


final package class FinalPkgKlass {
  package var data = 1
  package init(_ arg: Int) {
    data = arg
  }
  package func fnlPkgFunc(_ arg: Int) -> Int {
    data + arg
  }
}

// CHECK-COMMON-LABEL: sil_vtable [serialized] PubKlass {
// CHECK-COMMON-NEXT:   #PubKlass.data!getter: (PubKlass) -> () -> Int : @$s3Lib8PubKlassC4dataSivg
// CHECK-COMMON-NEXT:   #PubKlass.data!setter: (PubKlass) -> (Int) -> () : @$s3Lib8PubKlassC4dataSivs
// CHECK-COMMON-NEXT:   #PubKlass.data!modify: (PubKlass) -> () -> () : @$s3Lib8PubKlassC4dataSivM
// CHECK-COMMON-NEXT:   #PubKlass.init!allocator: (PubKlass.Type) -> (Int) -> PubKlass : @$s3Lib8PubKlassCyACSicfC
// CHECK-COMMON-NEXT:   #PubKlass.pubfunc: (PubKlass) -> (Int) -> Int : @$s3Lib8PubKlassC7pubfuncyS2iF
// CHECK-COMMON-NEXT:   #PubKlass.deinit!deallocator: @$s3Lib8PubKlassCfD

// CHECK-COMMON-LABEL: sil_vtable [serialized] FinalPubKlass {
// CHECK-COMMON-NEXT:  #FinalPubKlass.init!allocator: (FinalPubKlass.Type) -> (Int) -> FinalPubKlass : @$s3Lib13FinalPubKlassCyACSicfC
// CHECK-COMMON-NEXT:  #FinalPubKlass.deinit!deallocator: @$s3Lib13FinalPubKlassCfD

// CHECK-COMMON-LABEL: sil_vtable [serialized] PkgKlass {
// CHECK-COMMON-NEXT:   #PkgKlass.data!getter: (PkgKlass) -> () -> Int : @$s3Lib8PkgKlassC4dataSivg
// CHECK-COMMON-NEXT:   #PkgKlass.data!setter: (PkgKlass) -> (Int) -> () : @$s3Lib8PkgKlassC4dataSivs
// CHECK-COMMON-NEXT:   #PkgKlass.data!modify: (PkgKlass) -> () -> () : @$s3Lib8PkgKlassC4dataSivM
// CHECK-COMMON-NEXT:   #PkgKlass.init!allocator: (PkgKlass.Type) -> (Int) -> PkgKlass : @$s3Lib8PkgKlassCyACSicfC
// CHECK-COMMON-NEXT:   #PkgKlass.pkgfunc: (PkgKlass) -> (Int) -> Int : @$s3Lib8PkgKlassC7pkgfuncyS2iF
// CHECK-COMMON-NEXT:   #PkgKlass.deinit!deallocator: @$s3Lib8PkgKlassCfD

// CHECK-COMMON-LABEL: sil_vtable [serialized] FinalPkgKlass {
// CHECK-COMMON-NEXT:  #FinalPkgKlass.init!allocator: (FinalPkgKlass.Type) -> (Int) -> FinalPkgKlass : @$s3Lib13FinalPkgKlassCyACSicfC
// CHECK-COMMON-NEXT:  #FinalPkgKlass.deinit!deallocator: @$s3Lib13FinalPkgKlassCfD

// CHECK-COMMON-LABEL: sil_witness_table [serialized] PubKlass: PubProto module Lib {
// CHECK-COMMON-NEXT:   method #PubProto.data!getter: <Self where Self : PubProto> (Self) -> () -> Int : @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivgTW
// CHECK-COMMON-NEXT:   method #PubProto.data!setter: <Self where Self : PubProto> (inout Self) -> (Int) -> () : @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivsTW
// CHECK-COMMON-NEXT:   method #PubProto.data!modify: <Self where Self : PubProto> (inout Self) -> () -> () : @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivMTW
// CHECK-COMMON-NEXT:   method #PubProto.pubfunc: <Self where Self : PubProto> (Self) -> (Int) -> Int : @$s3Lib8PubKlassCAA0B5ProtoA2aDP7pubfuncyS2iFTW

// CHECK-COMMON-LABEL: sil_witness_table package [serialized] PkgKlass: PkgProto module Lib {
// CHECK-COMMON-NEXT:   method #PkgProto.data!getter: <Self where Self : PkgProto> (Self) -> () -> Int : @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW
// CHECK-COMMON-NEXT:   method #PkgProto.data!setter: <Self where Self : PkgProto> (inout Self) -> (Int) -> () : @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW
// CHECK-COMMON-NEXT:   method #PkgProto.data!modify: <Self where Self : PkgProto> (inout Self) -> () -> () : @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivMTW
// CHECK-COMMON-NEXT:   method #PkgProto.pkgfunc: <Self where Self : PkgProto> (Self) -> (Int) -> Int : @$s3Lib8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW
