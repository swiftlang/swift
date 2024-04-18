// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-non-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-COMMON,CHECK-NONRES < %t/Lib-non-res.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil %t/main.swift -o %t/Main-non-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-MAIN-COMMON,CHECK-MAIN-NONRES < %t/Main-non-res.sil

// RUN: rm -rf %t/Lib.swiftmodule

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-COMMON,CHECK-RES < %t/Lib-res.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil %t/main.swift -o %t/Main-res.sil
// RUN: %FileCheck %s --check-prefixes=CHECK-MAIN-COMMON,CHECK-MAIN-RES < %t/Main-res.sil

// RUN: llvm-bcanalyzer --dump %t/Lib.swiftmodule | %FileCheck %s --check-prefix=CHECK-BC
// CHECK-BC: SERIALIZE_PACKAGE_ENABLED

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib

// CHECK-MAIN-COMMON-NOT: s3Lib8PubKlassC4dataSivg
// CHECK-MAIN-COMMON-NOT: s3Lib8PubKlassC4dataSivs
// CHECK-MAIN-COMMON-NOT: s3Lib8PkgKlassC4dataSivg
// CHECK-MAIN-COMMON-NOT: s3Lib8PkgKlassC4dataSivs
// CHECK-MAIN-NONRES-NOT: s3Lib9PubStructV6fooVarSivg
// CHECK-MAIN-NONRES-NOT: s3Lib9PubStructV6fooVarSivs
// CHECK-MAIN-NONRES-NOT: s3Lib9PkgStructV6fooVarSivg
// CHECK-MAIN-NONRES-NOT: s3Lib9PubStructV6fooVarSivs

// CHECK-MAIN-RES: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct
// CHECK-MAIN-NONRES: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct

// CHECK-MAIN-RES: function_ref @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int
// CHECK-MAIN-NONRES: [[PUB_GET:%.*]] = begin_access [read] [dynamic] {{.*}} : $*PubStruct
// CHECK-MAIN-NONRES: struct_element_addr [[PUB_GET]] : $*PubStruct, #PubStruct.fooVar

// CHECK-MAIN-RES: function_ref @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> ()
// CHECK-MAIN-NONRES: [[PUB_MODIFY:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*PubStruct
// CHECK-MAIN-NONRES: struct_element_addr [[PUB_MODIFY]] : $*PubStruct, #PubStruct.fooVar

// CHECK-MAIN-RES: function_ref @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PubStruct) -> ()
// CHECK-MAIN-NONRES: function_ref @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (PubStruct) -> ()

// CHECK-MAIN-COMMON: function_ref @$s3Lib11FrPubStructVyACSicfC : $@convention(method) (Int, @thin FrPubStruct.Type) -> FrPubStruct
// CHECK-MAIN-COMMON: begin_access [read] [dynamic] {{.*}} : $*FrPubStruct
// CHECK-MAIN-COMMON-NEXT: struct_element_addr {{.*}} : $*FrPubStruct, #FrPubStruct.fooVar
// CHECK-MAIN-COMMON-NEXT: copy_addr
// CHECK-MAIN-COMMON: begin_access [modify] [dynamic] {{.*}} : $*FrPubStruct
// CHECK-MAIN-COMMON-NEXT: struct_element_addr {{.*}} : $*FrPubStruct, #FrPubStruct.fooVar
// CHECK-MAIN-COMMON-NEXT: store
// CHECK-MAIN-COMMON: begin_access [read] [dynamic] {{.*}} : $*FrPubStruct
// CHECK-MAIN-COMMON-NEXT: load
// CHECK-MAIN-COMMON: function_ref @$s3Lib8runFrPubyyAA0cD6StructVF : $@convention(thin) (FrPubStruct) -> ()

// CHECK-MAIN-RES: function_ref @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int
// CHECK-MAIN-NONRES: [[PKG_GET:%.*]] = begin_access [read] [dynamic] {{.*}} : $*PkgStruct
// CHECK-MAIN-NONRES: struct_element_addr [[PKG_GET]] : $*PkgStruct, #PkgStruct.fooVar

// CHECK-MAIN-RES: function_ref @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> ()
// CHECK-MAIN-NONRES: [[PKG_MODIFY:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*PkgStruct
// CHECK-MAIN-NONRES: struct_element_addr [[PKG_MODIFY]] : $*PkgStruct, #PkgStruct.fooVar

// CHECK-MAIN-RES: function_ref @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> ()
// CHECK-MAIN-NONRES: function_ref @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (PkgStruct) -> ()

// CHECK-MAIN-COMMON: function_ref @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass
// CHECK-MAIN-COMMON: begin_access [read] [dynamic] {{.*}} : $*PubKlass
// CHECK-MAIN-COMMON: class_method {{.*}} : $PubKlass, #PubKlass.data!getter : (PubKlass) -> () -> Int, $@convention(method) (@guaranteed PubKlass) -> Int
// CHECK-MAIN-COMMON: class_method {{.*}} : $PubKlass, #PubKlass.data!setter : (PubKlass) -> (Int) -> (), $@convention(method) (Int, @guaranteed PubKlass) -> ()
// CHECK-MAIN-COMMON: function_ref @$s3Lib11runPubKlassyyAA0cD0CF : $@convention(thin) (@guaranteed PubKlass) -> ()

// CHECK-MAIN-COMMON: function_ref @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass
// CHECK-MAIN-COMMON: begin_access [read] [dynamic] {{.*}} : $*PkgKlass
// CHECK-MAIN-COMMON: class_method {{.*}} : $PkgKlass, #PkgKlass.data!getter : (PkgKlass) -> () -> Int, $@convention(method) (@guaranteed PkgKlass) -> Int
// CHECK-MAIN-COMMON: class_method {{.*}} : $PkgKlass, #PkgKlass.data!setter : (PkgKlass) -> (Int) -> (), $@convention(method) (Int, @guaranteed PkgKlass) -> ()
// CHECK-MAIN-COMMON: function_ref @$s3Lib11runPkgKlassyyAA0cD0CF : $@convention(thin) (@guaranteed PkgKlass) -> ()

var pub = PubStruct(1)
let prevPub = pub.fooVar
pub.fooVar = 3
runPub(pub)
print(prevPub)

// PubStruct.init(_:)
// CHECK-MAIN-RES:    sil @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct
// CHECK-MAIN-NONRES: sil @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct

// PubStruct.fooVar.getter
// CHECK-MAIN-RES: sil @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int

// PubStruct.fooVar.setter
// CHECK-MAIN-RES: sil @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> ()

// runPub(_:)
// CHECK-MAIN-RES:    sil @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PubStruct) -> ()
// CHECK-MAIN-NONRES: sil @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (PubStruct) -> ()

var frpub = FrPubStruct(1)
let prevFrPub = frpub.fooVar
frpub.fooVar = 3
runFrPub(frpub)
print(prevFrPub)

// FrPubStruct.init(_:)
// CHECK-MAIN-COMMON: sil @$s3Lib11FrPubStructVyACSicfC : $@convention(method) (Int, @thin FrPubStruct.Type) -> FrPubStruct

// runFrPub(_:)
// CHECK-MAIN-COMMON: sil @$s3Lib8runFrPubyyAA0cD6StructVF : $@convention(thin) (FrPubStruct) -> ()

var pkg = PkgStruct(1)
let prevPkg = pkg.fooVar
pkg.fooVar = 3
runPkg(pkg)
print(prevPkg)

// PkgStruct.init(_:)
// CHECK-MAIN-RES:    sil package_external @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> @out PkgStruct
// CHECK-MAIN-NONRES: sil package_external @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> PkgStruct

// PkgStruct.fooVar.getter
// CHECK-MAIN-RES: sil package_external @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int

// PkgStruct.fooVar.setter
// CHECK-MAIN-RES: sil package_external @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> ()

// runPkg(_:)
// CHECK-MAIN-RES:    sil package_external @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> ()
// CHECK-MAIN-NONRES: sil package_external @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (PkgStruct) -> ()

var pubKlass = PubKlass(2)
let prevPubData = pubKlass.data
pubKlass.data = 5
runPubKlass(pubKlass)
print(prevPubData)

// PubKlass.__allocating_init(_:)
// CHECK-MAIN-COMMON: sil @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass

// runPubKlass(_:)
// CHECK-MAIN-COMMON: sil @$s3Lib11runPubKlassyyAA0cD0CF : $@convention(thin) (@guaranteed PubKlass) -> ()

var pkgKlass = PkgKlass(2)
let prevPkgData = pkgKlass.data
pkgKlass.data = 5
runPkgKlass(pkgKlass)
print(prevPkgData)

// PkgKlass.__allocating_init(_:)
// CHECK-MAIN-COMMON: sil package_external @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass

// runPkgKlass(_:)
// CHECK-MAIN-COMMON: sil package_external @$s3Lib11runPkgKlassyyAA0cD0CF : $@convention(thin) (@guaranteed PkgKlass) -> ()


//--- Lib.swift

// FIXME: handle struct_element_addr %field in resilient mode; requires non-resilience in SIL verify.
// CHECK-RES-NOT: s3Lib9PubStructV6fooVarSivg
// CHECK-RES-NOT: s3Lib9PkgStructV6fooVarSivg

// FIXME: handle `struct $PubStruct` in resilient mode; PubStruct is by-address, so fails in IsLodableOrOpaque check.
// CHECK-RES-NOT: s3Lib9PubStructV6fooVarSivs
// CHECK-RES-NOT: s3Lib9PkgStructV6fooVarSivs

public struct PubStruct {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (PubStruct) -> Int
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV6fooVarSivM : $@yield_once @convention(method) (@inout PubStruct) -> @yields @inout Int {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> () {
  public var fooVar: Int

  public init(_ arg: Int) {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct {
    fooVar = arg
  }
  public func f() {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib9PubStructV1fyyF : $@convention(method) (PubStruct) -> () {
    print(fooVar)
  }
}

public func runPub(_ arg: PubStruct) {
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (PubStruct) -> () {
  print(arg)
}

@frozen
public struct FrPubStruct {
  // CHECK-COMMON-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivM : $@yield_once @convention(method) (@inout FrPubStruct) -> @yields @inout Int {
  // CHECK-COMMON-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivg : $@convention(method) (FrPubStruct) -> Int {
  // CHECK-COMMON-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib11FrPubStructV6fooVarSivs : $@convention(method) (Int, @inout FrPubStruct) -> () {
  public var fooVar: Int
  public init(_ arg: Int) {
    // CHECK-COMMON-DAG: sil [serialized] [canonical] @$s3Lib11FrPubStructVyACSicfC : $@convention(method) (Int, @thin FrPubStruct.Type) -> FrPubStruct {
    fooVar = arg
  }
  public func f() {
    // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib11FrPubStructV1fyyF : $@convention(method) (FrPubStruct) -> () {
    print(fooVar)
  }
}
public func runFrPub(_ arg: FrPubStruct) {
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib8runFrPubyyAA0cD6StructVF : $@convention(thin) (FrPubStruct) -> () {
  print(arg)
}

package struct PkgStruct {
  // fooVar.getter
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (PkgStruct) -> Int {
  // fooVar.modify
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV6fooVarSivM : $@yield_once @convention(method) (@inout PkgStruct) -> @yields @inout Int {
  // fooVar.setter
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> () {
  package var fooVar: Int

  package init(_ arg: Int) {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> PkgStruct {
    fooVar = arg
  }
  package func f() {
    // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib9PkgStructV1fyyF : $@convention(method) (PkgStruct) -> () {
    print(fooVar)
  }
}

package func runPkg(_ arg: PkgStruct) {
  // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (PkgStruct) -> () {
  print(arg)
}

public protocol PubProto {
  var data: Int { get set }
  func pubfunc(_ arg: Int) -> Int
}

public class PubKlass: PubProto {
  // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PubProto) (@in_guaranteed PubKlass) -> Int {
  // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PubProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PubKlass> {
  // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PubProto) (Int, @inout PubKlass) -> () {
  // CHECK-RES-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassC4dataSivg : $@convention(method) (@guaranteed PubKlass) -> Int
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib8PubKlassC4dataSivg : $@convention(method) (@guaranteed PubKlass) -> Int {
  // CHECK-RES-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassC4dataSivs : $@convention(method) (Int, @guaranteed PubKlass) -> () {
  // CHECK-NONRES-DAG: sil [transparent] [serialized] [canonical] [ossa] @$s3Lib8PubKlassC4dataSivs : $@convention(method) (Int, @guaranteed PubKlass) -> () {
  public var data: Int
  public init(_ arg: Int = 1) {
    // default argument 0 of PubKlass.init(_:)
    // CHECK-COMMON-DAG: sil non_abi [serialized] [canonical] @$s3Lib8PubKlassCyACSicfcfA_ : $@convention(thin) () -> Int {
    // CHECK-COMMON-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCyACSicfc : $@convention(method) (Int, @owned PubKlass) -> @owned PubKlass {
    // CHECK-COMMON-DAG: sil [serialized] [exact_self_class] [canonical] @$s3Lib8PubKlassCyACSicfC : $@convention(method) (Int, @thick PubKlass.Type) -> @owned PubKlass {
    // CHECK-COMMON-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCfD : $@convention(method) (@owned PubKlass) -> () {
    // CHECK-COMMON-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassCfd : $@convention(method) (@guaranteed PubKlass) -> @owned Builtin.NativeObject {
    self.data = arg
  }
  public func pubfunc(_ arg: Int) -> Int {
    // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PubKlassCAA0B5ProtoA2aDP7pubfuncyS2iFTW : $@convention(witness_method: PubProto) (Int, @in_guaranteed PubKlass) -> Int {
    // CHECK-COMMON-DAG: sil [serialized] [canonical] @$s3Lib8PubKlassC7pubfuncyS2iF : $@convention(method) (Int, @guaranteed PubKlass) -> Int {
    return data + arg
  }
}

public func runPubKlass(_ arg: PubKlass) {
  // CHECK-NONRES-DAG: sil [serialized] [canonical] @$s3Lib11runPubKlassyyAA0cD0CF : $@convention(thin) (@guaranteed PubKlass) -> () {
  print(arg)
}

package protocol PkgProto {
  var data: Int { get set }
  func pkgfunc(_ arg: Int) -> Int
}

package class PkgKlass: PkgProto {
  // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlass) -> Int {
  // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PkgProto) (Int, @inout PkgKlass) -> () {
  // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP4dataSivMTW : $@yield_once @convention(witness_method: PkgProto) @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Int for <PkgKlass> {
  // CHECK-RES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
  // CHECK-RES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
  // CHECK-RES-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
  // CHECK-NONRES-DAG: sil package [transparent] [serialized] [canonical] [ossa] @$s3Lib8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
  package var data: Int

  package init(_ arg: Int = 1) {
    // FIXME: package -> package_non_abi for default argument 0 of PkgKlass.init(_:)
    // CHECK-COMMON-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCyACSicfcfA_ : $@convention(thin) () -> Int {
    // CHECK-COMMON-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCyACSicfc : $@convention(method) (Int, @owned PkgKlass) -> @owned PkgKlass {
    // CHECK-COMMON-DAG: sil package [serialized] [exact_self_class] [canonical] @$s3Lib8PkgKlassCyACSicfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {
    // CHECK-COMMON-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCfd : $@convention(method) (@guaranteed PkgKlass) -> @owned Builtin.NativeObject {
    // CHECK-COMMON-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassCfD : $@convention(method) (@owned PkgKlass) -> ()
    self.data = arg
  }

  package func pkgfunc(_ arg: Int) -> Int {
    // CHECK-COMMON-DAG: sil shared [transparent] [serialized] [thunk] [canonical] [ossa] @$s3Lib8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW : $@convention(witness_method: PkgProto) (Int, @in_guaranteed PkgKlass) -> Int {
    // CHECK-COMMON-DAG: sil package [serialized] [canonical] @$s3Lib8PkgKlassC7pkgfuncyS2iF : $@convention(method) (Int, @guaranteed PkgKlass) -> Int {
    return data + arg
  }
}

package func runPkgKlass(_ arg: PkgKlass) {
  // CHECK-NONRES-DAG: sil package [serialized] [canonical] @$s3Lib11runPkgKlassyyAA0cD0CF : $@convention(thin) (@guaranteed PkgKlass) -> () {
  print(arg)
}


// CHECK-COMMON-LABEL: sil_vtable [serialized] PubKlass {
// CHECK-COMMON-NEXT:   #PubKlass.data!getter: (PubKlass) -> () -> Int : @$s3Lib8PubKlassC4dataSivg
// CHECK-COMMON-NEXT:   #PubKlass.data!setter: (PubKlass) -> (Int) -> () : @$s3Lib8PubKlassC4dataSivs
// CHECK-COMMON-NEXT:   #PubKlass.data!modify: (PubKlass) -> () -> () : @$s3Lib8PubKlassC4dataSivM
// CHECK-COMMON-NEXT:   #PubKlass.init!allocator: (PubKlass.Type) -> (Int) -> PubKlass : @$s3Lib8PubKlassCyACSicfC
// CHECK-COMMON-NEXT:   #PubKlass.pubfunc: (PubKlass) -> (Int) -> Int : @$s3Lib8PubKlassC7pubfuncyS2iF
// CHECK-COMMON-NEXT:   #PubKlass.deinit!deallocator: @$s3Lib8PubKlassCfD

// CHECK-COMMON-LABEL: sil_vtable [serialized] PkgKlass {
// CHECK-COMMON-NEXT:   #PkgKlass.data!getter: (PkgKlass) -> () -> Int : @$s3Lib8PkgKlassC4dataSivg
// CHECK-COMMON-NEXT:   #PkgKlass.data!setter: (PkgKlass) -> (Int) -> () : @$s3Lib8PkgKlassC4dataSivs
// CHECK-COMMON-NEXT:   #PkgKlass.data!modify: (PkgKlass) -> () -> () : @$s3Lib8PkgKlassC4dataSivM
// CHECK-COMMON-NEXT:   #PkgKlass.init!allocator: (PkgKlass.Type) -> (Int) -> PkgKlass : @$s3Lib8PkgKlassCyACSicfC
// CHECK-COMMON-NEXT:   #PkgKlass.pkgfunc: (PkgKlass) -> (Int) -> Int : @$s3Lib8PkgKlassC7pkgfuncyS2iF
// CHECK-COMMON-NEXT:   #PkgKlass.deinit!deallocator: @$s3Lib8PkgKlassCfD

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
