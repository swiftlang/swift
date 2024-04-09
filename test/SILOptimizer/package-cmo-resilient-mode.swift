// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-sil-opt.sil
// RUN: %FileCheck %s --check-prefix=CHECK-NONRES < %t/Lib-sil-opt.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil %t/main.swift -o %t/Main.sil
// RUN: %FileCheck %s --check-prefix=CHECK-ACCESS-NONRES < %t/Main.sil

// RUN: rm -rf %t/Lib.swiftmodule

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution

// RUN: %target-sil-opt %t/Lib.swiftmodule -sil-verify-all -o %t/Lib-sil-opt.sil
// RUN: %FileCheck %s --check-prefix=CHECK-RES < %t/Lib-sil-opt.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil %t/main.swift -o %t/Main.sil
// RUN: %FileCheck %s --check-prefix=CHECK-ACCESS-RES < %t/Main.sil

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib

// CHECK-ACCESS-NONRES-NOT: s3Lib9PubStructV6fooVarSivg
// CHECK-ACCESS-NONRES-NOT: s3Lib9PubStructV6fooVarSivs
// CHECK-ACCESS-NONRES-NOT: s3Lib9PkgStructV6fooVarSivg
// CHECK-ACCESS-NONRES-NOT: s3Lib9PubStructV6fooVarSivs

// CHECK-ACCESS-RES: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct
// CHECK-ACCESS-NONRES: function_ref @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct

// CHECK-ACCESS-RES: function_ref @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int
// CHECK-ACCESS-NONRES: [[PUB_GET:%.*]] = begin_access [read] [dynamic] {{.*}} : $*PubStruct
// CHECK-ACCESS-NONRES: struct_element_addr [[PUB_GET]] : $*PubStruct, #PubStruct.fooVar

// CHECK-ACCESS-RES: function_ref @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> ()
// CHECK-ACCESS-NONRES: [[PUB_MODIFY:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*PubStruct
// CHECK-ACCESS-NONRES: struct_element_addr [[PUB_MODIFY]] : $*PubStruct, #PubStruct.fooVar

// CHECK-ACCESS-RES: function_ref @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PubStruct) -> ()
// CHECK-ACCESS-NONRES: function_ref @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (PubStruct) -> ()

// CHECK-ACCESS-RES: function_ref @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int
// CHECK-ACCESS-NONRES: [[PKG_GET:%.*]] = begin_access [read] [dynamic] {{.*}} : $*PkgStruct
// CHECK-ACCESS-NONRES: struct_element_addr [[PKG_GET]] : $*PkgStruct, #PkgStruct.fooVar

// CHECK-ACCESS-RES: function_ref @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> ()
// CHECK-ACCESS-NONRES: [[PKG_MODIFY:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*PkgStruct
// CHECK-ACCESS-NONRES: struct_element_addr [[PKG_MODIFY]] : $*PkgStruct, #PkgStruct.fooVar

// CHECK-ACCESS-RES: function_ref @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> ()
// CHECK-ACCESS-NONRES: function_ref @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (PkgStruct) -> ()

var p = PubStruct(1)
let prev = p.fooVar
p.fooVar = 3
runPub(p)
print(prev)

// PubStruct.init(_:)
// CHECK-ACCESS-RES:    sil @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> @out PubStruct
// CHECK-ACCESS-NONRES: sil @$s3Lib9PubStructVyACSicfC : $@convention(method) (Int, @thin PubStruct.Type) -> PubStruct

// PubStruct.fooVar.getter
// CHECK-ACCESS-RES: sil @$s3Lib9PubStructV6fooVarSivg : $@convention(method) (@in_guaranteed PubStruct) -> Int

// PubStruct.fooVar.setter
// CHECK-ACCESS-RES: sil @$s3Lib9PubStructV6fooVarSivs : $@convention(method) (Int, @inout PubStruct) -> ()

// runPub(_:)
// CHECK-ACCESS-RES:    sil @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PubStruct) -> ()
// CHECK-ACCESS-NONRES: sil @$s3Lib6runPubyyAA0C6StructVF : $@convention(thin) (PubStruct) -> ()

var pkg = PkgStruct(1)
let prevPkg = pkg.fooVar
pkg.fooVar = 3
runPkg(pkg)
print(prevPkg)

// PkgStruct.init(_:)
// CHECK-ACCESS-RES:    sil package_external @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> @out PkgStruct
// CHECK-ACCESS-NONRES: sil package_external @$s3Lib9PkgStructVyACSicfC : $@convention(method) (Int, @thin PkgStruct.Type) -> PkgStruct

// PkgStruct.fooVar.getter
// CHECK-ACCESS-RES: sil package_external @$s3Lib9PkgStructV6fooVarSivg : $@convention(method) (@in_guaranteed PkgStruct) -> Int

// PkgStruct.fooVar.setter
// CHECK-ACCESS-RES: sil package_external @$s3Lib9PkgStructV6fooVarSivs : $@convention(method) (Int, @inout PkgStruct) -> ()

// runPkg(_:)
// CHECK-ACCESS-RES:    sil package_external @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (@in_guaranteed PkgStruct) -> ()
// CHECK-ACCESS-NONRES: sil package_external @$s3Lib6runPkgyyAA0C6StructVF : $@convention(thin) (PkgStruct) -> ()


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
