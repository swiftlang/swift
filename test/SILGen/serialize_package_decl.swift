// RUN: %empty-directory(%t)
// RUN: split-file %s %t


/// Build Utils module non-resiliently
// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name Pkg \
// RUN:   -O -wmo \
// RUN:   -experimental-package-cmo -experimental-allow-non-resilient-access \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-sil-opt %t/Utils.swiftmodule -sil-verify-all -o %t/Utils-A.sil

// RUN: %target-build-swift -module-name=Main -package-name Pkg -I%t -emit-sil %t/main.swift -o %t/Main-A.sil

// REQUIRES: swift_in_compiler


//--- Utils.swift

// CHECK: PkgProto

package class PkgStruct {
  package static let pkgClosurePtr: (Int) -> (Int) = { $0 }
}

package protocol PkgProto {
  var data: Int { get set }
  func pkgfunc(_ arg: Int) -> Int
}

package class PkgKlass: PkgProto {
  // Similar to PublicKlass, key path getter/setter for PkgKlass are only emitted
  // when resilient.

  // key path getter for PkgKlass.data : PkgKlass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTK : $@convention(keypath_accessor_getter) (@in_guaranteed PkgKlass) -> @out Int {

  // key path setter for PkgKlass.data : PkgKlass
  // UTILS-RES-DAG: sil shared [thunk] [ossa] @$s5Utils8PkgKlassC4dataSivpACTk : $@convention(keypath_accessor_setter) (@in_guaranteed Int, @in_guaranteed PkgKlass) -> () {

  // protocol witness for PkgProto.data.getter in conformance PkgKlass
  // UTILS-COMMON-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP4dataSivgTW : $@convention(witness_method: PkgProto) (@in_guaranteed PkgKlass) -> Int {

  // protocol witness for PkgProto.data.setter in conformance PkgKlass
  // UTILS-COMMON-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP4dataSivsTW : $@convention(witness_method: PkgProto) (Int, @inout PkgKlass) -> () {

  // PkgKlass.data.getter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivg : $@convention(method) (@guaranteed PkgKlass) -> Int {

  // PkgKlass.data.setter
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivs : $@convention(method) (Int, @guaranteed PkgKlass) -> () {

  // PkgKlass.data.modify
  // UTILS-RES-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {
  // UTILS-NONRES-DAG: sil package [transparent] [ossa] @$s5Utils8PkgKlassC4dataSivM : $@yield_once @convention(method) (@guaranteed PkgKlass) -> @yields @inout Int {

  // protocol witness for PkgProto.pkgfunc(_:) in conformance PkgKlass
  // UTILS-COMMON-DAG: sil private [transparent] [thunk] [ossa] @$s5Utils8PkgKlassCAA0B5ProtoA2aDP7pkgfuncyS2iFTW : $@convention(witness_method: PkgProto) (Int, @in_guaranteed PkgKlass) -> Int {
  package var data: Int

  // default argument 0 of PkgKlass.init(data:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfcfA_ : $@convention(thin) () -> Int {

  // PkgKlass.__allocating_init(data:)
  // UTILS-COMMON-DAG: sil package [exact_self_class] [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfC : $@convention(method) (Int, @thick PkgKlass.Type) -> @owned PkgKlass {

  // PkgKlass.init(data:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassC4dataACSi_tcfc : $@convention(method) (Int, @owned PkgKlass) -> @owned PkgKlass {

  // PkgKlass.deinit
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassCfd : $@convention(method) (@guaranteed PkgKlass) -> @owned Builtin.NativeObject {

  // PkgKlass.__deallocating_deinit
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassCfD : $@convention(method) (@owned PkgKlass) -> () {
  package init(data: Int = 1) {
    self.data = data
  }

  // PkgKlass.pkgfunc(_:)
  // UTILS-COMMON-DAG: sil package [ossa] @$s5Utils8PkgKlassC7pkgfuncyS2iF : $@convention(method) (Int, @guaranteed PkgKlass) -> Int {
  package func pkgfunc(_ arg: Int) -> Int {
    return data + arg
  }
}

//--- main.swift
import Utils

package func callPkgStaticClosurePointer(_ x: Int) -> Int {
  return PkgStruct.pkgClosurePtr(x)
}
