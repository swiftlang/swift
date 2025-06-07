// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -parse-as-library -emit-module -emit-module-path %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -experimental-package-cmo -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution
// RUN: %target-sil-opt -enable-sil-verify-all %t/Lib.swiftmodule -o %t/Lib.sil

// RUN: %FileCheck %s < %t/Lib.sil

// REQUIRES: swift_in_compiler

//--- Lib.swift
package protocol PkgProto {
  /// Key path getter for a protocol property has a shared linkage as below, and a function referencing
  /// this getter (as well as this getter) should not be serialized in Package CMO, even if the `witness_method`
  /// in this getter has package or public access level.
  // key path getter for PkgProto.pkgVar : <A>A
  //  sil shared [thunk] @$s3Lib8PkgProtoP6pkgVarSSvpAaBRzlxTK : $@convention(keypath_accessor_getter) <T where T : PkgProto> (@in_guaranteed T) -> @out String {
  //  [%0: noescape **, write v**]
  //  [%1: read v**, write v**, copy v**, destroy v**]
  //  [global: read,write,copy,destroy,allocate,deinit_barrier]
  //  // %0                                             // user: %4
  //  // %1                                             // user: %3
  //  bb0(%0 : $*String, %1 : $*T):
  //    %2 = witness_method $T, #PkgProto.pkgVar!getter : <Self where Self : PkgProto> (Self) -> () -> String : $@convention(witness_method: PkgProto) <τ_0_0 where τ_0_0 : PkgProto> (@in_guaranteed τ_0_0) -> @owned String // user: %3
  //    %3 = apply %2<T>(%1) : $@convention(witness_method: PkgProto) <τ_0_0 where τ_0_0 : PkgProto> (@in_guaranteed τ_0_0) -> @owned String // user: %4
  //    store %3 to %0 : $*String                       // id: %4
  //    %5 = tuple ()                                   // user: %6
  //    return %5 : $()                                 // id: %6
  //  } // end sil function '$s3Lib8PkgProtoP6pkgVarSSvpAaBRzlxTK'
  var pkgVar: String { get }
  
  // key path getter for PkgProto.pkgVar : <A>A
  // CHECK-NOT: sil [serialized_for_package] [thunk] [canonical] @$s3Lib8PkgProtoP6pkgVarSSvpAaBRzlxTK : $@convention(keypath_accessor_getter) <T where T : PkgProto> (@in_guaranteed T) -> @out String {
  // CHECK-NOT: witness_method $T, #PkgProto.pkgVar!getter : <Self where Self : PkgProto> (Self) -> () -> String : $@convention(witness_method: PkgProto) <τ_0_0 where τ_0_0 : PkgProto> (@in_guaranteed τ_0_0) -> @owned String // user: %3
  // CHECK-NOT: // end sil function '$s3Lib8PkgProtoP6pkgVarSSvpAaBRzlxTK'
}

package struct Foo: PkgProto {
  package var pkgVar: String {
    return "Foo pkgVar"
  }
}

/// testKeyPath dynamically accesses pkgVar property on a generic type using keypath,
/// referencing the getter above, s3Lib8PkgProtoP6pkgVarSSvpAaBRzlxTK,
/// and should not be serialized.
// testKeyPath<A>(_:)
// CHECK-NOT: sil package [serialized_for_package] [canonical] @$s3Lib11testKeyPathys0cD0CyxSSGSayxGAA8PkgProtoRzlF : $@convention(thin) <T where T : PkgProto> (@guaranteed Array<T>) -> @owned KeyPath<T, String> {
// CHECK-NOT: keypath $KeyPath<T, String>, <τ_0_0 where τ_0_0 : PkgProto> (root $τ_0_0; gettable_property $String,  id #PkgProto.pkgVar!getter : <Self where Self : PkgProto> (Self) -> () -> String, getter @$s3Lib8PkgProtoP6pkgVarSSvpAaBRzlxTK : $@convention(keypath_accessor_getter) <τ_0_0 where τ_0_0 : PkgProto> (@in_guaranteed τ_0_0) -> @out String) <T>
// CHECK-NOT: } // end sil function '$s3Lib11testKeyPathys0cD0CyxSSGSayxGAA8PkgProtoRzlF'
package func testKeyPath<T: PkgProto>(_ array: [T]) -> KeyPath<T, String> {
  return \T.pkgVar
}

// CHECK: sil_witness_table package [serialized_for_package] Foo: PkgProto module Lib {
// CHECK:  method #PkgProto.pkgVar!getter: <Self where Self : PkgProto> (Self) -> () -> String : @$s3Lib3FooVAA8PkgProtoA2aDP6pkgVarSSvgTW

