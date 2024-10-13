 // RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name=Lib -package-name libpkg -emit-silgen %s -enable-library-evolution -wmo -o %t/Lib-no-pcmo-silgen.sil

// RUN: %target-swift-frontend -module-name=Lib -package-name libpkg -emit-sil %s -enable-library-evolution -wmo -o %t/Lib-no-pcmo-sil.sil

// RUN: %target-swift-frontend -module-name=Lib -package-name libpkg -emit-ir %s -enable-library-evolution -wmo -o %t/Lib-no-pcmo-ir.ll -emit-tbd-path %t/Lib-no-pcmo-tbd.tbd -tbd-install_name Lib

// RUN: %target-swift-frontend -module-name=Lib -package-name libpkg -emit-silgen %s -enable-library-evolution -wmo -allow-non-resilient-access -package-cmo -o %t/Lib-silgen.sil

// RUN: %target-swift-frontend -module-name=Lib -package-name libpkg -emit-sil %s -enable-library-evolution -wmo -allow-non-resilient-access -package-cmo -o %t/Lib-sil.sil

// RUN: %target-swift-frontend -module-name=Lib -package-name libpkg -emit-ir %s -enable-library-evolution -wmo -allow-non-resilient-access -package-cmo -o %t/Lib-ir.ll -emit-tbd-path %t/Lib-tbd.tbd -tbd-install_name Lib

// RUN: %FileCheck %s --check-prefix=CHECK-NO-PCMO-SILGEN < %t/Lib-no-pcmo-silgen.sil
// RUN: %FileCheck %s --check-prefix=CHECK-NO-PCMO-SIL < %t/Lib-no-pcmo-sil.sil
// RUN: %FileCheck %s --check-prefix=CHECK-NO-PCMO-IR < %t/Lib-no-pcmo-ir.ll
// RUN: %FileCheck %s --check-prefix=CHECK-NO-PCMO-TBD < %t/Lib-no-pcmo-tbd.tbd
// RUN: %FileCheck %s --check-prefix=CHECK-SILGEN < %t/Lib-silgen.sil
// RUN: %FileCheck %s --check-prefix=CHECK-SIL < %t/Lib-sil.sil
// RUN: %FileCheck %s --check-prefix=CHECK-IR < %t/Lib-ir.ll
// RUN: %FileCheck %s --check-prefix=CHECK-TBD < %t/Lib-tbd.tbd


/// TEST that a public (or package) protocol witness thunk gets a shared linkage if Package CMO is enabled.
/// It also gets [serialized] in emit-silgen.
///
// protocol witness for Proto.foo.getter in conformance Pub

// CHECK-NO-PCMO-SILGEN-DAG: sil private [transparent] [thunk] [ossa] @$s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW : $@convention(witness_method: Proto) (@in_guaranteed Pub) -> @owned String {
// CHECK-SILGEN-DAG:         sil shared [transparent] [serialized] [thunk] [ossa] @$s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW : $@convention(witness_method: Proto) (@in_guaranteed Pub) -> @owned String {

// CHECK-NO-PCMO-SIL-DAG: sil private [transparent] [thunk] @$s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW : $@convention(witness_method: Proto) (@in_guaranteed Pub) -> @owned String {
// CHECK-SIL-DAG:         sil shared [transparent] [thunk] @$s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW : $@convention(witness_method: Proto) (@in_guaranteed Pub) -> @owned String {

// CHECK-NO-PCMO-IR-DAG: define internal swiftcc { {{i64, ptr|i32, i32, i32}} } @"$s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW"
// CHECK-IR-DAG:         define linkonce_odr hidden swiftcc { {{i64, ptr|i32, i32, i32}} } @"$s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW"

// protocol witness for Proto.bar(_:) in conformance Pub

// CHECK-NO-PCMO-SILGEN-DAG: sil private [transparent] [thunk] [ossa] @$s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW : $@convention(witness_method: Proto) (Int, @in_guaranteed Pub) -> Int {
// CHECK-SILGEN-DAG:      sil shared [transparent] [serialized] [thunk] [ossa] @$s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW : $@convention(witness_method: Proto) (Int, @in_guaranteed Pub) -> Int {

// CHECK-NO-PCMO-SIL-DAG: sil private [transparent] [thunk] @$s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW : $@convention(witness_method: Proto) (Int, @in_guaranteed Pub) -> Int {
// CHECK-SIL-DAG:         sil shared [transparent] [thunk] @$s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW : $@convention(witness_method: Proto) (Int, @in_guaranteed Pub) -> Int {

// CHECK-NO-PCMO-IR-DAG: define internal swiftcc {{i64|i32}} @"$s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW"
// CHECK-IR-DAG:         define linkonce_odr hidden swiftcc {{i64|i32}} @"$s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW"

// CHECK-NO-PCMO-TBD-NOT: s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW
// CHECK-NO-PCMO-TBD-NOT: s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW
// CHECK-TBD-NOT: s3Lib3PubVAA5ProtoA2aDP3fooSSvgTW
// CHECK-TBD-NOT: s3Lib3PubVAA5ProtoA2aDP3baryS2iFTW

public protocol Proto {
  var foo: String { get set }
  func bar(_ arg: Int) -> Int
}

public struct Pub: Proto {
  public var foo = "foo"
  public func bar(_ arg: Int) -> Int {
    return arg
  }
}
