// RUN: %target-swift-frontend %use_no_opaque_pointers -prespecialize-generic-metadata -target %module-target-future -emit-ir -primary-file %s %S/Inputs/signature_conformances_other.swift | %FileCheck %s -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -prespecialize-generic-metadata -target %module-target-future -emit-ir -primary-file %s %S/Inputs/signature_conformances_other.swift

// REQUIRES: VENDOR=apple || OS=linux-gnu
// UNSUPPORTED: CPU=i386 && OS=ios
// UNSUPPORTED: CPU=armv7 && OS=ios
// UNSUPPORTED: CPU=armv7s && OS=ios

// Make sure we correctly determine the witness table is dependent, even though
// it was defined in a different file.

// CHECK-LABEL: define hidden swiftcc void @"$s39signature_conformances_multifile_future5passQyyF"()
func passQ() {
  // CHECK: call swiftcc void @"$s39signature_conformances_multifile_future12AlsoConformsVACyxGycfC"(%swift.type* @"$sSiN")
  // CHECK: [[METADATA:%[0-9]+]] = call %swift.type* @__swift_instantiateConcreteTypeFromMangledName({ i32, i32 }* @"$s39signature_conformances_multifile_future12AlsoConformsVySiGMD")
  // CHECK: [[WITNESS_TABLE:%[0-9]+]] = call i8** @"$s39signature_conformances_multifile_future12AlsoConformsVySiGACyxGAA1QAAWl"()
  // CHECK: call swiftcc void @"$s39signature_conformances_multifile_future6takesQyyxAA1QRzlF"(
  // CHECK-SAME:   %swift.opaque* noalias nocapture undef, 
  // CHECK-SAME:   %swift.type* [[METADATA]], 
  // CHECK-SAME:   i8** [[WITNESS_TABLE]]
  // CHECK-SAME: )
  takesQ(AlsoConforms<Int>())

  // CHECK: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"$s39signature_conformances_multifile_future5passPyyF"()
func passP() {
  // CHECK: call swiftcc void @"$s39signature_conformances_multifile_future8ConformsVACyxq_GycfC"(%swift.type* @"$sSiN", %swift.type* @"$sSSN")
  // CHECK: [[METADATA:%[0-9]+]] = call %swift.type* @__swift_instantiateConcreteTypeFromMangledName({ i32, i32 }* @"$s39signature_conformances_multifile_future8ConformsVySiSSGMD")
  // CHECK: [[WITNESS_TABLE:%[0-9]+]] = call i8** @"$s39signature_conformances_multifile_future8ConformsVySiSSGACyxq_GAA1PAAWl"()
  // CHECK: call swiftcc void @"$s39signature_conformances_multifile_future6takesPyyxAA1PRzlF"(
  // CHECK-SAME:   %swift.opaque* noalias nocapture undef, 
  // CHECK-SAME:   %swift.type* [[METADATA]], 
  // CHECK-SAME:   i8** [[WITNESS_TABLE]]
  // CHECK-SAME: )
  takesP(Conforms<Int, String>())

  // CHECK: ret void
}
