// RUN: %target-swift-frontend %use_no_opaque_pointers -disable-generic-metadata-prespecialization -emit-ir -primary-file %s %S/Inputs/signature_conformances_other.swift | %FileCheck %s
// RUN: %target-swift-frontend -disable-generic-metadata-prespecialization -emit-ir -primary-file %s %S/Inputs/signature_conformances_other.swift

// Make sure we correctly determine the witness table is dependent, even though
// it was defined in a different file.

// CHECK-LABEL: define hidden swiftcc void @"$s32signature_conformances_multifile5passQyyF"()
func passQ() {
  // CHECK: call swiftcc void @"$s32signature_conformances_multifile12AlsoConformsVACyxGycfC"(%swift.type* @"$sSiN")
  // CHECK: %0 = call {{.*}} @"$s32signature_conformances_multifile12AlsoConformsVySiGMD"
  // CHECK: %1 = call i8** @"$s32signature_conformances_multifile12AlsoConformsVySiGACyxGAA1QAAWl"()
  // CHECK: call swiftcc void @"$s32signature_conformances_multifile6takesQyyxAA1QRzlF"(%swift.opaque* noalias nocapture undef, %swift.type* %0, i8** %1)
  takesQ(AlsoConforms<Int>())

  // CHECK: ret void
}

// CHECK-LABEL: define hidden swiftcc void @"$s32signature_conformances_multifile5passPyyF"()
func passP() {
  // CHECK: call swiftcc void @"$s32signature_conformances_multifile8ConformsVACyxq_GycfC"(%swift.type* @"$sSiN", %swift.type* @"$sSSN")
  // CHECK: %0 = call {{.*}} @"$s32signature_conformances_multifile8ConformsVySiSSGMD"
  // CHECK: %1 = call i8** @"$s32signature_conformances_multifile8ConformsVySiSSGACyxq_GAA1PAAWl"()
  // CHECK: call swiftcc void @"$s32signature_conformances_multifile6takesPyyxAA1PRzlF"(%swift.opaque* noalias nocapture undef, %swift.type* %0, i8** %1)
  takesP(Conforms<Int, String>())

  // CHECK: ret void
}
