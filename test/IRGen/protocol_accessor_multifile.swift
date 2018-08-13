// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/protocol_accessor_multifile_other.swift > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.ll

// CHECK-LABEL: define{{.*}} void @"$S27protocol_accessor_multifile14useExistentialyyF"()
func useExistential() {
  // CHECK: [[BOX:%.+]] = alloca %T27protocol_accessor_multifile5ProtoP,
  // CHECK: call swiftcc void @"$S27protocol_accessor_multifile17globalExistentialAA5Proto_pvg"({{%.+}} [[BOX]])
  // CHECK: call swiftcc void @"$S27protocol_accessor_multifile5ProtoPAAE6methodyyF"
  globalExistential.method()
  // CHECK: call void @__swift_destroy_boxed_opaque_existential_1({{%.+}} [[BOX]])
  // CHECK: ret void
}

// NEGATIVE-NOT: @"$S27protocol_accessor_multifile5ProtoMp" = 
