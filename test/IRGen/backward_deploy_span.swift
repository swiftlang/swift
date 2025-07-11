// RUN: %target-swift-frontend -target %target-cpu-apple-macos12 -emit-ir -o - -primary-file %s | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: OS=macosx

func useGenericMetatype(_ type: any ~Escapable.Type) { }

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span11testSpanIntyyF"()
func testSpanInt() {
  // CHECK: call swiftcc %swift.metadata_response @"$ss4SpanVySiGMa"
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi0_s_XPXpF"
  useGenericMetatype(Span<Int>.self)
}

// CHECK-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$ss4SpanVySiGMa"(i64 %0)
// CHECK: call swiftcc %swift.metadata_response @"$ss4SpanVMa"(i64 %0, ptr @"$sSiN")


