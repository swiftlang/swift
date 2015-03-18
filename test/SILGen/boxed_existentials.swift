// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func test_type_lowering(x: _ErrorType) { }
// CHECK-LABEL: sil hidden @_TF18boxed_existentials18test_type_loweringFPSs10_ErrorType_T_ : $@thin (@owned _ErrorType) -> () {
// CHECK:         strong_release %0 : $_ErrorType
