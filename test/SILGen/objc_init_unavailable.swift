// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -enable-objc-interop -import-objc-header %S/Inputs/objc_init_unavailable.h %s | %FileCheck %s
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

@available(macOS, unavailable)
public func callUnavailableInit(name: String) -> ClassWithUnavailableInit {
  return ClassWithUnavailableInit(bundleID: name)
}

// CHECK-LABEL: sil [weak_imported] [ossa] @$s21objc_init_unavailable19callUnavailableInit4nameSo09ClassWitheF0CSS_tF : $@convention(thin) (@guaranteed String) -> @owned ClassWithUnavailableInit {
// CHECK: function_ref @$sSo24ClassWithUnavailableInitC8bundleIDABSgSSSg_tcfC : $@convention(method) (@owned Optional<String>, @thick ClassWithUnavailableInit.Type) -> @owned Optional<ClassWithUnavailableInit>
// CHECK: return

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo24ClassWithUnavailableInitC8bundleIDABSgSSSg_tcfC : $@convention(method) (@owned Optional<String>, @thick ClassWithUnavailableInit.Type) -> @owned Optional<ClassWithUnavailableInit> {
// CHECK: function_ref @$sSo24ClassWithUnavailableInitC8bundleIDABSgSSSg_tcfcTO : $@convention(method) (@owned Optional<String>, @owned ClassWithUnavailableInit) -> @owned Optional<ClassWithUnavailableInit>
// CHECK: return

// CHECK-LABEL: sil shared [serialized] [thunk] [ossa] @$sSo24ClassWithUnavailableInitC8bundleIDABSgSSSg_tcfcTO : $@convention(method) (@owned Optional<String>, @owned ClassWithUnavailableInit) -> @owned Optional<ClassWithUnavailableInit> {
// CHECK: objc_method %1 : $ClassWithUnavailableInit, #ClassWithUnavailableInit.init!initializer.foreign : (ClassWithUnavailableInit.Type) -> (String?) -> ClassWithUnavailableInit?, $@convention(objc_method) (Optional<NSString>, @owned ClassWithUnavailableInit) -> @owned Optional<ClassWithUnavailableInit>
// CHECK: return
