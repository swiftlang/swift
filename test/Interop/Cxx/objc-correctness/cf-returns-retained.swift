// RUN: %target-swift-emit-silgen -I %S/Inputs -cxx-interoperability-mode=default %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple

import CoreFoundation
import CFReturnsRetained

// CHECK-LABEL: sil hidden [ossa] @$s4main10testMethodyySo8CFHolderVF
func testMethod(_ h: CFHolder) {
  // CHECK: function_ref @{{.*}}copyString{{.*}} : $@convention(cxx_method) (@in_guaranteed CFHolder) -> @owned Optional<CFString>
  _ = h.copyString()
  // CHECK: function_ref @{{.*}}getString{{.*}} : $@convention(cxx_method) (@in_guaranteed CFHolder) -> Optional<Unmanaged<CFString>>
  _ = h.getString()
  // CHECK: function_ref @{{.*}}getStringNotRetained{{.*}} : $@convention(cxx_method) (@in_guaranteed CFHolder) -> @autoreleased Optional<CFString>
  _ = h.getStringNotRetained()
}

// CHECK-LABEL: sil hidden [ossa] @$s4main16testStaticMethodyySo11CFStringRefaF
func testStaticMethod(_ s: CFString) {
  // CHECK: function_ref @{{.*}}staticCopyString{{.*}} : $@convention(c) (Optional<CFString>) -> @owned Optional<CFString>
  _ = CFHolder.staticCopyString(s)
  // CHECK: function_ref @{{.*}}staticGetString{{.*}} : $@convention(c) (Optional<CFString>) -> Optional<Unmanaged<CFString>>
  _ = CFHolder.staticGetString(s)
}

// CHECK-LABEL: sil hidden [ossa] @$s4main16testFreeFunctionyySo11CFStringRefaF
func testFreeFunction(_ s: CFString) {
  // CHECK: function_ref @{{.*}}freeCopyString{{.*}} : $@convention(c) (Optional<CFString>) -> @owned Optional<CFString>
  _ = freeCopyString(s)
}
