// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s -validate-tbd-against-ir=none | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s

// CF_RETURNS_(NOT_)RETAINED on a function returning a foreign reference type
// must not make Swift treat the result as an autoreleased Objective-C object.

import CFReturnsAttributesFRT

func testFreeFunctions() {
  _ = getCFRefCountedNotRetained()
  // CHECK: function_ref @{{.*}}getCFRefCountedNotRetained{{.*}} : $@convention(c) () -> CFRefCounted
  _ = createCFRefCountedNotRetained()
  // CHECK: function_ref @{{.*}}createCFRefCountedNotRetained{{.*}} : $@convention(c) () -> CFRefCounted
  _ = getCFRefCountedRetained()
  // CHECK: function_ref @{{.*}}getCFRefCountedRetained{{.*}} : $@convention(c) () -> @owned CFRefCounted
}

func testMethods(_ x: CFRefCounted) {
  _ = CFRefCounted.staticCreateNotRetained()
  // CHECK: function_ref @{{.*}}staticCreateNotRetained{{.*}} : $@convention(c) () -> CFRefCounted
  _ = CFRefCounted.staticGetRetained()
  // CHECK: function_ref @{{.*}}staticGetRetained{{.*}} : $@convention(c) () -> @owned CFRefCounted
  _ = x.methodNotRetained()
  // CHECK: function_ref @{{.*}}methodNotRetained{{.*}} : $@convention(cxx_method) (@guaranteed CFRefCounted) -> CFRefCounted
  _ = x.methodRetained()
  // CHECK: function_ref @{{.*}}methodRetained{{.*}} : $@convention(cxx_method) (@guaranteed CFRefCounted) -> @owned CFRefCounted
}

// CHECK-NOT: @autoreleased
