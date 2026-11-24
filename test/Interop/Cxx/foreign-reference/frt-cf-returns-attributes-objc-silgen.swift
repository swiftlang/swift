// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s -validate-tbd-against-ir=none | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s

// REQUIRES: objc_interop

import CFReturnsAttributesFRT

func testObjCMethods() {
  _ = CFRefCountedProvider.classGetNotRetained()
  // CHECK: objc_method {{.*}}classGetNotRetained{{.*}}, $@convention(objc_method) (@objc_metatype CFRefCountedProvider.Type) -> CFRefCounted
  _ = CFRefCountedProvider.classGetRetained()
  // CHECK: objc_method {{.*}}classGetRetained{{.*}}, $@convention(objc_method) (@objc_metatype CFRefCountedProvider.Type) -> @owned CFRefCounted
}

// CHECK-NOT: @autoreleased
