// RUN: %target-swift-emit-silgen -I %S/Inputs -cxx-interoperability-mode=default %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: VENDOR=apple

// Members inherited from a C++ base class are called through a synthesized
// forwarding method. Make sure that it has the same ownership conventions as
// the base class member.

import CoreFoundation
import CFInheritedMembers

func testDerived(_ d: CFDerived, _ a: CFMutableArray) {
  d.consume(a)
  _ = d.borrow()
  _ = d.copy()
  _ = d.getAudited()
}

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo9CFDerivedV7consumeyySo17CFMutableArrayRefaSgnF : $@convention(method) (@owned Optional<CFMutableArray>, @in_guaranteed CFDerived) -> ()
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_consume{{.*}} : $@convention(cxx_method) (@owned Optional<CFMutableArray>, @in_guaranteed CFDerived) -> ()

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo9CFDerivedV6borrowSo17CFMutableArrayRefaSgyF : $@convention(method) (@in_guaranteed CFDerived) -> @owned Optional<CFMutableArray>
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_borrow{{.*}} : $@convention(cxx_method) (@in_guaranteed CFDerived) -> @autoreleased Optional<CFMutableArray>

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo9CFDerivedV4copySo17CFMutableArrayRefaSgyF : $@convention(method) (@in_guaranteed CFDerived) -> @owned Optional<CFMutableArray>
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_copy{{.*}} : $@convention(cxx_method) (@in_guaranteed CFDerived) -> @owned Optional<CFMutableArray>

// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo9CFDerivedV10getAuditedSo17CFMutableArrayRefaSgyF : $@convention(method) (@in_guaranteed CFDerived) -> @owned Optional<CFMutableArray>
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_getAudited{{.*}} : $@convention(cxx_method) (@in_guaranteed CFDerived) -> @autoreleased Optional<CFMutableArray>
