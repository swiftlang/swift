// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_debug.swift -experimental-serialize-debug-info -O -g
// RUN: llvm-bcanalyzer %t/def_debug.swiftmodule | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -module-name debug -g -emit-sil -I %t %s -O | %FileCheck %s --dump-input=fail
// RUN: %target-swift-frontend -module-name debug -g -emit-sil -I %t %s | %FileCheck %s --check-prefix=NOOPT --dump-input=fail

import def_debug

// SIL-NOT: UnknownCode

// CHECK: sil_scope [[SCOPE_ONE:[0-9]+]] { loc "{{.*}}def_debug.swift":2:13 parent @$s9def_debug3foo1xs6UInt64VAE_tF : $@convention(thin) (UInt64) -> UInt64 inlined_at [[INLINE_SITE_ONE:[0-9]+]] }
// CHECK: sil_scope [[SCOPE_TWO:[0-9]+]] { loc "{{.*}}def_debug.swift":3:5 parent [[SCOPE_ONE]] inlined_at [[INLINE_SITE_ONE]] }
let simpleFunc = foo(x: UInt64.random(in: 1...200))

// _transparent with Onone
// NOOPT: sil_scope [[SCOPE_GENERIC_ONE:[0-9]+]] { loc "{{.*}}def_debug.swift":10:13 parent @$s9def_debug25specializedGenericInlinedSiyF : $@convention(thin) () -> Int }
var funcReferringInlinedGeneric = specializedGenericInlined()

// _alwaysEmitIntoClient with Onone
// NOOPT: sil_scope [[SCOPE_ONE:[0-9]+]] { loc "{{.*}}def_debug.swift":21:13 parent @$s9def_debug10barGeneric_3sumxSayxG_xtSjRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : Numeric> (@guaranteed Array<τ_0_0>, @in_guaranteed τ_0_0) -> @out τ_0_0 }
// NOOPT-DAG: sil_scope [[SCOPE_TWO:[0-9]+]] { loc "{{.*}}def_debug.swift":22:9 parent [[SCOPE_ONE]] }
// NOOPT-DAG: sil_scope {{[0-9]+}} { loc "{{.*}}def_debug.swift":22:16 parent [[SCOPE_ONE]] }
// NOOPT: sil_scope [[SCOPE_THREE:[0-9]+]] { loc "{{.*}}def_debug.swift":23:5 parent [[SCOPE_TWO]] }
// NOOPT: sil_scope {{[0-9]+}} { loc "{{.*}}def_debug.swift":23:14 parent [[SCOPE_THREE]] }
let x = barGeneric([1,2], sum: 0)

//Use inlinable so that the generic inliner later tries to lookup an inlinable
//id function
@inlinable
func fooId() {
    /*
    Swift tries to lookup for generic specialization <serialized, Swift.Int> of
    def_debug.id<A where A: Swift.Equatable>(A) -> A` which is only referenced
    by debug info in the def_debug module and deserialized as a zombie.  Make
    sure compiler handles this correctly and does not crash when trying to
    compile this
     */
    let _ = id(1)
}
