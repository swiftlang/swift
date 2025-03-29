// RUN: %target-swift-frontend -enable-objc-interop -import-objc-header %S/Inputs/throwing-mismarked-nonnullable-error.h %s -Xllvm -sil-print-types -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

// Make sure that we do not crash when importing a non-null NSError as
// throwing. We really just shouldn't expect this at all. I filed: rdar://94656178
// to track that work.

// CHECK-LABEL: sil hidden {{.*}}@$s4main1fyyF : $@convention(thin) () -> () {
// CHECK: [[STACK:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
// CHECK: inject_enum_addr [[STACK]] : $*Optional<NSError>, #Optional.none!enumelt
// CHECK: [[FN:%.*]] = objc_method {{%[0-9]+}} : $MyClass, #MyClass.submit!foreign : (MyClass) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<NSError>>, MyClass) -> ObjCBool
// CHECK: [[NEXT_STACK:%.*]] = alloc_stack $@sil_unmanaged Optional<NSError>
// CHECK: [[VAL:%.*]] = load [[STACK]] : $*Optional<NSError>
// CHECK: [[UNMANAGED:%.*]] = ref_to_unmanaged [[VAL]]
// CHECK: store [[UNMANAGED]] to [[NEXT_STACK]]
// CHECK: [[PTR:%.*]] = address_to_pointer [stack_protection] [[NEXT_STACK]]
// CHECK: [[AUMP:%.*]] = struct $AutoreleasingUnsafeMutablePointer<NSError> ([[PTR]] :
// CHECK: [[OPT_AUMP:%.*]] = enum $Optional<AutoreleasingUnsafeMutablePointer<NSError>>, #Optional.some!enumelt, [[AUMP]] : $AutoreleasingUnsafeMutablePointer<NSError>
// CHECK: apply {{%.*}}([[OPT_AUMP]],
// CHECK: } // end sil function '$s4main1fyyF'

func f() {
  let c = MyClass()
  do {
    try c.submit()
  } catch {}
}

f()
