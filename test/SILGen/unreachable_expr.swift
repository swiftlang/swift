// RUN: %target-swift-emit-silgen %s -verify -sil-verify-all | %FileCheck %s --check-prefixes CHECK,REG
// RUN: %target-swift-emit-silgen %s -verify -sil-verify-all -enable-sil-opaque-values | %FileCheck %s --check-prefixes CHECK,OV

// CHECK-LABEL:   sil{{.*}} [ossa] @{{.*}}uninhabited_generic{{.*}}
// CHECK:           [[NEVER:%[^,]+]] = apply {{.*}} -> Never
// CHECK-NEXT:      ignored_use [[NEVER]]
// CHECK-NEXT:      unreachable
//
// CHECK:         bb1:
// CHECK-NOT:         Preds

            // Without opaque values, take from an undef address,
            // through a temporary alloc, to eventually the out-parameter %0
// REG-NEXT:      [[BOGUS_ALLOC:%.*]] = alloc_stack $T
// REG-NEXT:      copy_addr [take] undef to [init] [[BOGUS_ALLOC]]
// REG-NEXT:      copy_addr [take] [[BOGUS_ALLOC]] to [init] %0

            // With opaque values, simply return the undef value
// OV-NEXT:       return undef : $T
func uninhabited_generic<T>() -> T { fatalError("todo") }
