// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_property_wrapper_backing %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  init(wrappedValue: T, _ x: Int) {
    self.wrappedValue = wrappedValue
  }
}

// rdar://99931619 – Make sure we emit the profiler increment for the backing
// initializer.

// CHECK-LABEL: sil hidden @$s33coverage_property_wrapper_backing1SV1iSivpfP : $@convention(thin) (Int) -> Wrapper<Int>
// CHECK:       increment_profiler_counter 0
// CHECK:       function_ref @$sSb6randomSbyFZ
// CHECK:       cond_br {{%[0-9]+}}, [[BB:bb[0-9]]]
// CHECK:       [[BB]]:
// CHECK-NEXT:  increment_profiler_counter 1

struct S {
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s33coverage_property_wrapper_backing1SV1iSivpfP" {{.*}} // property wrapper backing initializer of coverage_property_wrapper_backing.S.i
  // CHECK-NEXT:  [[@LINE+4]]:4 -> [[@LINE+4]]:30 : 0
  // CHECK-NEXT:  [[@LINE+3]]:24 -> [[@LINE+3]]:25 : 1
  // CHECK-NEXT:  [[@LINE+2]]:28 -> [[@LINE+2]]:29 : (0 - 1)
  // CHECK-NEXT:  }
  @Wrapper(.random() ? 1 : 2)
  var i = 3
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s33coverage_property_wrapper_backing1SV2_i{{.*}}" {{.*}} // variable initialization expression of coverage_property_wrapper_backing.S.(_i in {{.*}}) : coverage_property_wrapper_backing.Wrapper<Swift.Int>
  // CHECK-NEXT: [[@LINE-2]]:11 -> [[@LINE-2]]:12 : 0
  // CHECK-NEXT: }
}

// FIXME(rdar://99962285): This is currently needed to SILGen the property
// initializer for 'i'.
_ = S().i
