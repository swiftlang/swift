// This uses '-primary-file' to ensure we're conservative with lazy SIL emission.
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_property_wrapper_backing -primary-file %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  init(wrappedValue: T, _ x: Int) {
    self.wrappedValue = wrappedValue
  }
}

@propertyWrapper
struct PassThroughWrapper<T> {
  var wrappedValue: T
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

struct T {
  // The backing initializer for j has no user-written code, so no coverage map needed.
  // CHECK-NOT: sil_coverage_map {{.*}} "s33coverage_property_wrapper_backing1TV1jSivpfP"
  @PassThroughWrapper
  var j = 3

  // FIXME: Ideally the region here would only span the length of the @Wrapper
  // argument list.
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s33coverage_property_wrapper_backing1TV1kSivpfP" {{.*}} // property wrapper backing initializer of coverage_property_wrapper_backing.T.k
  // CHECK-NEXT:  [[@LINE+4]]:4 -> [[@LINE+5]]:30 : 0
  // CHECK-NEXT:  [[@LINE+4]]:24 -> [[@LINE+4]]:25 : 1
  // CHECK-NEXT:  [[@LINE+3]]:28 -> [[@LINE+3]]:29 : (0 - 1)
  // CHECK-NEXT:  }
  @PassThroughWrapper
  @Wrapper(.random() ? 1 : 2)
  var k = 3
}
