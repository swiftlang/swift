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

// CHECK-LABEL: sil hidden @$s33coverage_property_wrapper_backing1UV1xSivpfP : $@convention(thin) (Int) -> Wrapper<Int>
// CHECK:       function_ref @$sSb6randomSbyFZ : $@convention(method) (@thin Bool.Type) -> Bool
// CHECK:       cond_br {{%[0-9]+}}, [[BB_TRUE:bb[0-9]+]], [[BB_FALSE:bb[0-9]+]]
//
// CHECK:       [[BB_FALSE]]:
// CHECK:       integer_literal {{.*}}, 2
//
// CHECK:       [[BB_TRUE]]:
// CHECK:       increment_profiler_counter 1
// CHECK:       integer_literal {{.*}}, 1

// CHECK-LABEL: sil hidden [transparent] @$s33coverage_property_wrapper_backing1UV2_{{.*}}7WrapperVySiGvpfi : $@convention(thin) () -> Int
// CHECK:       increment_profiler_counter 0
// CHECK:       function_ref @$sSb6randomSbyFZ : $@convention(method) (@thin Bool.Type) -> Bool
// CHECK:       cond_br {{%[0-9]+}}, [[BB_TRUE:bb[0-9]+]], [[BB_FALSE:bb[0-9]+]]

// CHECK:       [[BB_FALSE]]:
// CHECK:       integer_literal {{.*}}, 4
//
// CHECK:       [[BB_TRUE]]:
// CHECK:       increment_profiler_counter 1
// CHECK:       integer_literal {{.*}}, 3

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

// rdar://118939162 - Make sure we don't include the initialization expression
// in the backing initializer.
struct U {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_property_wrapper_backing.U.x
  // CHECK-NEXT:  [[@LINE+4]]:4  -> [[@LINE+4]]:30 : 0
  // CHECK-NEXT:  [[@LINE+3]]:24 -> [[@LINE+3]]:25 : 1
  // CHECK-NEXT:  [[@LINE+2]]:28 -> [[@LINE+2]]:29 : (0 - 1)
  // CHECK-NEXT:  }
  @Wrapper(.random() ? 1 : 2)
  var x = if .random() { 3 } else { 4 }
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_property_wrapper_backing.U.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:40 : 0
  // CHECK-NEXT:  [[@LINE-3]]:14 -> [[@LINE-3]]:23 : 0
  // CHECK-NEXT:  [[@LINE-4]]:24 -> [[@LINE-4]]:29 : 1
  // CHECK-NEXT:  [[@LINE-5]]:35 -> [[@LINE-5]]:40 : (0 - 1)
  // CHECK-NEXT:  }
}

struct V {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_property_wrapper_backing.V.x
  // CHECK-NEXT:  [[@LINE+2]]:4  -> [[@LINE+2]]:14 : 0
  // CHECK-NEXT:  }
  @Wrapper(0)
  var x = switch Bool.random() { case true: 0 case false: 0 }
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_property_wrapper_backing.V.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:62 : 0
  // CHECK-NEXT:  [[@LINE-3]]:18 -> [[@LINE-3]]:31 : 0
  // CHECK-NEXT:  [[@LINE-4]]:34 -> [[@LINE-4]]:46 : 1
  // CHECK-NEXT:  [[@LINE-5]]:47 -> [[@LINE-5]]:60 : 2
  // CHECK-NEXT:  }
}
