// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend                                      \
// RUN:     -emit-silgen                                            \
// RUN:     %s                                                      \
// RUN: | %FileCheck %s

// Binding the payload of a non-Copyable enum in memory with `if case`,
// `guard case`, or `while let` consumes the entire enum, as a consuming
// `switch` does. SILGen enters a deinit access for the enum, so that the
// move-only checker doesn't treat the other cases' payloads as still live.

enum Maybe<Wrapped: ~Copyable>: ~Copyable {
  case none
  case some(Wrapped)
}

func use<T: ~Copyable>(_ t: consuming T) {}

// CHECK-LABEL: sil {{.*}}[ossa] @ifCase {{.*}} {
// CHECK:         [[ADDR:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable]
// CHECK:         switch_enum_addr [[ADDR]]
// CHECK-SAME:        case #Maybe.some!enumelt: [[SOME:bb[0-9]+]]
// CHECK:       [[SOME]]:
// CHECK-NEXT:    [[ACCESS:%[^,]+]] = begin_access [deinit] [static] [no_nested_conflict] [[ADDR]]
// CHECK-NEXT:    [[PAYLOAD:%[^,]+]] = unchecked_{{inplace|take}}_enum_data_addr [[ACCESS]]
// CHECK-SAME:        #Maybe.some!enumelt
// CHECK-NEXT:    copy_addr [take] [[PAYLOAD]] to [init]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK-LABEL: } // end sil function 'ifCase'
@_silgen_name("ifCase")
func ifCase<T: ~Copyable>(_ m: consuming Maybe<T>) {
  if case .some(let t) = consume m {
    use(t)
  }
}

// CHECK-LABEL: sil {{.*}}[ossa] @guardCase {{.*}} {
// CHECK:         [[ADDR:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable]
// CHECK:         switch_enum_addr [[ADDR]]
// CHECK-SAME:        case #Maybe.some!enumelt: [[SOME:bb[0-9]+]]
// CHECK:       [[SOME]]:
// CHECK-NEXT:    [[ACCESS:%[^,]+]] = begin_access [deinit] [static] [no_nested_conflict] [[ADDR]]
// CHECK-NEXT:    [[PAYLOAD:%[^,]+]] = unchecked_{{inplace|take}}_enum_data_addr [[ACCESS]]
// CHECK-NEXT:    copy_addr [take] [[PAYLOAD]] to [init]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK-LABEL: } // end sil function 'guardCase'
@_silgen_name("guardCase")
func guardCase<T: ~Copyable>(_ m: consuming Maybe<T>) {
  guard case .some(let t) = consume m else { return }
  use(t)
}

// CHECK-LABEL: sil {{.*}}[ossa] @whileLet {{.*}} {
// CHECK:         [[ADDR:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable]
// CHECK:         switch_enum_addr [[ADDR]]
// CHECK-SAME:        case #Optional.some!enumelt: [[SOME:bb[0-9]+]]
// CHECK:       [[SOME]]:
// CHECK-NEXT:    [[ACCESS:%[^,]+]] = begin_access [deinit] [static] [no_nested_conflict] [[ADDR]]
// CHECK-NEXT:    [[PAYLOAD:%[^,]+]] = unchecked_{{inplace|take}}_enum_data_addr [[ACCESS]]
// CHECK-NEXT:    copy_addr [take] [[PAYLOAD]] to [init]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK-LABEL: } // end sil function 'whileLet'
@_silgen_name("whileLet")
func whileLet<T: ~Copyable>(_ next: () -> T?) {
  while let t = next() {
    use(t)
  }
}

// A Copyable enum is not checked by the move-only checker, and gets no access.
// CHECK-LABEL: sil {{.*}}[ossa] @ifCaseCopyable {{.*}} {
// CHECK-NOT:     begin_access [deinit]
// CHECK-LABEL: } // end sil function 'ifCaseCopyable'
@_silgen_name("ifCaseCopyable")
func ifCaseCopyable<T>(_ m: T?) {
  if case .some(let t) = m {
    use(t)
  }
}
