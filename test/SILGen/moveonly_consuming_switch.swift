// RUN: %target-swift-frontend                                      \
// RUN:     -emit-silgen                                            \
// RUN:     %s                                                      \
// RUN: | %FileCheck %s

enum MaybeMaybeVoid<Wrapped: ~Copyable>: ~Copyable {
    case none(())
    case some(Wrapped)
}

// CHECK-LABEL: sil {{.*}}[ossa] @maybeMaybeVoid2Optional {{.*}} {
// CHECK:         [[STACK:%[^,]+]] = alloc_stack
// CHECK:         [[ADDR:%[^,]+]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[STACK]]
// CHECK:         [[ACCESS:%[^,]+]] = begin_access [read] [static] [no_nested_conflict] [[ADDR]]
// CHECK:         switch_enum_addr [[ACCESS]]
// CHECK-SAME:        case #MaybeMaybeVoid.none!enumelt: [[NONE_BLOCK:bb[0-9]+]]
// CHECK:       [[NONE_BLOCK]]:
// CHECK:         [[REGISTER_14:%[^,]+]] = tuple ()
// CHECK:         end_access [[ACCESS]]
// CHECK:         [[ACCESS_AGAIN:%[^,]+]] = begin_access [deinit] [static] [no_nested_conflict] [[ADDR]]
// CHECK:         [[NONE_ADDR:%[^,]+]] = unchecked_take_enum_data_addr [[ACCESS_AGAIN]]
// CHECK-SAME:        #MaybeMaybeVoid.none!enumelt
// Verify that the load is trivial.
// CHECK:         load [trivial] [[NONE_ADDR]]
// CHECK-LABEL: } // end sil function 'maybeMaybeVoid2Optional'
@_silgen_name("maybeMaybeVoid2Optional")
func maybeMaybeVoid2Optional<Wrapped: ~Copyable>(_ o2: consuming MaybeMaybeVoid<Wrapped>) -> Optional<Wrapped> {
  switch consume o2 {
  case .none(let void):
    return .none
  case .some(let wrapped):
    return .some(wrapped)
  }
}
