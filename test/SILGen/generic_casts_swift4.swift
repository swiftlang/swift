
// RUN: %target-swift-emit-silgen -swift-version 4 -module-name generic_casts -Xllvm -sil-full-demangle -enable-sil-ownership %s | %FileCheck %s

// The below tests are to ensure we maintain compatibility with Swift 4.1's
// behaviour when casting to an archetype – the compiler assumes a non-optional
// archetype is non-optional, and therefore can unwrap the source.

// CHECK-LABEL: sil hidden @$s13generic_casts32optional_any_to_opaque_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_to_opaque_archetype<T>(_ x: Any?) -> T {
  return x as! T
  // CHECK: bb0([[RET:%.*]] : @trivial $*T, {{%.*}} : @trivial $*Optional<Any>):
  // CHECK: unconditional_checked_cast_addr Any in {{%.*}} : $*Any to T in [[RET]] : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts46optional_any_conditionally_to_opaque_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_conditionally_to_opaque_archetype<T>(_ x: Any?) -> T? {
  return x as? T
  // CHECK: checked_cast_addr_br take_always Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts32optional_any_is_opaque_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_is_opaque_archetype<T>(_ x: Any?, _: T) -> Bool {
  return x is T
  // CHECK: checked_cast_addr_br take_always Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts016optional_any_to_C17_opaque_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_to_optional_opaque_archetype<T>(_ x: Any?) -> T? {
  return x as! T?
  // CHECK: unconditional_checked_cast_addr Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts030optional_any_conditionally_to_C17_opaque_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_conditionally_to_optional_opaque_archetype<T>(_ x: Any?) -> T?? {
  return x as? T?
  // CHECK: checked_cast_addr_br take_always Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts016optional_any_is_C17_opaque_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_is_optional_opaque_archetype<T>(_ x: Any?, _: T) -> Bool {
  return x is T?
  //   Because the levels of optional are the same, 'is' doesn't transform into an 'as?',
  //   so we just cast directly without digging into the optional operand.
  // CHECK: checked_cast_addr_br take_always Optional<Any> in {{%.*}} : $*Optional<Any> to Optional<T> in {{%.*}} : $*Optional<T>
}

// CHECK-LABEL: sil hidden @$s13generic_casts31optional_any_to_class_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_to_class_archetype<T : AnyObject>(_ x: Any?) -> T {
  return x as! T
  // CHECK: unconditional_checked_cast_addr Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts45optional_any_conditionally_to_class_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_conditionally_to_class_archetype<T : AnyObject>(_ x: Any?) -> T? {
  return x as? T
  // CHECK: checked_cast_addr_br take_always Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

// CHECK-LABEL: sil hidden @$s13generic_casts31optional_any_is_class_archetype{{[_0-9a-zA-Z]*}}F
func optional_any_is_class_archetype<T : AnyObject>(_ x: Any?, _: T) -> Bool {
  return x is T
  // CHECK: checked_cast_addr_br take_always Any in {{%.*}} : $*Any to T in {{%.*}} : $*T
}

