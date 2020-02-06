// RUN: %target-swift-emit-silgen -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s
// RUN: %target-swift-emit-sil -Onone -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck -check-prefix=CANONICAL %s
// RUN: %target-swift-emit-sil -O -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck -check-prefix=OPT %s

import Swift

// Eventually element will be unconstrained, but for testing this builtin, we
// should use it this way.
@frozen
public struct UnsafeValue<Element: AnyObject> {
  @usableFromInline
  internal unowned(unsafe) var _value: Element?

  @_transparent
  @inlinable
  public init() {
    _value = nil
  }

  // Create a new unmanaged value that owns the underlying value. This unmanaged
  // value must after use be deinitialized by calling the function deinitialize()
  //
  // This will insert a retain that the optimizer can not remove!
  @_transparent
  @inlinable
  public init(copying newValue: __shared Element) {
    self.init()
    _value = newValue
  }

  // Create a new unmanaged value that unsafely produces a new
  // unmanaged value without introducing any rr traffic.
  //
  // CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC : $@convention(method) <Element where Element : AnyObject> (@guaranteed Element, @thin UnsafeValue<Element>.Type) -> UnsafeValue<Element> {
  // CHECK: bb0([[INPUT_ELEMENT:%.*]] : @guaranteed $Element,
  // CHECK:   [[BOX:%.*]] = alloc_box
  // CHECK:   [[UNINIT_BOX:%.*]] = mark_uninitialized [delegatingself] [[BOX]]
  // CHECK:   [[PROJECT_UNINIT_BOX:%.*]] = project_box [[UNINIT_BOX]]
  // CHECK:   [[DELEGATING_INIT_RESULT:%.*]] = apply {{%.*}}<Element>(
  // CHECK:   assign [[DELEGATING_INIT_RESULT]] to [[PROJECT_UNINIT_BOX]]
  // CHECK:   [[ACCESS:%.*]] = begin_access [read] [unknown] [[PROJECT_UNINIT_BOX]]
  // CHECK:   [[STRUCT_ACCESS:%.*]] = struct_element_addr [[ACCESS]]
  // CHECK:   [[OPT_INPUT_ELEMENT:%.*]] = enum $Optional<Element>, #Optional.some!enumelt.1, [[INPUT_ELEMENT]]
  // CHECK:   [[UNMANAGED_OPT_INPUT_ELEMENT:%.*]] = ref_to_unmanaged [[OPT_INPUT_ELEMENT]]
  // CHECK:   store [[UNMANAGED_OPT_INPUT_ELEMENT]] to [trivial] [[STRUCT_ACCESS]]
  // CHECK:   end_access [[ACCESS]]
  // CHECK:   [[RESULT:%.*]] = load [trivial] [[PROJECT_UNINIT_BOX]]
  // CHECK:   destroy_value [[UNINIT_BOX]]
  // CHECK:   return [[RESULT]]
  // CHECK: } // end sil function '$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC'
  //
  // CANONICAL-LABEL: sil [transparent] [serialized] @$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC : $@convention(method) <Element where Element : AnyObject> (@guaranteed Element, @thin UnsafeValue<Element>.Type) -> UnsafeValue<Element> {
  // CANONICAL: bb0([[INPUT_ELEMENT:%.*]] : $Element,
  // CANONICAL-NEXT: debug_value
  // TODO(gottesmm): release_value on a .none shouldn't exist.
  // CANONICAL-NEXT: [[NONE:%.*]] = enum $Optional<Element>, #Optional.none!enumelt
  // CANONICAL-NEXT: release_value [[NONE]]
  // CANONICAL-NEXT: [[ENUM:%.*]] = enum $Optional<Element>, #Optional.some!enumelt.1, [[INPUT_ELEMENT]]
  // CANONICAL-NEXT: [[UNMANAGED_ENUM:%.*]] = ref_to_unmanaged [[ENUM]]
  // CANONICAL-NEXT: [[RESULT:%.*]] = struct $UnsafeValue<Element> ([[UNMANAGED_ENUM]] : $@sil_unmanaged Optional<Element>)
  // CANONICAL-NEXT: tuple
  // CANONICAL-NEXT: return [[RESULT]]
  // CANONICAL: } // end sil function '$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC'
  //
  // OPT-LABEL: sil [transparent] @$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC : $@convention(method) <Element where Element : AnyObject> (@guaranteed Element, @thin UnsafeValue<Element>.Type) -> UnsafeValue<Element> {
  // OPT: bb0([[INPUT_ELEMENT:%.*]] : $Element,
  // OPT-NEXT: debug_value
  // OPT-NEXT: [[ENUM:%.*]] = enum $Optional<Element>, #Optional.some!enumelt.1, [[INPUT_ELEMENT]]
  // OPT-NEXT: [[UNMANAGED_ENUM:%.*]] = ref_to_unmanaged [[ENUM]]
  // OPT-NEXT: [[RESULT:%.*]] = struct $UnsafeValue<Element> ([[UNMANAGED_ENUM]] : $@sil_unmanaged Optional<Element>)
  // OPT-NEXT: return [[RESULT]]
  // OPT: } // end sil function '$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC'
  @_transparent
  @inlinable
  public init(unsafelyAssign newValue: __shared Element) {
    self.init()
    Builtin.convertStrongToUnownedUnsafe(newValue, &_value)
  }

  // Access the underlying value at +0, guaranteeing its lifetime by base.
  @_transparent
  @inlinable
  func withGuaranteeingBase<Base>(base: Base, f: (Element) -> ()) {
    // TODO: Once we have a builtin for mark_dependence, fill this in.
  }

  // If the unmanaged value was initialized with a copy, release the underlying value.
  //
  // This will insert a release that can not be removed by the optimizer.
  @_transparent
  @inlinable
  mutating func deinitialize() {
    _value = nil
  }

  // Return a new strong reference to the unmanaged value.
  //
  // This will insert a retain that can not be removed by the optimizer!
  @_transparent
  @inlinable
  public var strongRef: Element { _value.unsafelyUnwrapped }
}
