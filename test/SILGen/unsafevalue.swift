// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -Onone -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck -check-prefix=CANONICAL %s
// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -O -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck -check-prefix=OPT %s

import Swift

// Eventually element will be unconstrained, but for testing this builtin, we
// should use it this way.
@frozen
public struct UnsafeValue<Element: AnyObject> {
  @usableFromInline
  internal unowned(unsafe) var _value: Element

  // Create a new unmanaged value that owns the underlying value. This unmanaged
  // value must after use be deinitialized by calling the function deinitialize()
  //
  // This will insert a retain that the optimizer can not remove!
  @_transparent
  @inlinable
  public init(copying newValue: __shared Element) {
    Builtin.retain(newValue)
    _value = newValue
  }

  // Create a new unmanaged value that unsafely produces a new
  // unmanaged value without introducing any rr traffic.
  //
  // CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC : $@convention(method) <Element where Element : AnyObject> (@guaranteed Element, @thin UnsafeValue<Element>.Type) -> UnsafeValue<Element> {
  // CHECK: bb0([[INPUT_ELEMENT:%.*]] : @guaranteed $Element,
  // CHECK:   [[BOX:%.*]] = alloc_box
  // CHECK:   [[UNINIT_BOX:%.*]] = mark_uninitialized [rootself] [[BOX]]
  // CHECK:   [[BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[UNINIT_BOX]]
  // CHECK:   [[PROJECT_UNINIT_BOX:%.*]] = project_box [[BOX_LIFETIME]]
  // CHECK:   [[COPY_INPUT_ELEMENT:%.*]] = copy_value [[INPUT_ELEMENT]]
  // CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[PROJECT_UNINIT_BOX]]
  // CHECK:   [[STRUCT_ACCESS:%.*]] = struct_element_addr [[ACCESS]]
  // CHECK:   [[UNMANAGED_INPUT_ELEMENT:%.*]] = ref_to_unmanaged [[COPY_INPUT_ELEMENT]]
  // CHECK:   assign [[UNMANAGED_INPUT_ELEMENT]] to [[STRUCT_ACCESS]]
  // CHECK:   destroy_value [[COPY_INPUT_ELEMENT]]
  // CHECK:   end_access [[ACCESS]]
  // CHECK:   [[RESULT:%.*]] = load [trivial] [[PROJECT_UNINIT_BOX]]
  // CHECK:   destroy_value [[UNINIT_BOX]]
  // CHECK:   return [[RESULT]]
  // CHECK: } // end sil function '$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC'
  //
  // CANONICAL-LABEL: sil [transparent] @$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC : $@convention(method) <Element where Element : AnyObject> (@guaranteed Element, @thin UnsafeValue<Element>.Type) -> UnsafeValue<Element> {
  // CANONICAL: bb0([[INPUT_ELEMENT:%.*]] : $Element,
  // CANONICAL-NEXT: debug_value
  // CANONICAL-NEXT: strong_retain [[INPUT_ELEMENT]]
  // CANONICAL-NEXT: [[UNMANAGED_ELEMENT:%.*]] = ref_to_unmanaged [[INPUT_ELEMENT]]
  // CANONICAL-NEXT: strong_release [[INPUT_ELEMENT]]
  // CANONICAL-NEXT: [[RESULT:%.*]] = struct $UnsafeValue<Element> ([[UNMANAGED_ELEMENT]] : $@sil_unmanaged Element)
  // CANONICAL-NEXT: return [[RESULT]]
  // CANONICAL: } // end sil function '$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC'
  //
  // OPT-LABEL: sil [transparent] @$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC : $@convention(method) <Element where Element : AnyObject> (@guaranteed Element, @thin UnsafeValue<Element>.Type) -> UnsafeValue<Element> {
  // OPT: bb0([[INPUT_ELEMENT:%.*]] : $Element,
  // OPT-NEXT: debug_value
  // OPT-NEXT: [[UNMANAGED_ELEMENT:%.*]] = ref_to_unmanaged [[INPUT_ELEMENT]]
  // OPT-NEXT: [[RESULT:%.*]] = struct $UnsafeValue<Element> ([[UNMANAGED_ELEMENT]] : $@sil_unmanaged Element)
  // OPT-NEXT: return [[RESULT]]
  // OPT: } // end sil function '$s11unsafevalue11UnsafeValueV14unsafelyAssignACyxGxh_tcfC'
  @_transparent
  @inlinable
  public init(unsafelyAssign newValue: __shared Element) {
    _value = newValue
  }

  // Access the underlying value at +0, guaranteeing its lifetime by base.
  //
  // CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s11unsafevalue11UnsafeValueV20withGuaranteeingBase4base_qd_0_qd___qd_0_xXEtr0_lF :
  // CHECK: bb0([[RESULT:%.*]] : $*Result, [[BASE:%.*]] : $*Base, [[CLOSURE:%.*]] : @guaranteed $@noescape @callee_guaranteed {{.*}}, [[UNSAFE_VALUE:%.*]] : $UnsafeValue<Element>):
  // CHECK:  [[COPY_BOX:%.*]] = alloc_box
  // CHECK:  [[BOX_LIFETIME:%.*]] = begin_borrow [var_decl] [[COPY_BOX]]
  // CHECK:  [[COPY_PROJ:%.*]] = project_box [[BOX_LIFETIME]]
  // CHECK:  store [[UNSAFE_VALUE]] to [trivial] [[COPY_PROJ]]
  // CHECK:  [[CLOSUREC:%.*]] = copy_value [[CLOSURE]]
  // CHECK:  [[VALUE_ADDR:%.*]] = begin_access [read] [unknown] [[COPY_PROJ]]
  // CHECK:  [[STR_VALUE_ADDR:%.*]] = struct_element_addr [[VALUE_ADDR]]
  // CHECK:  [[UNMANAGED_VALUE:%.*]] = load [trivial] [[STR_VALUE_ADDR]]
  // CHECK:  [[UNOWNED_REF:%.*]] = unmanaged_to_ref [[UNMANAGED_VALUE]]
  // CHECK:  [[GUARANTEED_REF:%.*]] = unchecked_ownership_conversion [[UNOWNED_REF]]
  // CHECK:  [[GUARANTEED_REF_DEP_ON_BASE:%.*]] = mark_dependence [[GUARANTEED_REF]] : $Element on [[BASE]]
  // CHECK:  end_access [[VALUE_ADDR]]
  // CHECK:  [[CLOSUREB:%.*]] = begin_borrow [[CLOSUREC]]
  // CHECK:  apply [[CLOSUREB]]([[RESULT]], [[GUARANTEED_REF_DEP_ON_BASE]])
  // CHECK:  end_borrow [[GUARANTEED_REF]]
  // CHECK:  destroy_value [[COPY_BOX]]
  // CHECK: } // end sil function '$s11unsafevalue11UnsafeValueV20withGuaranteeingBase4base_qd_0_qd___qd_0_xXEtr0_lF'
  //
  // CANONICAL-LABEL: sil [transparent] @$s11unsafevalue11UnsafeValueV20withGuaranteeingBase4base_qd_0_qd___qd_0_xXEtr0_lF :
  // CANONICAL: bb0([[RESULT:%.*]] : $*Result, [[BASE:%.*]] : $*Base, [[CLOSURE:%.*]] : $@noescape @callee_guaranteed {{.*}}, [[UNSAFE_VALUE:%.*]] : $UnsafeValue<Element>):
  // CANONICAL:  [[UNMANAGED_VALUE:%.*]] = struct_extract [[UNSAFE_VALUE]]
  // CANONICAL:  [[UNOWNED_REF:%.*]] = unmanaged_to_ref [[UNMANAGED_VALUE]]
  // CANONICAL:  [[GUARANTEED_REF_DEP_ON_BASE:%.*]] = mark_dependence [[UNOWNED_REF]] : $Element on [[BASE]]
  // CANONICAL:  apply [[CLOSURE]]([[RESULT]], [[GUARANTEED_REF_DEP_ON_BASE]])
  // CANONICAL: } // end sil function '$s11unsafevalue11UnsafeValueV20withGuaranteeingBase4base_qd_0_qd___qd_0_xXEtr0_lF'
  //
  // OPT-LABEL: sil [transparent] {{.*}}@$s11unsafevalue11UnsafeValueV20withGuaranteeingBase4base_qd_0_qd___qd_0_xXEtr0_lF :
  // OPT: bb0([[RESULT:%.*]] : $*Result, [[BASE:%.*]] : $*Base, [[CLOSURE:%.*]] : $@noescape @callee_guaranteed {{.*}}, [[UNSAFE_VALUE:%.*]] : $UnsafeValue<Element>):
  // OPT:  [[UNMANAGED_VALUE:%.*]] = struct_extract [[UNSAFE_VALUE]]
  // OPT:  [[UNOWNED_REF:%.*]] = unmanaged_to_ref [[UNMANAGED_VALUE]]
  // OPT:  [[GUARANTEED_REF_DEP_ON_BASE:%.*]] = mark_dependence [[UNOWNED_REF]] : $Element on [[BASE]]
  // OPT:  apply [[CLOSURE]]([[RESULT]], [[GUARANTEED_REF_DEP_ON_BASE]])
  // OPT: } // end sil function '$s11unsafevalue11UnsafeValueV20withGuaranteeingBase4base_qd_0_qd___qd_0_xXEtr0_lF'
  @_transparent
  @inlinable
  func withGuaranteeingBase<Base, Result>(base: Base, _ f: (Element) -> Result) -> Result {
    // Just so we can not mark self as mutating. This is just a bitwise copy.
    var tmp = self
    return f(Builtin.convertUnownedUnsafeToGuaranteed(base, &tmp._value))
  }

  @_transparent
  @inlinable
  func assumingGuaranteeingBase<Result>(_ f: (Element) -> Result) -> Result {
    // Just so we can not mark self as mutating. This is just a bitwise copy.
    let fakeBase: Int? = nil
    return withGuaranteeingBase(base: fakeBase, f)
  }

  // If the unmanaged value was initialized with a copy, release the underlying value.
  //
  // This will insert a release that can not be removed by the optimizer.
  @_transparent
  @inlinable
  mutating func deinitialize() {
    Builtin.release(_value)
  }

  // Return a new strong reference to the unmanaged value.
  //
  // This will insert a retain that can not be removed by the optimizer!
  @_transparent
  @inlinable
  public var strongRef: Element { _value }
}
