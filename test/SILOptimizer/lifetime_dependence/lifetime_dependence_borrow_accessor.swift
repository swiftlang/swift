// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature BorrowAndMutateAccessors

// Check which value each dependence is rooted at.
//
// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -target %target-swift-6.2-abi-triple \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature BorrowAndMutateAccessors \
// RUN:   -Xllvm -sil-print-after=lifetime-dependence-insertion 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowAndMutateAccessors

struct Wrapped {
  var span: Span<Int> {
    @_lifetime(borrow self)
    get { fatalError() }
  }
}

final class Holder {
  let _wrapped: Wrapped

  init(wrapped: Wrapped) { self._wrapped = wrapped }

  var wrapped: Wrapped {
    borrow { return _wrapped }
  }
}

// `Wrapped` above is empty, so the accessor's result is trivial and has no
// borrow introducers at all. A non-trivial result takes a different path -- the
// introducer walk reaches the accessor's `self` argument on its own -- so both
// have to be covered.
final class RichHolder {
  let _data = [1, 2, 3]

  var data: [Int] {
    borrow { return _data }
  }
}

// MARK: - Accepted

// The base is a guaranteed argument, so the accessor's result is valid for the
// whole function and the dependence roots at the argument.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test12fromArgumentys4SpanVySiGAA6HolderCF : $@convention(thin) (@guaranteed Holder) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $Holder):
// CHECK: [[WRAPPED:%.*]] = apply %{{.*}}(%0) : $@convention(method) (@guaranteed Holder) -> @guaranteed Wrapped
// CHECK: [[SPAN:%.*]] = apply %{{.*}}([[WRAPPED]]) : $@convention(method) (Wrapped) -> @lifetime(borrow 0) @owned Span<Int>
// CHECK: mark_dependence [unresolved] [[SPAN]] on %0
// CHECK-LABEL: } // end sil function '$s4test12fromArgumentys4SpanVySiGAA6HolderCF'
@_lifetime(borrow h)
func fromArgument(_ h: Holder) -> Span<Int> {
  return h.wrapped.span
}

// The reference reaches the accessor through a `struct_extract` of a borrowed
// struct. SILGen copies and borrows that reference just for the accessor call,
// so the dependence must root at the borrowed struct instead.
struct Outer {
  let holder: Holder

  // CHECK-LABEL: sil hidden [ossa] @$s4test5OuterV4spans4SpanVySiGvg : $@convention(method) (@guaranteed Outer) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $Outer):
  // CHECK: struct_extract %0, #Outer.holder
  // CHECK: [[SPAN:%.*]] = apply %{{.*}} : $@convention(method) (Wrapped) -> @lifetime(borrow 0) @owned Span<Int>
  // CHECK: mark_dependence [unresolved] [[SPAN]] on %0
  // CHECK-LABEL: } // end sil function '$s4test5OuterV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get {
      holder.wrapped.span
    }
  }
}

// A dependent value used within the function, rather than returned.
func localUse(_ h: Holder) -> Int {
  let span = h.wrapped.span
  return span.count
}

// The same two shapes with a non-trivial accessor result. These reach the
// accessor's `self` argument through the borrow introducer walk rather than
// through the trivial path, so they must root at the same place.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test8richFromys4SpanVySiGAA10RichHolderCF : $@convention(thin) (@guaranteed RichHolder) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $RichHolder):
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test8richFromys4SpanVySiGAA10RichHolderCF'
@_lifetime(borrow h)
func richFrom(_ h: RichHolder) -> Span<Int> {
  return h.data.span
}

struct RichOuter {
  let holder: RichHolder

  // CHECK-LABEL: sil hidden [ossa] @$s4test9RichOuterV4spans4SpanVySiGvg : $@convention(method) (@guaranteed RichOuter) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $RichOuter):
  // CHECK: struct_extract %0, #RichOuter.holder
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test9RichOuterV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get {
      holder.data.span
    }
  }
}

// rdar://191315943: SILGen borrows a noncopyable 'self' just for the borrow accessor call and copies the copyable
// element into a temporary, so the dependence must root at 'self' rather than at that temporary borrow.
struct TrivialElement {}

struct ElementWrapper: ~Copyable {
  let items: UniqueArray<TrivialElement>

  // CHECK-LABEL: sil hidden [ossa] @$s4test14ElementWrapperV5firsts3RefVyAA07TrivialB0VGvg : $@convention(method) (@guaranteed ElementWrapper) -> @lifetime(borrow 0) @owned Ref<TrivialElement> {
  // CHECK: bb0(%0 : @guaranteed $ElementWrapper):
  // CHECK: mark_dependence_addr [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test14ElementWrapperV5firsts3RefVyAA07TrivialB0VGvg'
  var first: Ref<TrivialElement> {
    Ref(items[0])
  }
}

// MARK: - Accessor conventions

// Each combination of the accessor's 'self' convention and result convention must root the dependence at the
// accessor's base. A base that is a stored property of a noncopyable struct is copied and borrowed just for the
// accessor call, and a loadable base of an @in_guaranteed accessor is stored to a temporary.

// @guaranteed self, @guaranteed result

struct GuaranteedBox {
  var _array: [Int]
  var array: [Int] {
    borrow { return _array }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test30guaranteedSelfGuaranteedResultys4SpanVySiGAA0D3BoxVF : $@convention(thin) (@guaranteed GuaranteedBox) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $GuaranteedBox):
// CHECK: apply %{{.*}} : $@convention(method) (@guaranteed GuaranteedBox) -> @guaranteed Array<Int>
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test30guaranteedSelfGuaranteedResultys4SpanVySiGAA0D3BoxVF'
@_lifetime(borrow b)
func guaranteedSelfGuaranteedResult(_ b: GuaranteedBox) -> Span<Int> {
  return b.array.span
}

// CHECK-LABEL: sil hidden [ossa] @$s4test33guaranteedSelfGuaranteedResultRefys0F0VySaySiGGAA0D3BoxVF : $@convention(thin) (@guaranteed GuaranteedBox) -> @lifetime(borrow 0) @owned Ref<Array<Int>> {
// CHECK: bb0(%0 : @guaranteed $GuaranteedBox):
// CHECK: apply %{{.*}} : $@convention(method) (@guaranteed GuaranteedBox) -> @guaranteed Array<Int>
// CHECK: mark_dependence_addr [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test33guaranteedSelfGuaranteedResultRefys0F0VySaySiGGAA0D3BoxVF'
@_lifetime(borrow b)
func guaranteedSelfGuaranteedResultRef(_ b: GuaranteedBox) -> Ref<[Int]> {
  return Ref(b.array)
}

// @in_guaranteed self, @guaranteed result

struct AddressOnlyBox {
  var _array: [Int]
  var _extra: Any
  var array: [Int] {
    borrow { return _array }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test016inGuaranteedSelfC6Resultys4SpanVySiGAA14AddressOnlyBoxVF : $@convention(thin) (@in_guaranteed AddressOnlyBox) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : $*AddressOnlyBox):
// CHECK: apply %{{.*}} : $@convention(method) (@in_guaranteed AddressOnlyBox) -> @guaranteed Array<Int>
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test016inGuaranteedSelfC6Resultys4SpanVySiGAA14AddressOnlyBoxVF'
@_lifetime(borrow b)
func inGuaranteedSelfGuaranteedResult(_ b: AddressOnlyBox) -> Span<Int> {
  return b.array.span
}

struct GenericArrayBox<T> {
  var _array: [Int]
  var _t: T
  var array: [Int] {
    borrow { return _array }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test016inGuaranteedSelfC15ResultTemporaryys4SpanVySiGAA15GenericArrayBoxVyyXlGF : $@convention(thin) (@guaranteed GenericArrayBox<AnyObject>) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $GenericArrayBox<AnyObject>):
// CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericArrayBox<τ_0_0>) -> @guaranteed Array<Int>
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test016inGuaranteedSelfC15ResultTemporaryys4SpanVySiGAA15GenericArrayBoxVyyXlGF'
@_lifetime(borrow b)
func inGuaranteedSelfGuaranteedResultTemporary(_ b: GenericArrayBox<AnyObject>) -> Span<Int> {
  return b.array.span
}

// CHECK-LABEL: sil hidden [ossa] @$s4test016inGuaranteedSelfC18ResultTemporaryRefys0G0VySaySiGGAA15GenericArrayBoxVyyXlGF : $@convention(thin) (@guaranteed GenericArrayBox<AnyObject>) -> @lifetime(borrow 0) @owned Ref<Array<Int>> {
// CHECK: bb0(%0 : @guaranteed $GenericArrayBox<AnyObject>):
// CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericArrayBox<τ_0_0>) -> @guaranteed Array<Int>
// CHECK: mark_dependence_addr [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test016inGuaranteedSelfC18ResultTemporaryRefys0G0VySaySiGGAA15GenericArrayBoxVyyXlGF'
@_lifetime(borrow b)
func inGuaranteedSelfGuaranteedResultTemporaryRef(_ b: GenericArrayBox<AnyObject>) -> Ref<[Int]> {
  return Ref(b.array)
}

struct GenericArrayBoxWrapper: ~Copyable {
  let box: GenericArrayBox<AnyObject>

  // CHECK-LABEL: sil hidden [ossa] @$s4test22GenericArrayBoxWrapperV4spans4SpanVySiGvg : $@convention(method) (@guaranteed GenericArrayBoxWrapper) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $GenericArrayBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericArrayBox<τ_0_0>) -> @guaranteed Array<Int>
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test22GenericArrayBoxWrapperV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get { box.array.span }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test22GenericArrayBoxWrapperV3refs3RefVySaySiGGvg : $@convention(method) (@guaranteed GenericArrayBoxWrapper) -> @lifetime(borrow 0) @owned Ref<Array<Int>> {
  // CHECK: bb0(%0 : @guaranteed $GenericArrayBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericArrayBox<τ_0_0>) -> @guaranteed Array<Int>
  // CHECK: mark_dependence_addr [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test22GenericArrayBoxWrapperV3refs3RefVySaySiGGvg'
  var ref: Ref<[Int]> {
    @_lifetime(borrow self)
    get { Ref(box.array) }
  }
}

// @guaranteed self, @guaranteed_address result

final class ClassBox<T> {
  let _value: T
  init(_ value: T) { _value = value }
  var value: T {
    borrow { return _value }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test37guaranteedSelfGuaranteedAddressResultys4SpanVySiGAA8ClassBoxCySaySiGGF : $@convention(thin) (@guaranteed ClassBox<Array<Int>>) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $ClassBox<Array<Int>>):
// CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed ClassBox<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test37guaranteedSelfGuaranteedAddressResultys4SpanVySiGAA8ClassBoxCySaySiGGF'
@_lifetime(borrow b)
func guaranteedSelfGuaranteedAddressResult(_ b: ClassBox<[Int]>) -> Span<Int> {
  return b.value.span
}

struct ClassBoxWrapper: ~Copyable {
  let box: ClassBox<[Int]>

  // CHECK-LABEL: sil hidden [ossa] @$s4test15ClassBoxWrapperV4spans4SpanVySiGvg : $@convention(method) (@guaranteed ClassBoxWrapper) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $ClassBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed ClassBox<τ_0_0>) -> @guaranteed_address τ_0_0
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test15ClassBoxWrapperV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get { box.value.span }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test15ClassBoxWrapperV3refs3RefVySaySiGGvg : $@convention(method) (@guaranteed ClassBoxWrapper) -> @lifetime(borrow 0) @owned Ref<Array<Int>> {
  // CHECK: bb0(%0 : @guaranteed $ClassBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@guaranteed ClassBox<τ_0_0>) -> @guaranteed_address τ_0_0
  // CHECK: mark_dependence_addr [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test15ClassBoxWrapperV3refs3RefVySaySiGGvg'
  var ref: Ref<[Int]> {
    @_lifetime(borrow self)
    get { Ref(box.value) }
  }
}

// @in_guaranteed self, @guaranteed_address result

struct GenericBox<T> {
  var _value: T
  var value: T {
    borrow { return _value }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test016inGuaranteedSelfC13AddressResultys4SpanVySiGAA10GenericBoxVySaySiGGF : $@convention(thin) (@guaranteed GenericBox<Array<Int>>) -> @lifetime(borrow 0) @owned Span<Int> {
// CHECK: bb0(%0 : @guaranteed $GenericBox<Array<Int>>):
// CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericBox<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test016inGuaranteedSelfC13AddressResultys4SpanVySiGAA10GenericBoxVySaySiGGF'
@_lifetime(borrow b)
func inGuaranteedSelfGuaranteedAddressResult(_ b: GenericBox<[Int]>) -> Span<Int> {
  return b.value.span
}

// CHECK-LABEL: sil hidden [ossa] @$s4test016inGuaranteedSelfC16AddressResultRefys0G0VySaySiGGAA10GenericBoxVyAEGF : $@convention(thin) (@guaranteed GenericBox<Array<Int>>) -> @lifetime(borrow 0) @owned Ref<Array<Int>> {
// CHECK: bb0(%0 : @guaranteed $GenericBox<Array<Int>>):
// CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericBox<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK: mark_dependence_addr [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test016inGuaranteedSelfC16AddressResultRefys0G0VySaySiGGAA10GenericBoxVyAEGF'
@_lifetime(borrow b)
func inGuaranteedSelfGuaranteedAddressResultRef(_ b: GenericBox<[Int]>) -> Ref<[Int]> {
  return Ref(b.value)
}

struct GenericBoxWrapper: ~Copyable {
  let box: GenericBox<[Int]>

  // CHECK-LABEL: sil hidden [ossa] @$s4test17GenericBoxWrapperV4spans4SpanVySiGvg : $@convention(method) (@guaranteed GenericBoxWrapper) -> @lifetime(borrow 0) @owned Span<Int> {
  // CHECK: bb0(%0 : @guaranteed $GenericBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) <τ_0_0> (@in_guaranteed GenericBox<τ_0_0>) -> @guaranteed_address τ_0_0
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test17GenericBoxWrapperV4spans4SpanVySiGvg'
  var span: Span<Int> {
    @_lifetime(borrow self)
    get { box.value.span }
  }
}

// @guaranteed self, @inout result

struct LoadablePointerBox {
  let _storage: UnsafeMutablePointer<[Int]>
  let _owner: AnyObject
  var array: [Int] {
    @_unsafeSelfDependentResult
    borrow { return _storage.pointee }
    @_unsafeSelfDependentResult
    nonmutating mutate { return &_storage.pointee }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test25guaranteedSelfInoutResultys11MutableSpanVySiGAA18LoadablePointerBoxVF : $@convention(thin) (@guaranteed LoadablePointerBox) -> @lifetime(borrow 0) @owned MutableSpan<Int> {
// CHECK: bb0(%0 : @guaranteed $LoadablePointerBox):
// CHECK: apply %{{.*}} : $@convention(method) (@guaranteed LoadablePointerBox) -> @inout Array<Int>
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test25guaranteedSelfInoutResultys11MutableSpanVySiGAA18LoadablePointerBoxVF'
@_lifetime(borrow b)
func guaranteedSelfInoutResult(_ b: LoadablePointerBox) -> MutableSpan<Int> {
  return b.array.mutableSpan
}

struct LoadablePointerBoxWrapper: ~Copyable {
  let box: LoadablePointerBox

  // CHECK-LABEL: sil hidden [ossa] @$s4test25LoadablePointerBoxWrapperV11mutableSpans07MutableG0VySiGvg : $@convention(method) (@guaranteed LoadablePointerBoxWrapper) -> @lifetime(borrow 0) @owned MutableSpan<Int> {
  // CHECK: bb0(%0 : @guaranteed $LoadablePointerBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) (@guaranteed LoadablePointerBox) -> @inout Array<Int>
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test25LoadablePointerBoxWrapperV11mutableSpans07MutableG0VySiGvg'
  var mutableSpan: MutableSpan<Int> {
    @_lifetime(borrow self)
    get { box.array.mutableSpan }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test25LoadablePointerBoxWrapperV10mutableRefs07MutableG0VySaySiGGvg : $@convention(method) (@guaranteed LoadablePointerBoxWrapper) -> @lifetime(borrow 0) @owned MutableRef<Array<Int>> {
  // CHECK: bb0(%0 : @guaranteed $LoadablePointerBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) (@guaranteed LoadablePointerBox) -> @inout Array<Int>
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test25LoadablePointerBoxWrapperV10mutableRefs07MutableG0VySaySiGGvg'
  var mutableRef: MutableRef<[Int]> {
    @_lifetime(borrow self)
    get { MutableRef(&box.array) }
  }
}

// @in_guaranteed self, @inout result

struct PointerBox {
  let _storage: UnsafeMutablePointer<[Int]>
  var _extra: Any
  var array: [Int] {
    @_unsafeSelfDependentResult
    borrow { return _storage.pointee }
    @_unsafeSelfDependentResult
    nonmutating mutate { return &_storage.pointee }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s4test27inGuaranteedSelfInoutResultys11MutableSpanVySiGAA10PointerBoxVF : $@convention(thin) (@in_guaranteed PointerBox) -> @lifetime(borrow 0) @owned MutableSpan<Int> {
// CHECK: bb0(%0 : $*PointerBox):
// CHECK: apply %{{.*}} : $@convention(method) (@in_guaranteed PointerBox) -> @inout Array<Int>
// CHECK: mark_dependence [unresolved] %{{.*}} on %0
// CHECK-LABEL: } // end sil function '$s4test27inGuaranteedSelfInoutResultys11MutableSpanVySiGAA10PointerBoxVF'
@_lifetime(borrow b)
func inGuaranteedSelfInoutResult(_ b: PointerBox) -> MutableSpan<Int> {
  return b.array.mutableSpan
}

struct PointerBoxWrapper: ~Copyable {
  let box: PointerBox

  // CHECK-LABEL: sil hidden [ossa] @$s4test17PointerBoxWrapperV10mutableRefs07MutableF0VySaySiGGvg : $@convention(method) (@in_guaranteed PointerBoxWrapper) -> @lifetime(borrow 0) @owned MutableRef<Array<Int>> {
  // CHECK: bb0(%0 : $*PointerBoxWrapper):
  // CHECK: apply %{{.*}} : $@convention(method) (@in_guaranteed PointerBox) -> @inout Array<Int>
  // CHECK: mark_dependence [unresolved] %{{.*}} on %0
  // CHECK-LABEL: } // end sil function '$s4test17PointerBoxWrapperV10mutableRefs07MutableF0VySaySiGGvg'
  var mutableRef: MutableRef<[Int]> {
    @_lifetime(borrow self)
    get { MutableRef(&box.array) }
  }
}

// MARK: - Rejected

// The base is a local: the accessor's result is only valid while that local is,
// so the dependent value cannot outlive the function. `other` gives the result
// something to depend on, so the only error is the escape itself.
@_lifetime(borrow other)
func escapingLocal(_ other: Holder) -> Span<Int> {
  let local = Holder(wrapped: Wrapped())
  return local.wrapped.span // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on the lifetime of variable 'local'}}
  // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
}

struct Composite: ~Copyable {
  var items: UniqueArray<UniqueArray<Int>>

  // Lifetime dependent value derived from a subscript implemented via borrow accessor with @guaranteed_address
  @_lifetime(borrow self)
  func check(at index: Int) -> Span<Int> {
    self.items[index].span
  }

  // Lifetime dependent value derived from a subscript implemented via mutate accessor
  @_lifetime(&self)
  mutating func checkMutable(at index: Int) -> MutableSpan<Int> {
    self.items[index].mutableSpan
  }
}
