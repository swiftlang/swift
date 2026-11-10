// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name InlineMoves -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves.h
// RUN: %FileCheck %s < %t/moves.h
// RUN: %check-interop-cxx-header-in-clang(%t/moves.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %s -module-name InlineMoves -enable-library-evolution -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/moves-evo.h
// RUN: %FileCheck %s < %t/moves-evo.h
// RUN: %check-interop-cxx-header-in-clang(%t/moves-evo.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// The Bool supplies a nonzero sentinel while weak storage makes the value
// nontrivial without providing more extra inhabitants than the Bool.
@frozen public struct BoolWeak {
  public var ready: Bool
  public weak var ref: Ref?
  public init(_ ready: Bool, _ ref: Ref) {
    self.ready = ready
    self.ref = ref
  }
  public func isReady() -> Bool { ready }
  public func hasRef() -> Bool { ref?.id == 42 }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) BoolWeak final {
// CHECK: BoolWeak(BoolWeak &&other) noexcept {
// CHECK: if (other._isMovedFrom()) {
// CHECK: BoolWeak() noexcept { _setMovedFrom(); }
// CHECK: bool _isMovedFrom() const noexcept {
// CHECK-NEXT: return (static_cast<unsigned char>(_storage[0]) & 255) == 2;
// CHECK: void _setMovedFrom() noexcept {
// CHECK-NEXT: _storage[0] = static_cast<char>(2);

// Swift returns this indirectly, initializing the result over its moved-from
// state.
@frozen public struct LargeRef {
  public var ref: Ref
  public var a = 1, b = 2, c = 3, d = 4
  public init(_ ref: Ref) { self.ref = ref }
  public func check() -> Bool { ref.id == 42 && a + b + c + d == 10 }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) LargeRef final {
// CHECK: LargeRef(LargeRef &&other) noexcept {
// CHECK: bool _isMovedFrom() const noexcept {

// Weak optional references have no extra inhabitants, so moving this inline
// value still copies it.
@frozen public struct NoExtraInhabitants {
  public weak var ref: Ref?
  public init(_ ref: Ref) { self.ref = ref }
  public func check() -> Bool { ref?.id == 42 }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) NoExtraInhabitants final {
// CHECK-NOT: NoExtraInhabitants(NoExtraInhabitants &&other)
// CHECK-NOT: _isMovedFrom
// CHECK: alignas({{[0-9]+}}) char _storage[{{[0-9]+}}];
// CHECK-NEXT: friend class _impl::_impl_NoExtraInhabitants;

// The mask skips the first field, trailing field and padding.
@frozen public struct PaddedRef {
  public var prefix: UInt8
  public var ref: Ref
  public var suffix: UInt8
  public init(_ ref: Ref) {
    prefix = 7
    self.ref = ref
    suffix = 9
  }
  public func check() -> Bool { prefix == 7 && suffix == 9 && ref.id == 42 }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) PaddedRef final {
// CHECK: PaddedRef(PaddedRef &&other) noexcept {
// CHECK: bool _isMovedFrom() const noexcept {
// CHECK-NEXT: return (static_cast<unsigned char>(_storage[{{[1-9][0-9]*}}]) & 255) == 0 &&
// CHECK: void _setMovedFrom() noexcept {

private var destroyedReferences = 0
private var watchedReference: UnsafeRawPointer?
private var expectedReference: UInt = 0
private var watchedDeinitSawNewValue = false
public final class Ref {
  public let id: Int
  public init(_ id: Int) { self.id = id }
  deinit {
    destroyedReferences += 1
    if let reference = watchedReference {
      watchedReference = nil
      watchedDeinitSawNewValue =
          reference.load(as: UInt.self) == expectedReference
    }
  }
}

// An empty case consumes one reference extra inhabitant. The moved-from state
// must use a different one, including while constructing and projecting cases.
@frozen public enum ReferenceChoice {
  case value(Ref)
  case empty
  public func check() -> Bool {
    if case .value(let ref) = self { return ref.id == 42 }
    return false
  }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) ReferenceChoice final {
// CHECK: ReferenceChoice(ReferenceChoice &&other) noexcept {
// CHECK: bool _isMovedFrom() const noexcept {
// CHECK: void _setMovedFrom() noexcept {

@frozen public struct SingleRef {
  public var ref: Ref
  public init(_ ref: Ref) { self.ref = ref }
  public func check() -> Bool { ref.id == 42 }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) SingleRef final {
// CHECK: ~SingleRef() noexcept {
// CHECK-NEXT: if (_isMovedFrom()) return;
// CHECK: SingleRef(SingleRef &&other) noexcept {
// CHECK: SingleRef &operator =(SingleRef &&other) noexcept {
// CHECK: void *oldReference;
// CHECK-NEXT: memcpy(&oldReference, _storage, sizeof(oldReference));
// CHECK: other._setMovedFrom();
// CHECK-NEXT: }
// CHECK-NEXT: swift::_impl::swift_release(oldReference);
// CHECK: SingleRef(const SingleRef &other) noexcept {
// CHECK-NEXT: if (other._isMovedFrom()) {
// CHECK: void _setMovedFrom() noexcept {

// The moved-from state shares spare bits with the case tag, and C++ case
// constructors start from that state.
@frozen public enum SpareBitChoice {
  case first(Ref)
  case second(Ref)
  case neither
  public func check() -> Bool {
    if case .second(let ref) = self { return ref.id == 42 }
    return false
  }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) SpareBitChoice final {
// CHECK: SpareBitChoice(SpareBitChoice &&other) noexcept {
// CHECK: bool _isMovedFrom() const noexcept {

// Trivial wrappers keep their defaulted special members despite having extra
// inhabitants. Adding a user-provided move would change their C++ type traits.
@frozen public struct TrivialBool {
  public var value: Bool
  public init(_ value: Bool) { self.value = value }
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) TrivialBool final {
// CHECK: ~TrivialBool() noexcept = default;
// CHECK: TrivialBool(const TrivialBool &other) noexcept = default;
// CHECK-NOT: TrivialBool(TrivialBool &&other)
// CHECK-NOT: _isMovedFrom
// CHECK: alignas({{[0-9]+}}) char _storage[{{[0-9]+}}];
// CHECK-NEXT: friend class _impl::_impl_TrivialBool;

// The special members name their parameter 'other', and the moved-from check
// calls the C 'abort', so neither may pick up these Swift names instead.
@frozen public struct other {
  public var ref: Ref
  public var count = 0
  public init(_ ref: Ref) { self.ref = ref }
  public mutating func abort() {}
}

// CHECK-LABEL: class SWIFT_SYMBOL({{.*}}) other final {
// CHECK: *this = static_cast<class other>(other);
// CHECK: _getOpaquePointer() const noexcept { if (_isMovedFrom()) ::abort(); return _storage; }

public func borrow(_ value: borrowing PaddedRef) -> Int { Int(value.prefix) }
public func update(_ value: inout PaddedRef) { value.prefix += 1 }
public func borrowGeneric<T>(_ value: borrowing T) -> Int { MemoryLayout<T>.size }
public func updateGeneric<T>(_ value: inout T) {}
public func identity<T>(_ value: T) -> T { value }
public func consume(_ value: consuming SingleRef) -> Int { value.ref.id }
public func paddedStride() -> Int { MemoryLayout<PaddedRef>.stride }
public func boolWeakStride() -> Int { MemoryLayout<BoolWeak>.stride }
public func destroyedRefs() -> Int { destroyedReferences }

// The next 'Ref' deinit checks that 'value' already holds 'expected' at
// 'offset'.
public func watchReference(in value: UnsafeRawPointer, at offset: Int,
                           expecting expected: Ref) {
  watchedReference = value + offset
  expectedReference =
      UInt(bitPattern: Unmanaged.passUnretained(expected).toOpaque())
  watchedDeinitSawNewValue = false
}
public func deinitSawNewValue() -> Bool {
  watchedReference == nil && watchedDeinitSawNewValue
}
public func paddedRefOffset() -> Int {
  MemoryLayout<PaddedRef>.offset(of: \.ref)!
}
