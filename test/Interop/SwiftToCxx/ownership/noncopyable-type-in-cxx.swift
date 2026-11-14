// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Noncopyable -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/noncopyable.h
// RUN: %FileCheck --check-prefixes=CHECK,COMMON --implicit-check-not="bool _isMovedFrom =" %s < %t/noncopyable.h
// RUN: %check-interop-cxx-header-in-clang(%t/noncopyable.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// RUN: %target-swift-frontend %s -module-name Noncopyable -enable-library-evolution -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/noncopyable-evo.h
// RUN: %FileCheck --check-prefixes=BOXED,COMMON --implicit-check-not="bool _isMovedFrom =" %s < %t/noncopyable-evo.h
// RUN: %check-interop-cxx-header-in-clang(%t/noncopyable-evo.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_GenerateBindingsForNoncopyableTypesInCXX

public struct MoveOnlyStruct: ~Copyable {
    public let x: Int
    public let y: Int

    public init(x: Int) {
        self.x = x
        self.y = 0
    }

    deinit {
        print("destroy MoveOnlyStruct(\(x))")
    }

    public func getX() -> Int { return x }
}

public enum MoveOnlyEnum: ~Copyable {
    case a(Int)
    case b
    case c(Ref)
}

public func makeMoveOnlyStruct(_ x: Int) -> MoveOnlyStruct {
    return MoveOnlyStruct(x: x)
}

public func borrowMoveOnlyStruct(_ s: borrowing MoveOnlyStruct) -> Int {
    return s.getX()
}

public func consumeMoveOnlyStruct(_ s: consuming MoveOnlyStruct) {}

// A copyable type keeps its copy operations and gains no moved-from flag.
public struct CopyableStruct {
    public let x: Int
}

public final class Ref {
    public init() {}
    deinit { print("destroy Ref") }
}

public struct MoveOnlyWithRef: ~Copyable {
    public let r: Ref
    public init() { self.r = Ref() }
}

public func makeMoveOnlyWithRef() -> MoveOnlyWithRef { return MoveOnlyWithRef() }
public func consumeMoveOnlyWithRef(_ s: consuming MoveOnlyWithRef) {}

// More than four words, so it is passed and returned indirectly.
public struct BigMoveOnly: ~Copyable {
    public var a, b, c, d, e: Int

    public init(a: Int) {
        self.a = a
        b = 0; c = 0; d = 0; e = 0
    }

    deinit { print("destroy BigMoveOnly(\(a))") }

    public func getA() -> Int { return a }
}

public func makeBigMoveOnly(_ a: Int) -> BigMoveOnly { return BigMoveOnly(a: a) }
public func consumeBigMoveOnly(_ s: consuming BigMoveOnly) {}

// The layout depends on 'T', so the value is boxed.
public struct Unique<T>: ~Copyable {
    public var value: T

    public init(_ value: T) { self.value = value }

    deinit { print("destroy Unique") }

    public func get() -> T { return value }
    public mutating func set(_ newValue: T) { value = newValue }
}

// The layout has no extra inhabitants, so the value is boxed.
public struct Handle<T>: ~Copyable {
    public let raw: Int

    public init(raw: Int) { self.raw = raw }

    deinit { print("destroy Handle(\(raw))") }

    public func getRaw() -> Int { return raw }
}

public enum Slot<T>: ~Copyable {
    case full(T)
    case empty
}

public func makeUnique<T>(_ value: T) -> Unique<T> { return Unique(value) }
public func borrowUnique<T>(_ u: borrowing Unique<T>) -> T { return u.get() }
public func consumeUnique<T>(_ u: consuming Unique<T>) {}
public func consumeHandle<T>(_ h: consuming Handle<T>) {}

// A concrete specialization is passed directly.
public func makeUniqueInt(_ x: Int) -> Unique<Int> { return Unique(x) }
public func borrowUniqueInt(_ u: borrowing Unique<Int>) -> Int { return u.value }
public func consumeUniqueInt(_ u: consuming Unique<Int>) {}

// This fixed generic enum has no extra inhabitants and is boxed too.
public enum Flag<T>: ~Copyable {
    case on(Int)
    case off
}

// Only the Bool contributes to the extra-inhabitant mask. The Int must not
// participate in the moved-from check.
@frozen public struct ReadyHandle<T>: ~Copyable {
    public let raw: Int
    public let ready: Bool

    public init(raw: Int, ready: Bool) {
        self.raw = raw
        self.ready = ready
    }

    deinit { print("destroy ReadyHandle(\(raw))") }

    public func value() -> Int { return raw }
    public func isReady() -> Bool { return ready }
}

public func borrowReadyHandle<T>(_ h: borrowing ReadyHandle<T>) -> Int {
    return h.raw
}
public func consumeReadyHandle<T>(_ h: consuming ReadyHandle<T>) {}
public func readyHandleStride() -> Int {
    return MemoryLayout<ReadyHandle<Int>>.stride
}

// The checks below follow the order in which the types are emitted.

// Copyable types are unaffected.
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::CopyableStruct> = true;
// CHECK: class SWIFT_SYMBOL({{.*}}) CopyableStruct final {
// CHECK:   SWIFT_INLINE_THUNK ~CopyableStruct() noexcept = default;
// CHECK-NOT: _isMovedFrom
// CHECK:   SWIFT_INLINE_THUNK CopyableStruct(const CopyableStruct &other) noexcept = default;
// CHECK:   alignas({{[0-9]+}}) char _storage[{{[0-9]+}}];
// CHECK-NEXT:   friend class _impl::_impl_CopyableStruct;

// A generic noncopyable type is a move-only class template. This enum has no
// extra inhabitants, so its fixed layout is boxed.
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::Flag<T_0_0>> = false;
// CHECK: class SWIFT_SYMBOL({{.*}}) Flag final {
// CHECK:   Flag(const Flag &) = delete;
// CHECK:   SWIFT_INLINE_THUNK Flag(Flag &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// CHECK:   swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT:   friend class _impl::_impl_Flag<T_0_0>;

// A fixed layout without extra inhabitants uses boxed storage.
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::Handle<T_0_0>> = false;
// CHECK: class SWIFT_SYMBOL({{.*}}) Handle final {
// CHECK:   Handle(const Handle &) = delete;
// CHECK-NEXT:   Handle &operator =(const Handle &) = delete;
// CHECK-NEXT:   SWIFT_INLINE_THUNK Handle(Handle &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// CHECK:   swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT:   friend class _impl::_impl_Handle<T_0_0>;

// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::MoveOnlyEnum> = false;

// CHECK: class SWIFT_SYMBOL({{.*}}) MoveOnlyEnum final {
// CHECK:   SWIFT_INLINE_THUNK ~MoveOnlyEnum() noexcept {
// CHECK-NEXT:     if (_isMovedFrom()) return;
// CHECK:   MoveOnlyEnum(const MoveOnlyEnum &) = delete;
// CHECK-NEXT:   MoveOnlyEnum &operator =(const MoveOnlyEnum &) = delete;
// CHECK-NEXT:   SWIFT_INLINE_THUNK MoveOnlyEnum(MoveOnlyEnum &&other) noexcept {
// CHECK:   SWIFT_INLINE_THUNK MoveOnlyEnum operator()(swift::Int val) const;
// CHECK:   SWIFT_INLINE_THUNK bool isA() const;
// Projecting a payload out of a borrowed noncopyable enum would need a copy.
// CHECK-NOT: getA
// CHECK:   SWIFT_INLINE_THUNK bool isB() const;

// A fixed layout with no extra inhabitants uses a box instead of an added
// state member.
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::MoveOnlyStruct> = false;
// CHECK: class SWIFT_SYMBOL({{.*}}) MoveOnlyStruct final {
// CHECK:   SWIFT_INLINE_THUNK ~MoveOnlyStruct() noexcept {
// CHECK-NEXT:     if (!_storage.isAllocated()) return;
// CHECK:   MoveOnlyStruct(const MoveOnlyStruct &) = delete;
// CHECK-NEXT:   MoveOnlyStruct &operator =(const MoveOnlyStruct &) = delete;
// CHECK-NEXT:   SWIFT_INLINE_THUNK MoveOnlyStruct(MoveOnlyStruct &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// CHECK:   swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT:   friend class _impl::_impl_MoveOnlyStruct;

// A boxed value has no moved-from flag. A move transfers the box, and a
// moved-from object has none.
// BOXED: class SWIFT_SYMBOL({{.*}}) MoveOnlyStruct final {
// BOXED:   SWIFT_INLINE_THUNK ~MoveOnlyStruct() noexcept {
// BOXED-NEXT:     if (!_storage.isAllocated()) return;
// BOXED:   MoveOnlyStruct(const MoveOnlyStruct &) = delete;
// BOXED-NEXT:   MoveOnlyStruct &operator =(const MoveOnlyStruct &) = delete;
// BOXED-NEXT:   SWIFT_INLINE_THUNK MoveOnlyStruct(MoveOnlyStruct &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// BOXED-NEXT:   SWIFT_INLINE_THUNK MoveOnlyStruct &operator =(MoveOnlyStruct &&other) noexcept {
// BOXED:   SWIFT_INLINE_THUNK const char * _Nonnull _getOpaquePointer() const noexcept { return _storage.getOpaquePointer(); }
// BOXED:   swift::_impl::OpaqueStorage _storage;
// BOXED-NEXT:   friend class _impl::_impl_MoveOnlyStruct;

// A reference extra inhabitant supplies the inline moved-from state.
// CHECK: class SWIFT_SYMBOL({{.*}}) MoveOnlyWithRef final {
// CHECK:   SWIFT_INLINE_THUNK ~MoveOnlyWithRef() noexcept {
// CHECK-NEXT:     if (_isMovedFrom()) return;
// CHECK:   SWIFT_INLINE_THUNK MoveOnlyWithRef(MoveOnlyWithRef &&other) noexcept {
// CHECK-NEXT:     if (other._isMovedFrom()) {
// CHECK-NEXT:       _setMovedFrom();
// CHECK-NEXT:       return;
// CHECK-NEXT:     }
// CHECK:     vwTable->initializeWithTake(_storage, const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT:     other._setMovedFrom();
// CHECK:   SWIFT_INLINE_THUNK MoveOnlyWithRef &operator =(MoveOnlyWithRef &&other) noexcept {
// CHECK-NEXT:     if (this == &other) return *this;
// CHECK:     if (!_isMovedFrom())
// CHECK-NEXT:       vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK:     if (other._isMovedFrom()) {
// CHECK-NEXT:       _setMovedFrom();
// CHECK:     vwTable->initializeWithTake(_storage, const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT:     other._setMovedFrom();
// CHECK:   SWIFT_INLINE_THUNK const char * _Nonnull _getOpaquePointer() const noexcept { if (_isMovedFrom()) abort(); return _storage; }
// CHECK-NEXT:   SWIFT_INLINE_THUNK char * _Nonnull _getOpaquePointer() noexcept { if (_isMovedFrom()) abort(); return _storage; }
// CHECK:   bool _isMovedFrom() const noexcept {
// CHECK:   void _setMovedFrom() noexcept {
// CHECK:   friend class _impl::_impl_MoveOnlyWithRef;

// A Bool's first extra inhabitant is 2. Only that field's byte is checked,
// including with library evolution enabled for the frozen generic layout.
// COMMON: class SWIFT_SYMBOL({{.*}}) ReadyHandle final {
// COMMON:   ReadyHandle(const ReadyHandle &) = delete;
// COMMON:   ReadyHandle() noexcept { _setMovedFrom(); }
// COMMON:   alignas({{[0-9]+}}) char _storage[{{[0-9]+}}];
// COMMON:   bool _isMovedFrom() const noexcept {
// COMMON-NEXT:     return (static_cast<unsigned char>(_storage[[[READY_OFFSET:[1-9][0-9]*]]]) & 255) == 2;
// COMMON:   void _setMovedFrom() noexcept {
// COMMON-NEXT:     _storage[[[READY_OFFSET]]] = static_cast<char>(2);
// COMMON:   friend class _impl::_impl_ReadyHandle<T_0_0>;
// COMMON:   static {{.*}} ReadyHandle<T_0_0> returnNewValue(T callable) {
// COMMON:     callable(result._storage);

// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::Slot<T_0_0>> = false;
// CHECK: class SWIFT_SYMBOL({{.*}}) Slot final {
// CHECK:   SWIFT_INLINE_THUNK Slot(Slot &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// CHECK:   SWIFT_INLINE_THUNK bool isFull() const;
// CHECK-NOT: getFull
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::Unique<T_0_0>> = false;
// CHECK: class SWIFT_SYMBOL({{.*}}) Unique final {
// CHECK:   Unique(const Unique &) = delete;
// CHECK-NEXT:   Unique &operator =(const Unique &) = delete;
// CHECK-NEXT:   SWIFT_INLINE_THUNK Unique(Unique &&other) noexcept : _storage(static_cast<swift::_impl::OpaqueStorage &&>(other._storage)) {}
// CHECK:   swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT:   friend class _impl::_impl_Unique<T_0_0>;

// A borrowing parameter is still taken by const reference.
// CHECK: SWIFT_INLINE_THUNK swift::Int borrowMoveOnlyStruct(const MoveOnlyStruct& s) noexcept

// CHECK: SWIFT_INLINE_THUNK void consumeUnique(Unique<T_0_0>&& u) noexcept

// A concrete specialization is passed directly. Swift consumes the value, and
// C++ only frees the box.
// CHECK: SWIFT_INLINE_THUNK void consumeUniqueInt(Unique<swift::Int>&& u) noexcept
// CHECK-NEXT:   alignas(alignof(Unique<swift::Int>)) char copyBuffer_consumedParamCopy_u[sizeof(Unique<swift::Int>)];
// CHECK-NEXT:   auto &consumedParamCopy_u = *(new(copyBuffer_consumedParamCopy_u) Unique<swift::Int>(static_cast<Unique<swift::Int> &&>(u)));
// CHECK-NEXT:   swift::_impl::ConsumedValueStorageDestroyer<Unique<swift::Int>> storageGuard_consumedParamCopy_u(consumedParamCopy_u);
// CHECK-NEXT:   _impl::swift_interop_passDirect_Noncopyable_uint64_t_0_8(Noncopyable::_impl::_impl_Unique<swift::Int>::getOpaquePointer(consumedParamCopy_u))

// CHECK: SWIFT_INLINE_THUNK MoveOnlyStruct makeMoveOnlyStruct(swift::Int x) noexcept
