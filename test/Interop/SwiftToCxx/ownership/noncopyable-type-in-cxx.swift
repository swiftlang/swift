// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Noncopyable -enable-experimental-feature GenerateBindingsForNoncopyableTypesInCXX -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/noncopyable.h
// RUN: %FileCheck %s < %t/noncopyable.h
// RUN: %check-interop-cxx-header-in-clang(%t/noncopyable.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

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

// The checks below follow the order in which the types are emitted.

// Copyable types are unaffected.
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::CopyableStruct> = true;
// CHECK: class SWIFT_SYMBOL({{.*}}) CopyableStruct final {
// CHECK:   SWIFT_INLINE_THUNK ~CopyableStruct() noexcept {
// CHECK-NOT:     if (_isMovedFrom) return;
// CHECK:   SWIFT_INLINE_THUNK CopyableStruct(const CopyableStruct &other) noexcept {
// CHECK:   alignas(8) char _storage[8];
// CHECK-NEXT:   friend class _impl::_impl_CopyableStruct;

// A noncopyable type cannot be used as a Swift generic argument yet.
// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::MoveOnlyEnum> = false;

// CHECK: class SWIFT_SYMBOL({{.*}}) MoveOnlyEnum final {
// CHECK:   SWIFT_INLINE_THUNK ~MoveOnlyEnum() noexcept {
// CHECK-NEXT:     if (_isMovedFrom) return;
// CHECK:   MoveOnlyEnum(const MoveOnlyEnum &) = delete;
// CHECK-NEXT:   MoveOnlyEnum &operator =(const MoveOnlyEnum &) = delete;
// CHECK-NEXT:   SWIFT_INLINE_THUNK MoveOnlyEnum(MoveOnlyEnum &&other) noexcept {
// CHECK:   SWIFT_INLINE_THUNK MoveOnlyEnum operator()(swift::Int val) const;
// CHECK:   SWIFT_INLINE_THUNK bool isA() const;
// Projecting a payload out of a borrowed noncopyable enum would need a copy.
// CHECK-NOT: getA
// CHECK:   SWIFT_INLINE_THUNK bool isB() const;

// CHECK: inline const constexpr bool isUsableInGenericContext<Noncopyable::MoveOnlyStruct> = false;

// A noncopyable type is exposed as a move-only C++ class whose destructor is a
// no-op once the value has been moved out of it.
// CHECK: class SWIFT_SYMBOL({{.*}}) MoveOnlyStruct final {
// CHECK:   SWIFT_INLINE_THUNK ~MoveOnlyStruct() noexcept {
// CHECK-NEXT:     if (_isMovedFrom) return;
// CHECK:     vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:   }
// CHECK-NEXT:   MoveOnlyStruct(const MoveOnlyStruct &) = delete;
// CHECK-NEXT:   MoveOnlyStruct &operator =(const MoveOnlyStruct &) = delete;
// CHECK-NEXT:   SWIFT_INLINE_THUNK MoveOnlyStruct(MoveOnlyStruct &&other) noexcept {
// CHECK-NEXT:     if (other._isMovedFrom) {
// CHECK-NEXT:       _isMovedFrom = true;
// CHECK-NEXT:       return;
// CHECK-NEXT:     }
// CHECK:     vwTable->initializeWithTake(_getOpaquePointer(), const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT:     other._isMovedFrom = true;
// CHECK-NEXT:   }

// The destination of a move assignment may itself be moved from, as in
// 'std::swap'.
// CHECK-NEXT:   SWIFT_INLINE_THUNK MoveOnlyStruct &operator =(MoveOnlyStruct &&other) noexcept {
// CHECK-NEXT:     if (this == &other) return *this;
// CHECK:     if (!_isMovedFrom)
// CHECK-NEXT:       vwTable->destroy(_getOpaquePointer(), metadata._0);
// CHECK-NEXT:     if (other._isMovedFrom) {
// CHECK-NEXT:       _isMovedFrom = true;
// CHECK-NEXT:       return *this;
// CHECK-NEXT:     }
// CHECK-NEXT:     vwTable->initializeWithTake(_getOpaquePointer(), const_cast<char *>(other._getOpaquePointer()), metadata._0);
// CHECK-NEXT:     _isMovedFrom = false;
// CHECK-NEXT:     other._isMovedFrom = true;
// CHECK-NEXT:   return *this;
// CHECK-NEXT:   }

// The moved-from flag is stored after the Swift value, so that the address of
// the C++ object is still the address of the Swift value.
// CHECK:   alignas(8) char _storage[16];
// CHECK-NEXT:   bool _isMovedFrom = false;

// A borrowing parameter is still taken by const reference.
// CHECK: SWIFT_INLINE_THUNK swift::Int borrowMoveOnlyStruct(const MoveOnlyStruct& s) noexcept

// CHECK: SWIFT_INLINE_THUNK MoveOnlyStruct makeMoveOnlyStruct(swift::Int x) noexcept
