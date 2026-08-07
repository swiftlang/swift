// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Lifetimes -enable-experimental-feature Lifetimes -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/lifetimes.h
// RUN: %FileCheck %s < %t/lifetimes.h
// RUN: %check-interop-cxx-header-in-clang(%t/lifetimes.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_Lifetimes

public struct Owner {
    public var data: Int64 = 0
    public init() {}
}

public struct View: ~Escapable {
    var p: UnsafePointer<Int64>
    @_lifetime(borrow o)
    init(_ o: borrowing Owner, _ p: UnsafePointer<Int64>) { self.p = p }
    @_lifetime(copy other)
    init(_ other: borrowing View) { self.p = other.p }
}

@_lifetime(borrow o)
public func borrowDefaultParam(_ o: Owner) -> View { View(o, o.address) }

@_lifetime(borrow o)
public func borrowBorrowingParam(_ o: borrowing Owner) -> View { View(o, o.address) }

@_lifetime(&o)
public func borrowInoutParam(_ o: inout Owner) -> View { View(o, o.address) }

// A copy dependency ties the result to the lifetime the argument depends on,
// not to the argument's storage; the argument is passed as a reference in C++,
// so the dependency cannot be expressed with 'lifetimebound'.
@_lifetime(copy v)
public func copyDefaultParam(_ v: View) -> View { View(v) }

@_lifetime(copy v)
public func copyBorrowingParam(_ v: borrowing View) -> View { View(v) }

@_lifetime(&v)
public func borrowInoutViewParam(_ v: inout View) -> View { View(v) }

@_lifetime(borrow o)
public func borrowSecondParam(_ ignored: Int64, _ o: Owner) -> View { View(o, o.address) }

extension Owner {
    var address: UnsafePointer<Int64> {
        withUnsafePointer(to: self) {
            UnsafeRawPointer($0).assumingMemoryBound(to: Int64.self)
        }
    }

    @_lifetime(borrow self)
    public func borrowSelf() -> View { View(self, address) }

    @_lifetime(&self)
    public mutating func borrowSelfMutating() -> View { View(self, address) }

    public var viewProperty: View {
        @_lifetime(borrow self)
        get { View(self, address) }
    }
}

// CHECK: SWIFT_INLINE_THUNK View borrowSelf() const SWIFT_SELF_LIFETIMEBOUND SWIFT_SYMBOL({{.*}});
// CHECK: SWIFT_INLINE_THUNK View borrowSelfMutating() SWIFT_SELF_LIFETIMEBOUND SWIFT_SYMBOL({{.*}});
// CHECK: SWIFT_INLINE_THUNK View getViewProperty() const SWIFT_SELF_LIFETIMEBOUND SWIFT_SYMBOL({{.*}});

// CHECK: SWIFT_INLINE_THUNK View borrowBorrowingParam(const Owner& o SWIFT_LIFETIMEBOUND) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK View borrowDefaultParam(const Owner& o SWIFT_LIFETIMEBOUND) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK View borrowInoutParam(Owner& o SWIFT_LIFETIMEBOUND) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK View borrowInoutViewParam(View& v SWIFT_LIFETIMEBOUND) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK View borrowSecondParam(int64_t ignored, const Owner& o SWIFT_LIFETIMEBOUND) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK View copyBorrowingParam(const View& v) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK: SWIFT_INLINE_THUNK View copyDefaultParam(const View& v) noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {

// The attribute is repeated on the out-of-line definitions of the members.
// CHECK: SWIFT_INLINE_THUNK View Owner::borrowSelf() const SWIFT_SELF_LIFETIMEBOUND {
// CHECK: SWIFT_INLINE_THUNK View Owner::borrowSelfMutating() SWIFT_SELF_LIFETIMEBOUND {
// CHECK: SWIFT_INLINE_THUNK View Owner::getViewProperty() const SWIFT_SELF_LIFETIMEBOUND {
