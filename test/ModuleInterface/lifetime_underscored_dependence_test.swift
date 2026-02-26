// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -emit-module \
// RUN:     -enable-experimental-feature Lifetimes \
// RUN:     -o %t/lifetime_underscored_dependence.swiftmodule \
// RUN:     -emit-module-interface-path %t/lifetime_underscored_dependence.swiftinterface \
// RUN:     %S/Inputs/lifetime_underscored_dependence.swift

// Check the interfaces

// RUN: %FileCheck %s --input-file %t/lifetime_underscored_dependence.swiftinterface

// See if we can compile a module through just the interface and typecheck using it.

// RUN: %target-swift-frontend -compile-module-from-interface \
// RUN:    -enable-experimental-feature Lifetimes \
// RUN:    %t/lifetime_underscored_dependence.swiftinterface -o %t/lifetime_underscored_dependence.swiftmodule

// RUN: %target-swift-frontend -typecheck -I %t %s \
// RUN:    -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

import lifetime_underscored_dependence
// CHECK:  #if compiler(>=5.3) && $Lifetimes
// CHECK:  @_lifetime(borrow a)
// CHECK:  @inlinable internal init(_ ptr: Swift::UnsafeRawBufferPointer, _ a: borrowing Swift::Array<Swift::Int>) {
// CHECK:    let bv = BufferView(ptr, a.count)
// CHECK:    self = _overrideLifetime(bv, borrowing: a)
// CHECK:  }
// CHECK:  #else
// CHECK:  @lifetime(borrow a)
// CHECK:  @inlinable internal init(_ ptr: Swift::UnsafeRawBufferPointer, _ a: borrowing Swift::Array<Swift::Int>) {
// CHECK:    let bv = BufferView(ptr, a.count)
// CHECK:    self = _overrideLifetime(bv, borrowing: a)
// CHECK:  }
// CHECK:  #endif

// CHECK:  #if compiler(>=5.3) && $Lifetimes
// CHECK:  @_lifetime(copy a)
// CHECK:  @inlinable internal init(_ ptr: Swift::UnsafeRawBufferPointer, _ a: consuming lifetime_underscored_dependence::AnotherView) {
// CHECK:    let bv = BufferView(ptr, a._count)
// CHECK:    self = _overrideLifetime(bv, copying: a)
// CHECK:  }
// CHECK:  #else
// CHECK:  @lifetime(copy a)
// CHECK:  @inlinable internal init(_ ptr: Swift::UnsafeRawBufferPointer, _ a: consuming lifetime_underscored_dependence::AnotherView) {
// CHECK:    let bv = BufferView(ptr, a._count)
// CHECK:    self = _overrideLifetime(bv, copying: a)
// CHECK:  }
// CHECK:  #endif

// CHECK:#if compiler(>=5.3) && $Lifetimes
// CHECK:@_lifetime(copy x)
// CHECK:@inlinable public func derive(_ x: consuming lifetime_underscored_dependence::BufferView) -> lifetime_underscored_dependence::BufferView {
// CHECK:  let pointer = x._ptr
// CHECK:  let bv = BufferView(pointer, x._count)
// CHECK:  return _overrideLifetime(bv, copying: x)
// CHECK:}
// CHECK:#else
// CHECK:@lifetime(copy x)
// CHECK:@inlinable public func derive(_ x: consuming lifetime_underscored_dependence::BufferView) -> lifetime_underscored_dependence::BufferView {
// CHECK:  let pointer = x._ptr
// CHECK:  let bv = BufferView(pointer, x._count)
// CHECK:  return _overrideLifetime(bv, copying: x)
// CHECK:}
// CHECK:#endif

// CHECK:#if compiler(>=5.3) && $Lifetimes
// CHECK:@_lifetime(copy view)
// CHECK:@inlinable public func consumeAndCreate(_ view: consuming lifetime_underscored_dependence::BufferView) -> lifetime_underscored_dependence::BufferView {
// CHECK:  let pointer = view._ptr
// CHECK:  let bv = BufferView(pointer, view._count)
// CHECK:  return _overrideLifetime(bv, copying: view)
// CHECK:}
// CHECK:#else
// CHECK:@lifetime(copy view)
// CHECK:@inlinable public func consumeAndCreate(_ view: consuming lifetime_underscored_dependence::BufferView) -> lifetime_underscored_dependence::BufferView {
// CHECK:  let pointer = view._ptr
// CHECK:  let bv = BufferView(pointer, view._count)
// CHECK:  return _overrideLifetime(bv, copying: view)
// CHECK:}
// CHECK:#endif

// CHECK:#if compiler(>=5.3) && $Lifetimes
// CHECK:@_lifetime(copy this, copy that)
// CHECK:@inlinable public func deriveThisOrThat(_ this: consuming lifetime_underscored_dependence::BufferView, _ that: consuming lifetime_underscored_dependence::BufferView) -> lifetime_underscored_dependence::BufferView {
// CHECK:  if (Int.random(in: 1..<100) == 0) {
// CHECK:    let thisView = BufferView(this._ptr, this._count)
// CHECK:    return _overrideLifetime(thisView, copying: this)
// CHECK:  }
// CHECK:  let thatView = BufferView(that._ptr, that._count)
// CHECK:  return _overrideLifetime(thatView, copying: that)
// CHECK:}
// CHECK:#else
// CHECK:@lifetime(copy this, copy that)
// CHECK:@inlinable public func deriveThisOrThat(_ this: consuming lifetime_underscored_dependence::BufferView, _ that: consuming lifetime_underscored_dependence::BufferView) -> lifetime_underscored_dependence::BufferView {
// CHECK:  if (Int.random(in: 1..<100) == 0) {
// CHECK:    let thisView = BufferView(this._ptr, this._count)
// CHECK:    return _overrideLifetime(thisView, copying: this)
// CHECK:  }
// CHECK:  let thatView = BufferView(that._ptr, that._count)
// CHECK:  return _overrideLifetime(thatView, copying: that)
// CHECK:}
// CHECK:#endif

// CHECK: extension lifetime_underscored_dependence::Container {
// CHECK-NEXT: #if compiler(>=5.3) && $Lifetimes
// CHECK-NEXT:   public var storage: lifetime_underscored_dependence::BufferView {

// CHECK: public struct RigidArray : ~Swift::Copyable {
// CHECK:   @usableFromInline
// CHECK:   internal let _ptr: Swift::UnsafeRawBufferPointer
// CHECK:   #if compiler(>=5.3) && $Lifetimes
// CHECK:   public var span: Swift::RawSpan {
// CHECK:     @_lifetime(borrow self)
// CHECK:     get
// CHECK:   }
// CHECK:   #else
// CHECK:   public var span: Swift::RawSpan {
// CHECK:     @lifetime(borrow self)
// CHECK:     get
// CHECK:   }
// CHECK:   #endif
// CHECK: }

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: #if compiler(>=5.3) && $Lifetimes
// CHECK-NEXT: @_lifetime(copy ne0)
// CHECK-NEXT: @inlinable public func takeCopier(f: @_lifetime(io: copy io) @_lifetime(copy inview) (_ inview: consuming lifetime_underscored_dependence::AnotherView, _ io: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView, ne0: consuming lifetime_underscored_dependence::AnotherView, ne1: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView {
// CHECK-NEXT:   let ne2 = f(ne0, &ne1)
// CHECK-NEXT:   return ne2
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @lifetime(copy ne0)
// CHECK-NEXT: @inlinable public func takeCopier(f: @_lifetime(io: copy io) @_lifetime(copy inview) (_ inview: consuming lifetime_underscored_dependence::AnotherView, _ io: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView, ne0: consuming lifetime_underscored_dependence::AnotherView, ne1: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView {
// CHECK-NEXT:   let ne2 = f(ne0, &ne1)
// CHECK-NEXT:   return ne2
// CHECK-NEXT: }
// CHECK-NEXT: #endif
// CHECK-NEXT: #endif

// CHECK: @inlinable public func takeCopierUnannotated(f: (consuming lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView) {}

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: public typealias ExplicitNestedType = @_lifetime(copy ne0) @_lifetime(ne1: copy ne0, copy ne1) ((lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView, _ ne0: consuming lifetime_underscored_dependence::AnotherView, _ ne1: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: @inlinable public func takeExplicitNestedType(f: @_lifetime(copy ne0) @_lifetime(ne1: copy ne0, copy ne1) ((lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView, _ ne0: consuming lifetime_underscored_dependence::AnotherView, _ ne1: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView) {} 
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $Lifetimes
// CHECK-NEXT: @inlinable public func returnableCopier(_ aView: lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView {
// CHECK-NEXT:   return aView
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @inlinable public func returnableCopier(_ aView: lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView {
// CHECK-NEXT:   return aView
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: @inlinable public func returnCopier() -> @_lifetime(copy a) (_ a: lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView {
// CHECK-NEXT:  return returnableCopier
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $Lifetimes
// CHECK-NEXT: @_lifetime(dest: immortal)
// CHECK-NEXT: @inlinable public func immortalInout(dest: inout lifetime_underscored_dependence::AnotherView) {
// CHECK-NEXT:   dest = _overrideLifetime(dest, copying: ())
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @lifetime(dest: immortal)
// CHECK-NEXT: @inlinable public func immortalInout(dest: inout lifetime_underscored_dependence::AnotherView) {
// CHECK-NEXT:   dest = _overrideLifetime(dest, copying: ())
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: @inlinable public func takeImmortalInout(closure: @_lifetime(dest: immortal) (_ dest: inout lifetime_underscored_dependence::AnotherView) -> ()) {}
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $Lifetimes
// CHECK-NEXT: @_lifetime(dest: immortal, copy source)
// CHECK-NEXT: @inlinable public func fullReassign(dest: inout lifetime_underscored_dependence::AnotherView, source: lifetime_underscored_dependence::AnotherView) {
// CHECK-NEXT:   dest = source
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: @lifetime(dest: immortal, copy source)
// CHECK-NEXT: @inlinable public func fullReassign(dest: inout lifetime_underscored_dependence::AnotherView, source: lifetime_underscored_dependence::AnotherView) {
// CHECK-NEXT:   dest = source
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: @inlinable public func takeFullReassign(closure: @_lifetime(dest: immortal, copy source) (_ dest: inout lifetime_underscored_dependence::AnotherView, _ source: lifetime_underscored_dependence::AnotherView) -> ()) {}
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: @inlinable public func takeReadBorrower(f: @_lifetime(borrow a) (_ a: lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView) {}
// CHECK-NEXT: #endif

// CHECK-NEXT: #if compiler(>=5.3) && $ClosureLifetimes
// CHECK-NEXT: @inlinable public func takeWriteBorrower(f: @_lifetime(&a) (_ a: inout lifetime_underscored_dependence::AnotherView) -> lifetime_underscored_dependence::AnotherView) {}
// CHECK-NEXT: #endif
