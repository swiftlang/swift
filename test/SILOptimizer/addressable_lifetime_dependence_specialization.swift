// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Build the library as a separate, library-evolution-enabled module.
// RUN: %target-swift-frontend -emit-module -parse-as-library -O \
// RUN:   -enable-library-evolution -disable-availability-checking \
// RUN:   -enable-experimental-feature BorrowingSequence \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -module-name IterableLib %t/library.swift \
// RUN:   -emit-module-path %t/IterableLib.swiftmodule
//
// Compile the client, dumping the specialized witness thunk across the pipeline.
// RUN: %target-swift-frontend -emit-sil -O -I %t \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature BorrowingSequence \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -module-name main %t/client.swift \
// RUN:   -Xllvm '-sil-print-function=$ss11InlineArrayVyxq_G11IterableLib04TestC0ADRi__rlAdEP21makeBorrowingIterator0egH0QzyFTW$3__SiTg5' \
// RUN:   -o /dev/null 2> %t/thunk.sil
// RUN: %FileCheck %s < %t/thunk.sil
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t/thunk.sil

// REQUIRES: swift_feature_BorrowingSequence
// REQUIRES: swift_feature_Lifetimes

// Regression test for rdar://181365820.
//
// A lifetime dependence whose source is a *conditionally addressable*
// parameter (spelled `address_for_deps` in SIL) must survive serialization
// into a .swiftmodule. If it doesn't, a cross-module client deserializes the
// callee as a plain `borrow` dependence, the GenericSpecializer then promotes
// the `@in_guaranteed` (by-address) parameter to a by-value parameter, and the
// returned Span ends up depending on a temporary copy -> use-after-free.
//
// This test builds the library as a *separate* module (so the conformance and
// its `address_for_deps` lifetime dependence are serialized), then compiles a
// client and dumps the specialized protocol-witness thunk for
// InlineArray.makeBorrowingIterator(). The thunk must pass the InlineArray by
// address and keep the borrow dependence on that address.

// The specialized witness thunk still borrows the InlineArray by address, and
// the callee it invokes keeps the `address_for_deps` borrow dependence.
//
// CHECK-LABEL: sil {{.*}}[thunk] {{.*}}makeBorrowingIterator{{.*}}Tg5 : $@convention(witness_method: TestIterable) (@in_guaranteed InlineArray<4, Int>) -> @lifetime(borrow address_for_deps 0) @owned Span<Int>.TestBorrowingIterator {
// CHECK: bb0([[SELF:%[0-9]+]] : $*InlineArray<4, Int>):
// CHECK: [[F:%[0-9]+]] = function_ref @{{.*}}makeBorrowingIterator{{.*}} : $@convention(method) {{.*}}(@in_guaranteed InlineArray<{{.*}}>) -> @lifetime(borrow address_for_deps 0) @owned Span<{{.*}}>.TestBorrowingIterator
// CHECK: apply [[F]]<{{.*}}>([[SELF]])
// CHECK: mark_dependence [nonescaping] {{%[0-9]+}} on [[SELF]]

// The pre-fix miscompile specialized the callee to take the InlineArray by
// value, so the thunk copied the argument with a `load [trivial]` and the
// callee dependence became `@lifetime(borrow 0)` (no `address_for_deps`) on a
// temporary. None of that may appear.
//
// NEGATIVE-NOT: load [trivial]
// NEGATIVE-NOT: $@convention(method) (InlineArray<4, Int>) -> @lifetime(borrow 0)

//--- library.swift

public protocol TestBorrowingIteratorProtocol<Element, Failure>: ~Copyable, ~Escapable {
  associatedtype Element: ~Copyable

  associatedtype Failure: Error = Never

  @_lifetime(&self)
  @_lifetime(self: copy self)
  mutating func nextSpan(maxCount: Int) throws(Failure) -> Span<Element>
}

extension Span where Element: ~Copyable {
  @frozen
  public struct TestBorrowingIterator: TestBorrowingIteratorProtocol, ~Copyable, ~Escapable {
    @usableFromInline
    internal var _span: Span<Element>
    @usableFromInline
    internal var _start: Int
    @usableFromInline
    internal var _count: Int

    public typealias Failure = Never

    @_lifetime(copy elements)
    @inlinable
    public init(_ elements: Span<Element>) {
      _span = elements
      _start = 0
      _count = elements.count
    }

    @_alwaysEmitIntoClient
    @_lifetime(&self)
    @_lifetime(self: copy self)
    @_transparent
    public mutating func nextSpan(maxCount: Int) -> Span<Element> {
      let c = Swift.min(maxCount, _count)
      defer {
        _start &+= c
        _count &-= c
      }
      return _span.extracting(droppingFirst: _start).extracting(first: c)
    }
  }
}

public protocol TestIterable<Element, Failure>: ~Copyable, ~Escapable {
  /// A type representing the sequence's elements.
  associatedtype Element: ~Copyable

  associatedtype Failure: Error = Never

  /// A type that provides the sequence's iteration interface and
  /// encapsulates its iteration state.
  associatedtype TestBorrowingIterator: TestBorrowingIteratorProtocol<Element, Failure> & ~Copyable & ~Escapable

  /// Returns a borrowing iterator over the elements of this sequence.
  @_lifetime(borrow self)
  func makeBorrowingIterator() -> TestBorrowingIterator
}

extension InlineArray: TestIterable where Element: ~Copyable {
  public typealias TestBorrowingIterator = Span<Element>.TestBorrowingIterator

  public typealias Failure = Never

  @_alwaysEmitIntoClient
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> TestBorrowingIterator {
    Span.TestBorrowingIterator(self.span)
  }
}

//--- client.swift

import IterableLib

func printIterAddress<Iter: ~Copyable & ~Escapable>(_ iter: inout Iter) {
  withUnsafeBytes(of: &iter) { ptr in
    print("address of borrowingIterator:", ptr)
  }
}

func printEltAddress<Element>(_ span: Span<Element>) {
  span.withUnsafeBufferPointer { ptr in
    print("address of span elements:", ptr.baseAddress!)
  }
}

extension TestIterable where Self: ~Copyable & ~Escapable, Element: Copyable {
  @inline(never)
  @_silgen_name("collect")
  func collect() throws(Failure) -> [Element] {
    var borrowIterator = makeBorrowingIterator()
    printIterAddress(&borrowIterator)
    var result: [Element] = []
    let span = try borrowIterator.nextSpan(maxCount: .max)
    printEltAddress(span)
    for i in span.indices {
      result.append(span[i])
    }
    return result
  }
}

@inline(never)
func test() {
  let inline: [4 of Int] = [10, 20, 30, 40]
  let collected = inline.collect()
  precondition(collected[0] == 10)
}

test()
