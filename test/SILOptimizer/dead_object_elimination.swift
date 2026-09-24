// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %s | %FileCheck %s

// rdar://187491425
//
// DeadObjectElimination::processKeyPath must not eliminate a `keypath`
// instruction whose captured index argument is non-trivial (i.e. owns a
// refcounted value), even when the constructed KeyPath is otherwise unused.

final class Token {
  nonisolated(unsafe) static var alive = 0
  init() { Token.alive += 1 }
  deinit { Token.alive -= 1 }
}

struct Arg: Hashable {
  static func == (l: Self, r: Self) -> Bool { l.ref === r.ref }
  func hash(into h: inout Hasher) { ObjectIdentifier(ref).hash(into: &h) }
  let value: Int32
  let ref: Token
  init(_ v: Int32) { value = v; ref = Token() }
}

struct Root {
  subscript(meta arg: Arg) -> Int { get { 0 } set { _ = newValue } }
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}5checkyyF
// Check that DeadObjectElimination doesn't eliminate the keypath instruction
// CHECK: keypath $WritableKeyPath<Root, Int>
public func check() {
  let arg = Arg(42)
  withExtendedLifetime(\Root[meta: arg] as WritableKeyPath<Root, Int>) {}
}

// https://github.com/swiftlang/swift/issues/92448
//
// DeadObjectElimination correctly forwards the new value produced when
// rewriting `mark_dependence_addr` to `mark_dependence`.
@available(anyAppleOS 27.0, *)
func skipAlongIterable<I: Iterable & ~Escapable & ~Copyable>(
  limit: Int = .max,
  copying source: borrowing I
) -> Int where I.BorrowingIterator == Span<Int>.BorrowingIterator {
  var iterator = source.makeBorrowingIterator()
  var n = 0
  while n < limit {
    let span = iterator.nextSpan(maxCount: limit &- n)
    if span.isEmpty { break }
    n &+= span.count
  }
  return n
}

@available(anyAppleOS 27.0, *)
func callSkipperOnArray() {
  let a = Array(0..<12)
  _ = skipAlongIterable(limit: 8, copying: a.span)
}
