// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -verify \
// RUN:   -enable-experimental-feature Lifetimes -target %target-swift-6.2-abi-triple %s -o /dev/null

// REQUIRES: swift_feature_Lifetimes

// LifetimeDependenceScopeFixup runs before LifetimeResolution, so a dependent's uses extend the scopes of what it
// depends on instead of being diagnosed as escaping.

func use(_ s: Span<Int>) {}

struct View: ~Escapable {
  let ptr: UnsafeRawBufferPointer
  @_lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer) { self.ptr = ptr }
}

struct Container {
  var ptr: UnsafeRawBufferPointer
}

func use(_ v: borrowing View) {}
func consume(_ v: consuming View) {}

@_lifetime(borrow c)
func getView(_ c: borrowing Container) -> View {
  View(c.ptr)
}

func borrowedViewThenConsume(_ a: Array<Int>) {
  a.withUnsafeBytes {
    let x = Container(ptr: $0)
    let view = getView(x)
    use(view)
    consume(view)
  }
}

func spanOfLocalArray() -> Int {
  var a = [1, 2]
  a.append(3)
  let s = a.span
  return s.count
}

func mutableSpanOfLocalArray() {
  var a = [1, 2]
  var s = a.mutableSpan
  s[0] = 3
  _ = s.count
}

func spanOfBorrowedByAddress(_ i: Int) -> Int {
  let span = String(i).utf8.span
  return span.count
}
