// RUN: %target-swift-frontend -emit-sil -enable-copy-propagation %s -o /dev/null -verify

class Delegate {
  func foo() { }
}

final class Container {
  weak var delegate: Delegate?
  var strongRef: Delegate

  func callDelegate() {
    delegate!.foo()
  }

  init(_ d: Delegate) { strongRef = d }
  init(weakDelegate: Delegate, strongDelegate: Delegate) {
    delegate = weakDelegate
    strongRef = strongDelegate
  }
}

func warningForDeadDelegate(container: Container) {
  let delegate = Delegate()
  container.delegate = delegate
  container.callDelegate()
}

func noWarningForStoredDelegate(container: Container) {
  let delegate = Delegate()
  container.strongRef = delegate
  container.delegate = delegate
  container.callDelegate()
}

func noWarningWithFixLifetime(container: Container) {
  let delegate = Delegate()
  defer { _fixLifetime(delegate) }
  container.delegate = delegate
  container.callDelegate()
}

func warningWithControlFlow(container: Container, _ b: Bool) {
  let delegate = Delegate()
  container.delegate = delegate
  if b {
    container.callDelegate()
  }
}

var globalClosure: (() -> ())?

func storeClosure(_ c: @escaping () -> ()) {
  globalClosure = c
}

func warningForDeadClosureCapture() {
  let k = Delegate()
  storeClosure({ [weak k] in
                 k!.foo()
               })
}

func noWarningWithFixLifetime2() {
  let k = Delegate()
  defer { _fixLifetime(k) }
  storeClosure({ [weak k] in
                 k!.foo()
               })
}

func warningWithStoreWeakInCalledFunction() {
  let d = Delegate()
  let c = Container(weakDelegate: d, strongDelegate: Delegate())
  c.callDelegate()
}

final class StrongContainer {
  var k: Delegate
  init(_ k: Delegate) { self.k = k }
  func set(_ newk: Delegate) { k = newk }
  func noset(_ newk: Delegate) { }
}

final class Testcl {
  private weak var wk: Delegate?

  func test_set(_ c: StrongContainer) {
    let k = Delegate()
    c.set(k)
    wk = k // No warning here, because k is kept alive by c
  }

  func test_noset(_ c: StrongContainer) {
    let k = Delegate()
    c.noset(k)
    wk = k 
  }
}

