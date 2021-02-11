// RUN: %target-swift-frontend -emit-sil %s -o /dev/null -verify

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
}

func warningForDeadDelegate(container: Container) {
  let delegate = Delegate()
  container.delegate = delegate  // expected-warning {{weak reference will always be nil because the referenced object is deallocated here}}
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
  container.delegate = delegate  // expected-warning {{weak reference will always be nil because the referenced object is deallocated here}}
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
  storeClosure({ [weak k] in  // expected-warning {{weak reference will always be nil because the referenced object is deallocated here}}
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

