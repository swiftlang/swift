// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -target %target-swift-5.1-abi-triple %s
// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -target %target-swift-5.1-abi-triple %s
// RUN: %target-swift-frontend -emit-silgen -swift-version 6 -target %target-swift-5.1-abi-triple -D NO_ERRORS %s -o /dev/null

// REQUIRES: concurrency

// An 'unowned' self capture cannot be the isolation of an actor-isolated
// closure. See https://github.com/swiftlang/swift/issues/77365 and
// https://github.com/swiftlang/swift/issues/91530

func acceptEscaping(_ fn: @escaping () -> Void) {}

#if !NO_ERRORS
actor A {
  var x = 0

  func task() {
    Task { [unowned self] in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{13-20=weak}}
  }

  func taskUnsafe() {
    Task { [unowned(unsafe) self] in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned(unsafe)'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{13-28=weak}}
  }

  func isolatedAny() {
    let _: @isolated(any) () -> Void = { [unowned self] in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{43-50=weak}}
  }

  func sync() {
    let fn = { [unowned self] in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{17-24=weak}}
    fn()
  }

  func async() async {
    let fn = { [unowned self] () async in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{17-24=weak}}
    await fn()
  }

  func escaping() {
    acceptEscaping { [unowned self] in self.x += 1 }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{23-30=weak}}
  }

  func nested() {
    acceptEscaping { [unowned self] in
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{23-30=weak}}
      acceptEscaping { [y = 1] in self.x += y }
    }
  }
}

actor G<T> {
  var value: T? { nil }

  func f() {
    acceptEscaping { [unowned self] in _ = self.value }
    // expected-error@-1 {{actor-isolated closure cannot capture 'self' as 'unowned'}}
    // expected-note@-2 {{capture 'self' as 'weak' instead}} {{23-30=weak}}
  }
}
#endif

actor OK {
  var x = 0
  nonisolated func n() {}

  func weakTask() {
    Task { [weak self] in await self?.bump() }
  }

  func weakSync() {
    acceptEscaping { [weak self] in self?.n() }
  }

  func detached() {
    Task.detached { [unowned self] in self.n() }
  }

  func bump() { x += 1 }
}

func isolatedParam(_ a: isolated OK) {
  Task { [unowned a] in await a.bump() }
}

class C {
  func f() {}
  func g() {
    Task { [unowned self] in self.f() }
  }
}
