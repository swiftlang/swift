// RUN: %target-swift-frontend -verify -disable-availability-checking -strict-concurrency=complete -verify-additional-prefix complete- -emit-sil -o /dev/null %s
// RUN: %target-swift-frontend -verify -disable-availability-checking -strict-concurrency=complete -verify-additional-prefix region-isolation- -emit-sil -o /dev/null %s -enable-experimental-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

@globalActor
actor MyActor {
  static let shared = MyActor()
  @MyActor static var ns: NotSendable?
  @MyActor static func ohNo() { ns!.x += 1 }
}

@globalActor
actor YourActor {
  static let shared = YourActor()
  @YourActor static var ns: NotSendable?
  @YourActor static func ohNo() { ns!.x += 1 }
}

// expected-complete-note@+1 3{{class 'NotSendable' does not conform to the 'Sendable' protocol}}
class NotSendable {
  var x: Int = 0

  @MyActor init() {
    MyActor.ns = self
  }

  init(x: Int) {
    self.x = x
  }

  @MyActor func stash() {
    MyActor.ns = self
  }
}

@MyActor func exhibitRace1() async {
  let ns = NotSendable(x: 0)
  MyActor.ns = ns

  // expected-region-isolation-warning @+2 {{transferring 'ns' may cause a race}}
  // expected-region-isolation-note @+1 {{transferring global actor 'MyActor'-isolated 'ns' to global actor 'YourActor'-isolated callee could cause races between global actor 'YourActor'-isolated and global actor 'MyActor'-isolated uses}}
  await { @YourActor in
    // expected-complete-warning@+1 {{capture of 'ns' with non-sendable type 'NotSendable' in an isolated closure; this is an error in Swift 6}}
    YourActor.ns = ns
  }()

  await withTaskGroup(of: Void.self) {
    $0.addTask {
      await MyActor.ohNo()
    }

    $0.addTask {
      await YourActor.ohNo()
    }
  }
}

@MyActor func exhibitRace2() async {
  let ns = NotSendable(x: 0)
  ns.stash()

  // FIXME: Region isolation should diagnose this (https://github.com/apple/swift/issues/71533)
  await { @YourActor in
    // expected-complete-warning@+1 {{capture of 'ns' with non-sendable type 'NotSendable' in an isolated closure; this is an error in Swift 6}}
    YourActor.ns = ns
  }()

  await withTaskGroup(of: Void.self) {
    $0.addTask {
      await MyActor.ohNo()
    }

    $0.addTask {
      await YourActor.ohNo()
    }
  }
}

@MyActor func exhibitRace3() async {
  let ns = NotSendable()

  // FIXME: Region isolation should diagnose this (https://github.com/apple/swift/issues/71533)
  await { @YourActor in
    // expected-complete-warning@+1 {{capture of 'ns' with non-sendable type 'NotSendable' in an isolated closure; this is an error in Swift 6}}
    YourActor.ns = ns
  }()

  await withTaskGroup(of: Void.self) {
    $0.addTask {
      await MyActor.ohNo()
    }

    $0.addTask {
      await YourActor.ohNo()
    }
  }
}
