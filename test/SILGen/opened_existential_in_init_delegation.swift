// RUN: %target-swift-emit-silgen %s -target %target-swift-5.1-abi-triple

protocol P {}
class C {
  init(x: some P) {}

  convenience init(y: any P) { self.init(x: y) }
}

enum E {
  init(p: some P, throwing: ()) throws {}
  init(p: some P, async: ()) async {}
  init?(p: some P, failable: ()) {}

  init(p: any P, withTry: ()) throws {
    try self.init(p: p, throwing: ())
  }

  init(p: any P, withForceTry: ()) {
    try! self.init(p: p, throwing: ())
  }

  init(p: any P, withAwait: ()) async {
    await self.init(p: p, async: ())
  }

  init?(p: any P, withFailable: ()) {
    self.init(p: p, failable: ())
  }

  init(p: any P, withForce: ()) {
    self.init(p: p, failable: ())!
  }
}
