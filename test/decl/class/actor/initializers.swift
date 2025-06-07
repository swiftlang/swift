// RUN: %target-swift-frontend -swift-version 5 -target %target-swift-5.1-abi-triple -emit-sil -verify %s

// check the initializer kinds for protocols and their extensions

protocol GoodActor {
  init()
  init(with: Int)
}

extension GoodActor {
  init() {
    self.init(with: 0)
  }

  init(with: Int) {} // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

actor Myself: GoodActor {
  var x: Int

  init(with val: Int) {
    self.x = val
  }
}

actor SomebodyElse: GoodActor {
  var x: Int

  init(with val: Int) {
    self.x = val
  }

  init() {
    self.x = 0
  }
}
