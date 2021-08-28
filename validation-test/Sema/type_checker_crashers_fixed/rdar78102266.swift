// RUN: %target-swift-frontend %s -typecheck

struct Info {
}

class Test {
  var info: Info = Info()

  init() throws {}
}

_ = try  Test().info // Ok
_ = try! Test().info // Ok
