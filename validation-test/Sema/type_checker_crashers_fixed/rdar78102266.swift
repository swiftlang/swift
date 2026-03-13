// RUN: %target-swift-emit-silgen %s

struct Info {
}

class Test {
  var info: Info = Info()

  init() throws {}
}

_ = try  Test().info // Ok
_ = try! Test().info // Ok
