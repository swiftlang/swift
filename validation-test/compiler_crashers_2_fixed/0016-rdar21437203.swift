// RUN: %target-swift-frontend %s -emit-silgen

struct Curds {
  var whey: AnyObject? = nil
}

private class Butter {
    private func churn<T>(block: () throws -> T) throws -> T {
      return try block()
    }
}

struct Cow {
  private var b : Butter
  init() {
    self.b = Butter()
  }

  func cheese() throws {
    let a = Curds()
    let b = Curds()
    let c = Curds()
    var err = 0
    var newChild = 0

    defer { }

    try self.b.churn { return () }
  }
}
