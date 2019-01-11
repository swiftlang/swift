// RUN: %target-swift-emit-sil -swift-version 4 -verify %s
// RUN: %target-swift-emit-sil -swift-version 5 -verify %s

// Integration test to ensure that `type(of: self)` keeps working in
// class convenience initializers, even though they are now implemented as
// allocating entry points.

class C {
  required init() { }
  required init(throwingDesignated: ()) throws {}

  convenience init(normal: ()) {
    _ = (type(of: self), type(of: self))
    self.init()
    _ = (type(of: self), type(of: self))
  }

  convenience init(throwing: ()) throws {
    do {
      _ = (type(of: self), type(of: self))
      try self.init(throwingDesignated: ())
      _ = (type(of: self), type(of: self))
    } catch {
      _ = (type(of: self), type(of: self))
      throw error
    }
    _ = (type(of: self), type(of: self))
  }

  convenience init?(optional: Bool) {
    _ = (type(of: self), type(of: self))
    if optional {
      _ = (type(of: self), type(of: self))
      self.init()
    } else {
      _ = (type(of: self), type(of: self))
      return nil
    }
    _ = (type(of: self), type(of: self))
  }

  convenience init(closureCapture: ()) {
    let t = type(of: self)
    let fn = { t.init() }
    _ = fn()
    self.init()
  }
}
