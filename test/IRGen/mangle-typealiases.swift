// RUN: %target-swift-frontend -Onone -g -emit-ir %s -o /dev/null

// Check that the compiler does not crash when trying to mangle and re-mangle
// the typealiases for debug info.

public struct Mystruct<T> {
  func testit<U, V>(x: T, u: U, v: V) {
    typealias Myalias = AnyObject
    let newAnyObject = unsafeBitCast(x, to: Myalias.self)
  }

  func testit2(x: T) {
    typealias Myalias = Array<T>
    let newAnyObject = unsafeBitCast(x, to: Myalias.self)
  }

  func testit3() -> Int {
    let c: () -> Int = {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(self.t, to: Myalias.self)
      return 0
    }
    return c()
  }

  var t: T {
    didSet {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
    }
    willSet {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
    }
  }

  var abc: Int {
    get {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
      return 0
    }
    set {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
    }
  }

  init(x: T) {
    t = x
    typealias Myalias = AnyObject
    let newAnyObject = unsafeBitCast(x, to: Myalias.self)
  }

  init<U>(x: T, u: U) {
    t = x
    typealias Myalias = AnyObject
    let newAnyObject = unsafeBitCast(x, to: Myalias.self)
  }

  var def: Int {
    return {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
      return 0
    }()
  }

  subscript(_ i: Int) -> Int {
    get {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
      return 0
    }
    set {
      typealias Myalias = AnyObject
      let newAnyObject = unsafeBitCast(t, to: Myalias.self)
    }
  }

  func foo() {
    typealias Myalias<T: Hashable> = Dictionary<T, T>
    let newAnyObject = unsafeBitCast(t, to: Myalias<Int>.self)
  }
}


public class Myclass<T> {
  var t: T
  init(x: T) {
    t = x
    typealias Myalias = AnyObject
    let newAnyObject = unsafeBitCast(x, to: Myalias.self)
  }
  deinit {
    typealias Myalias = AnyObject
    let newAnyObject = unsafeBitCast(t, to: Myalias.self)
  }
}
