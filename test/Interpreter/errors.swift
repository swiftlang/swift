// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for error handling.
//

import StdlibUnittest


enum Excuse : Error { case CatAteHomework(LifetimeTracked) }

var ErrorHandlingTests = TestSuite("ErrorHandling")

func furball(_ b: Bool) throws -> LifetimeTracked {
  if b {
    throw Excuse.CatAteHomework(LifetimeTracked(0))
  } else {
    return LifetimeTracked(1)
  }
}

ErrorHandlingTests.test("tryCatch") {
  do {
    try expectEqual(furball(false), LifetimeTracked(1))
  } catch {
    expectUnreachable() 
  }

  do {
    try furball(true)
    expectUnreachable() 
  } catch let e {
    if case Excuse.CatAteHomework(let c) = e {
      expectEqual(c, LifetimeTracked(0))
    } else {
      expectUnreachable()
    }
  }
}

ErrorHandlingTests.test("tryOptional") {
  expectEqual(LifetimeTracked(1), try? furball(false))
  expectEqual(Optional<LifetimeTracked>.none, try? furball(true))
}

ErrorHandlingTests.test("fallthroughInCatch") {
  switch 1 {
  case 1:
    do {
      try furball(true)
      expectUnreachable()
    } catch Excuse.CatAteHomework(_) {
      fallthrough
    } catch {
      expectUnreachable()
    }
  case 0:
    return
  default:
    expectUnreachable()
  }
  expectUnreachable()
}

ErrorHandlingTests.test("breakInCatch") {
  switch 1 {
  case 1:
    do {
      try furball(true)
    } catch {
      break
    }
    expectUnreachable() // break out of the case, not the catch
  default:
    break
  }
}

enum Foo: Error {
  case a(LifetimeTracked)
  case b(LifetimeTracked)
}

func baz(_ x: Foo, _ condition: (LifetimeTracked) -> Bool) -> Bool {
  do {
    throw x
  } catch Foo.a(let obj) where condition(obj),
          Foo.b(let obj) where condition(obj) {
    return true
  } catch {}
  return false
}

ErrorHandlingTests.test("multiPatternWherePaths") {
  _ = baz(.a(LifetimeTracked(1)), { _ in true })
  _ = baz(.b(LifetimeTracked(2)), { _ in true })
  _ = baz(.a(LifetimeTracked(3)), { _ in false })
  _ = baz(.b(LifetimeTracked(4)), { _ in false })
}

public enum Phase<Value>: Error {
  case possible
  case active(Value)
  case paused(Value)
  case ended(Value)
  case failed
}

extension Phase {
  public var valueLet: Value? {
    do {
      throw self
    }
    catch Phase.possible, Phase.failed {
      return nil
    }
    catch let Phase.active(value), let Phase.paused(value), let Phase.ended(value) {
      return value
    } catch { expectUnreachable() }
    expectUnreachable()
    return nil
  }

  public var valueVar: Value? {
    do {
      throw self
    }
    catch Phase.possible, Phase.failed {
      return nil
    }
    catch var Phase.active(value), var Phase.paused(value), var Phase.ended(value) {
      return value
    } catch { expectUnreachable() }
    expectUnreachable()
    return nil
  }
}

enum K {
  case A, B
}

enum A<K>: Error {
  case left(a: K, b: K)
  case right(a: K, b: K)
  
  var valueLet: [K] {
    do {
      throw self
    }
    catch let A.left(a, b), let A.right(a, b) {
      return [a, b]
    } catch { expectUnreachable() }
    expectUnreachable()
    return []
  }
  
  var valueVar: [K] {
    do {
      throw self
    }
    catch var A.left(a, b), var A.right(a, b) {
      return [a, b]
    } catch { expectUnreachable() }
    expectUnreachable()
    return []
  }
}

ErrorHandlingTests.test("GenericLet") {
  do {
    expectEqual(1.0, Phase.active(1.0).valueLet)
    expectEqual(2.0, Phase.paused(2.0).valueLet)
    expectEqual(3.0, Phase.ended(3.0).valueLet)
  }

  do {
    let l = LifetimeTracked(0)
    expectTrue(l === Phase.active(l).valueLet)
    expectTrue(l === Phase.paused(l).valueLet)
    expectTrue(l === Phase.ended(l).valueLet)
  }

  do {
    expectEqual([K.A, K.B], A.left(a: K.A, b: K.B).valueLet)
    expectEqual([K.A, K.B], A.right(a: K.A, b: K.B).valueLet)
  }

  do {
    let l = LifetimeTracked(0)
    let r = LifetimeTracked(0)
    let arr = A.left(a: l, b: r).valueLet
    expectTrue(arr[0] === l)
    expectTrue(arr[1] === r)
  }

  do {
    let l = LifetimeTracked(0)
    let r = LifetimeTracked(0)
    let arr = A.right(a: l, b: r).valueLet
    expectTrue(arr[0] === l)
    expectTrue(arr[1] === r)
  }
}

ErrorHandlingTests.test("GenericVar") {
  do {
    expectEqual(1.0, Phase.active(1.0).valueVar)
    expectEqual(2.0, Phase.paused(2.0).valueVar)
    expectEqual(3.0, Phase.ended(3.0).valueVar)
  }

  do {
    let l = LifetimeTracked(0)
    expectTrue(l === Phase.active(l).valueVar)
    expectTrue(l === Phase.paused(l).valueVar)
    expectTrue(l === Phase.ended(l).valueVar)
  }

  do {
    expectEqual([K.A, K.B], A.left(a: K.A, b: K.B).valueVar)
    expectEqual([K.A, K.B], A.right(a: K.A, b: K.B).valueVar)
  }

  do {
    let l = LifetimeTracked(0)
    let r = LifetimeTracked(0)
    let arr = A.left(a: l, b: r).valueVar
    expectTrue(arr[0] === l)
    expectTrue(arr[1] === r)
  }

  do {
    let l = LifetimeTracked(0)
    let r = LifetimeTracked(0)
    let arr = A.right(a: l, b: r).valueVar
    expectTrue(arr[0] === l)
    expectTrue(arr[1] === r)
  }
}

enum Gesture: Error {
  case pan(Any)
  case pinch(Any)
}

extension Gesture {
  var valueLet: Any {
    do {
      throw self
    }
    catch Gesture.pan(let data),
          Gesture.pinch(let data) {
      return data
    } catch { expectUnreachable() }
    expectUnreachable()
    return 42
  }
  var valueVar: Any {
    do { throw self }
    catch Gesture.pan(var data),
          Gesture.pinch(var data) {
      return data
    } catch { expectUnreachable() }
    expectUnreachable()
    return 42
  }
}

ErrorHandlingTests.test("GenericLet") {
  expectEqual(1, Gesture.pan(1).valueLet as! Int)
  expectEqual(2, Gesture.pinch(2).valueLet as! Int)

  let l = LifetimeTracked(0)
  expectTrue(l === Gesture.pan(l).valueLet as! LifetimeTracked)
  expectTrue(l === Gesture.pinch(l).valueLet as! LifetimeTracked)
}

ErrorHandlingTests.test("GenericVar") {
  expectEqual(1, Gesture.pan(1).valueVar as! Int)
  expectEqual(2, Gesture.pinch(2).valueVar as! Int)

  let l = LifetimeTracked(0)
  expectTrue(l === Gesture.pan(l).valueVar as! LifetimeTracked)
  expectTrue(l === Gesture.pinch(l).valueVar as! LifetimeTracked)
}

ErrorHandlingTests.test("Enum Initialization Leaks") {
  enum Enum1 {
  case case1(LifetimeTracked)
  case case2(LifetimeTracked, Int)
  }

  enum Enum2: Error {
  case case1(LifetimeTracked)
  case case2(Enum1, LifetimeTracked)
  }

  struct Struct {
    var value: Enum2 = .case2(.case1(LifetimeTracked(0)), LifetimeTracked(1))

    func doSomethingCatch() {
      do {
        throw value
      }
      catch let Enum2.case2(.case2(k, _), _) {
        return
      } catch {
        return
      }
      return
    }
  }

  do {
    let s = Struct()
    s.doSomethingCatch()
  }
}

runAllTests()
