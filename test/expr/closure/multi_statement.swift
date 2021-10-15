// RUN: %target-typecheck-verify-swift -swift-version 5 -experimental-multi-statement-closures -enable-experimental-static-assert

func isInt<T>(_ value: T) -> Bool {
  return value is Int
}

func maybeGetValue<T>(_ value: T) -> T? {
  return value
}

enum MyError: Error {
  case featureIsTooCool

  func doIt() { }
}

enum State {
  case suspended
  case partial(Int, Int)
  case finished
}

func random(_: Int) -> Bool { return false }

func mightThrow() throws -> Bool { throw MyError.featureIsTooCool }

func mapWithMoreStatements(ints: [Int], state: State) throws {
  let _ = try ints.map { i in
    guard var actualValue = maybeGetValue(i) else {
      return String(0)
    }

    let value = actualValue + 1
    do {
      if isInt(i) {
        print(value)
      } else if value == 17 {
        print("seventeen!")
      }
    }

    while actualValue < 100 {
      actualValue += 1
    }

  my_repeat:
    repeat {
      print("still here")

      if i % 7 == 0 {
        break my_repeat
      }
    } while random(i)

    defer {
      print("I am so done here")
    }

    for j in 0..<i where j % 2 == 0 {
      if j % 7 == 0 {
        continue
      }

      switch (state, j) {
      case (.suspended, 0):
        print("something")
        fallthrough
      case (.finished, 0):
        print("something else")

      case (.partial(let current, let end), let j):
        print("\(current) of \(end): \(j)")

      default:
        print("so, here we are")
      }
      print("even")
      throw MyError.featureIsTooCool
    }

    #assert(true)

    // expected-warning@+1{{danger zone}}
    #warning("danger zone")

#if false
    struct NothingHere { }
#else
    struct NestedStruct {
      var x: Int
    }
#endif

    do {
      print(try mightThrow())
    } catch let e as MyError {
      e.doIt()
    } catch {
      print(error)
    }
    return String(value)
  }
}

func acceptsWhateverClosure<T, R>(_ value: T, _ fn: (T) -> R) { }

func testReturnWithoutExpr(i: Int) {
  acceptsWhateverClosure(i) { i in
    print(i)
    return
  }
}

// `withContiguousStorageIfAvailable` is overloaded, so let's make sure that
// filtering works correctly.
func test_overloaded_call(arr: [Int], body: (UnsafeBufferPointer<Int>) -> Void) -> Void {
  arr.withContiguousStorageIfAvailable { buffer in
    let _ = type(of: buffer)
    body(buffer) // ok
  }
}

// Used to wrap closure in `FunctionConversionExpr` in this case,
// but now solver would just inject return expression into optional where necessary.
func test_result_optional_injection() {
  func fn<T>(_: () -> T?) -> [T] {
    []
  }

  _ = fn {
    if true {
      return // Ok
    }
  }
}

let _ = {
  for i: Int8 in 0 ..< 20 { // Ok (pattern can inform a type of the sequence)
    print(i)
  }
}
