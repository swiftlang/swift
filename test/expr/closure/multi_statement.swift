// RUN: %target-typecheck-verify-swift -swift-version 5 -experimental-multi-statement-closures -enable-experimental-static-assert

func isInt<T>(_ value: T) -> Bool {
  return value is Int
}

func maybeGetValue<T>(_ value: T) -> T? {
  return value
}

enum MyError: Error {
  case featureIsTooCool
}

func random(_: Int) -> Bool { return false }

func mapWithMoreStatements(ints: [Int]) throws {
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

      print("even")
      throw MyError.featureIsTooCool
    }

    #assert(true)

    return String(value)
  }
}
