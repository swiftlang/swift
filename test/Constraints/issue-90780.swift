// rdar://182539642 (https://github.com/swiftlang/swift/issues/90780)
// RUN: %target-typecheck-verify-swift

func example() {
  var storage: [Int] = []

  func sync1<T>(execute work: () -> T) -> T { work() }
  let a: [Int] = sync1 { storage[...] }
  // expected-error@-1 {{cannot convert value of type 'ArraySlice<Int>' to specified type '[Int]'}}

  struct Thing {
    static func sync<T>(execute work: () -> T) -> T { work() }
    func sync<T>(execute work: () -> T) -> T { work() }
  }

  let b: [Int] = Thing.sync { storage[...] }
  // expected-error@-1 {{cannot convert value of type 'ArraySlice<Int>' to specified type '[Int]'}}
  let c: [Int] = Thing().sync { storage[...] }
  // expected-error@-1 {{cannot convert value of type 'ArraySlice<Int>' to specified type '[Int]'}}
}

struct S {
    static func g<R>(_ body: () -> R) -> R {
        body()
    }
}

func f(_ x: Int) -> Bool {
    S.g { x } // expected-error {{type 'Int' cannot be used as a boolean; test for '!= 0' instead}}
}
