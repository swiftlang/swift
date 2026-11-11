// rdar://182539642 (https://github.com/swiftlang/swift/issues/90780)
// RUN: %target-typecheck-verify-swift

func example() {
  var storage: [Int] = []

  func sync1<T>(execute work: () -> T) -> T { work() }
  let a: [Int] = sync1 { storage[...] }
  // expected-error@-1 {{conflicting arguments to generic parameter 'T' ('ArraySlice<Int>' vs. '[Int]')}}
  // expected-note@-2 {{generic parameter 'T' inferred as 'ArraySlice<Int>' from closure return expression}}
  // expected-note@-3 {{generic parameter 'T' inferred as '[Int]' from context}}

  struct Thing {
    static func sync<T>(execute work: () -> T) -> T { work() }
    func sync<T>(execute work: () -> T) -> T { work() }
  }

  let b: [Int] = Thing.sync { storage[...] }
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  let c: [Int] = Thing().sync { storage[...] }
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}

struct S {
    static func g<R>(_ body: () -> R) -> R {
        body()
    }
}

func f(_ x: Int) -> Bool {
    S.g { x } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
