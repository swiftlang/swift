// RUN: %target-typecheck-verify-swift

func foo(a: Int?, b: Int?) -> Int {
  switch (a, b) {
  case (.none, _): return 1
  case (_, .none): return 2
  case (.some(_), .some(_)): return 3
  }
    
  switch (a, b) {
  case (.none, _): return 1
  case (_, .none): return 2
  case (_?, _?): return 3
  }
  
  switch Optional<(Int?, Int?)>.some((a, b)) {
  case .none: return 1
  case let (_, x?)?: return x
  case let (x?, _)?: return x
  case (.none, .none)?: return 0
  }
}

func bar(a: Bool, b: Bool) -> Int {
  switch (a, b) {
  case (false, false):
    return 1
  case (true, _):
    return 2
  case (false, true):
    return 3
  }
}


enum Result<T> {
  case Ok(T)
  case Error(Error)

  func shouldWork<U>(other: Result<U>) -> Int {
    switch (self, other) { // No warning
    case (.Ok, .Ok): return 1
    case (.Error, .Error): return 2
    case (.Error, _): return 3
    case (_, .Error): return 4
    }
  }
}

enum Foo {
  case A(Int)
  case B(Int)
}
func foo() {
  switch (Foo.A(1), Foo.B(1)) {
  case (.A(_), .A(_)):
    ()
  case (.B(_), _):
    ()
  case (_, .B(_)):
    ()
  }
  
  switch (Foo.A(1), Optional<(Int, Int)>.some((0, 0))) {
  case (.A(_), _):
    break
  case (.B(_), (let q, _)?):
    print(q)
  case (.B(_), nil):
    break
  }
}

class C {}

enum Bar {
  case TheCase(C?)
}

func test(f: Bar) -> Bool {
  switch f {
  case .TheCase(_?):
    return true
  case .TheCase(nil):
    return false
  }
}

func op(this : Optional<Bool>, other : Optional<Bool>) -> Optional<Bool> {
  switch (this, other) { // No warning
  case let (.none, w):
    return w
  case let (w, .none):
    return w
  case let (.some(e1), .some(e2)):
    return .some(e1 && e2)
  }
}

enum Threepeat {
  case a, b, c
}

func test3(x: Threepeat, y: Threepeat) {
  switch (x, y) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.a, .c)'}}
  case (.a, .a):
    ()
  case (.b, _):
    ()
  case (.c, _):
    ()
  case (_, .b):
    ()
  }
}

enum A {
  case A(Int)
  case B(Bool)
  case C
  case D
}

enum B {
  case A
  case B
}

func s(a: A, b: B) {
  switch (a, b) {
  case (.A(_), .A):
    break
  case (.A(_), .B):
    break

  case (.B(_), let b):
  // expected-warning@-1 {{immutable value 'b' was never used; consider replacing with '_' or removing it}}
    break

  case (.C, _), (.D, _):
    break
  }
}


enum Grimble {
  case A
  case B
  case C
}

enum Gromble {
  case D
  case E
}

func doSomething(foo:Grimble, bar:Gromble) {
  switch(foo, bar) { // No warning
  case (.A, .D):
    break
  case (.A, .E):
    break
  case (.B, _):
    break
  case (.C, _):
    break
  }
}

enum E {
  case A
  case B
}

func f(l: E, r: E) {
  switch (l, r) {
  case (.A, .A):
    return
  case (.A, _):
    return
  case (_, .A):
    return
  case (.B, .B):
    return
  }
}

enum TestEnum {
  case A, B
}

func switchOverEnum(testEnumTuple: (TestEnum, TestEnum)) {

  switch testEnumTuple {
  case (_,.B):
    // Matches (.A, .B) and (.B, .B)
    break
  case (.A,_):
    // Matches (.A, .A)
    // Would also match (.A, .B) but first case takes precedent
    break
  case (.B,.A):
    // Matches (.B, .A)
    break
  }

}

func tests(a: Int?, b: String?) {
  switch (a, b) {
  case let (.some(n), _): print("a: ", n, "?")
  case (.none, _): print("Nothing", "?")
  }

  switch (a, b) {
  case let (.some(n), .some(s)): print("a: ", n, "b: ", s)
  case let (.some(n), .none): print("a: ", n, "Nothing")
  case (.none, _): print("Nothing")
  }

  switch (a, b) {
  case let (.some(n), .some(s)): print("a: ", n, "b: ", s)
  case let (.some(n), .none): print("a: ", n, "Nothing")
  case let (.none, .some(s)): print("Nothing", "b: ", s)
  case (.none, _): print("Nothing", "?")
  }

  switch (a, b) {
  case let (.some(n), .some(s)): print("a: ", n, "b: ", s)
  case let (.some(n), .none): print("a: ", n, "Nothing")
  case let (.none, .some(s)): print("Nothing", "b: ", s)
  case (.none, .none): print("Nothing", "Nothing")
  }

}

enum X {
  case Empty
  case A(Int)
  case B(Int)
}

func f(a: X, b: X) {
  switch (a, b) {
  case (_, .Empty): ()
  case (.Empty, _): ()

  case (.A, .A): ()
  case (.B, .B): ()

  case (.A, .B): ()
  case (.B, .A): ()

  }
}

func f2(a: X, b: X) {
  switch (a, b) {

  case (.A, .A): ()
  case (.B, .B): ()

  case (.A, .B): ()
  case (.B, .A): ()

  case (_, .Empty): ()
  case (.Empty, _): ()

  case (.A, .A): () // expected-warning {{case is already handled by previous patterns; consider removing it}}
  case (.B, .B): () // expected-warning {{case is already handled by previous patterns; consider removing it}}

  case (.A, .B): () // expected-warning {{case is already handled by previous patterns; consider removing it}}
  case (.B, .A): () // expected-warning {{case is already handled by previous patterns; consider removing it}}

  default: ()
  }
}

enum XX : Int {
  case A
  case B
  case C
  case D
  case E
}

func switcheroo(a: XX, b: XX) -> Int {
  switch(a, b) { // No warning
  case (.A, _)  : return 1
  case (_, .A)  : return 2

  case (.C, _)  : return 3
  case (_, .C)  : return 4

  case (.B, .B) : return 5
  case (.B, .D) : return 6
  case (.D, .B) : return 7

  case (.B, .E) : return 8
  case (.E, .B) : return 9

  case (.E, _)  : return 10
  case (_, .E)  : return 11
  case (.D, .D) : return 12

  default:
    print("never hits this:", a, b)
    return 13
  }
}

enum PatternCasts {
  case one(Any)
  case two
}

func checkPatternCasts() {
  // Pattern casts with this structure shouldn't warn about duplicate cases.
  let x: PatternCasts = .one("One")
  switch x {
  case .one(let s as String): print(s)
  case .one: break
  case .two: break
  }

  // But should warn here.
  switch x {
  case .one(_): print(s)
  case .one: break // expected-warning {{case is already handled by previous patterns; consider removing it}}
  case .two: break
  }
}

enum Runcible {
  case spoon
  case hat
  case fork
}

func checkDiagnosticMinimality(x: Runcible?) {
  switch (x!, x!) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.fork, _)'}}
  // expected-note@-2 {{add missing case: '(.hat, .hat)'}}
  // expected-note@-3 {{add missing case: '(.hat, .fork)'}}
  // expected-note@-4 {{add missing case: '(_, .fork)'}}
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  }

  switch (x!, x!) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.fork, _)'}}
  // expected-note@-2 {{add missing case: '(.hat, .spoon)'}}
  // expected-note@-3 {{add missing case: '(.hat, .fork)'}}
  // expected-note@-4 {{add missing case: '(.spoon, .hat)'}}
  // expected-note@-5 {{add missing case: '(.spoon, .fork)'}}
  // expected-note@-6 {{add missing case: '(_, .fork)'}}
  case (.spoon, .spoon):
    break
  case (.hat, .hat):
    break
  }
}

enum LargeSpaceEnum {
  case case0
  case case1
  case case2
  case case3
  case case4
  case case5
  case case6
  case case7
  case case8
  case case9
  case case10
}

func notQuiteBigEnough() -> Bool {
  switch (LargeSpaceEnum.case1, LargeSpaceEnum.case2) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 110 {{add missing case:}}
  case (.case0, .case0): return true
  case (.case1, .case1): return true
  case (.case2, .case2): return true
  case (.case3, .case3): return true
  case (.case4, .case4): return true
  case (.case5, .case5): return true
  case (.case6, .case6): return true
  case (.case7, .case7): return true
  case (.case8, .case8): return true
  case (.case9, .case9): return true
  case (.case10, .case10): return true
  }
}

enum OverlyLargeSpaceEnum {
  case case0
  case case1
  case case2
  case case3
  case case4
  case case5
  case case6
  case case7
  case case8
  case case9
  case case10
  case case11
}

func quiteBigEnough() -> Bool {
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{do you want to add a default clause?}}
  case (.case0, .case0): return true
  case (.case1, .case1): return true
  case (.case2, .case2): return true
  case (.case3, .case3): return true
  case (.case4, .case4): return true
  case (.case5, .case5): return true
  case (.case6, .case6): return true
  case (.case7, .case7): return true
  case (.case8, .case8): return true
  case (.case9, .case9): return true
  case (.case10, .case10): return true
  case (.case11, .case11): return true
  }

  // No diagnostic
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{do you want to add a default clause?}}
  case (.case0, _): return true
  case (.case1, _): return true
  case (.case2, _): return true
  case (.case3, _): return true
  case (.case4, _): return true
  case (.case5, _): return true
  case (.case6, _): return true
  case (.case7, _): return true
  case (.case8, _): return true
  case (.case9, _): return true
  case (.case10, _): return true
  }


  // No diagnostic
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) {
  case (.case0, _): return true
  case (.case1, _): return true
  case (.case2, _): return true
  case (.case3, _): return true
  case (.case4, _): return true
  case (.case5, _): return true
  case (.case6, _): return true
  case (.case7, _): return true
  case (.case8, _): return true
  case (.case9, _): return true
  case (.case10, _): return true
  case (.case11, _): return true
  }

  // No diagnostic
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) {
  case (_, .case0): return true
  case (_, .case1): return true
  case (_, .case2): return true
  case (_, .case3): return true
  case (_, .case4): return true
  case (_, .case5): return true
  case (_, .case6): return true
  case (_, .case7): return true
  case (_, .case8): return true
  case (_, .case9): return true
  case (_, .case10): return true
  case (_, .case11): return true
  }

  // No diagnostic
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) {
  case (_, _): return true
  }

  // No diagnostic
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) {
  case (.case0, .case0): return true
  case (.case1, .case1): return true
  case (.case2, .case2): return true
  case (.case3, .case3): return true
  case _: return true
  }
}

indirect enum InfinitelySized {
  case one
  case two
  case recur(InfinitelySized)
  case mutualRecur(MutuallyRecursive, InfinitelySized)
}

indirect enum MutuallyRecursive {
  case one
  case two
  case recur(MutuallyRecursive)
  case mutualRecur(InfinitelySized, MutuallyRecursive)
}

func infinitelySized() -> Bool {
  switch (InfinitelySized.one, InfinitelySized.one) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 10 {{add missing case:}}
  case (.one, .one): return true
  case (.two, .two): return true
  }
  
  switch (MutuallyRecursive.one, MutuallyRecursive.one) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 10 {{add missing case:}}
  case (.one, .one): return true
  case (.two, .two): return true
  }
}
