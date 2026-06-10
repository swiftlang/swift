// RUN: %target-typecheck-verify-swift -language-mode 6 -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -language-mode 7 -verify-additional-prefix swift7-
// REQUIRES: swift7

func testTupleLabelMismatchFuncConversion(fn1: @escaping ((x: Int, y: Int)) -> Void,
                                          fn2: @escaping () -> (x: Int, Int)) {
  // Warn on mismatches in Swift 6, upgrading to an error for Swift 7
  let _: ((a: Int, b: Int)) -> Void = fn1
  // expected-swift6-warning@-1 {{tuple conversion from '(a: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}
  // expected-swift7-error@-2 {{tuple conversion from '(a: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}
  let _: ((x: Int, b: Int)) -> Void = fn1
  // expected-swift6-warning@-1 {{tuple conversion from '(x: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}
  // expected-swift7-error@-2 {{tuple conversion from '(x: Int, b: Int)' to '(x: Int, y: Int)' mismatches labels}}

  let _: () -> (y: Int, Int) = fn2
  // expected-swift6-warning@-1 {{tuple conversion from '(x: Int, Int)' to '(y: Int, Int)' mismatches labels}}
  // expected-swift7-error@-2 {{tuple conversion from '(x: Int, Int)' to '(y: Int, Int)' mismatches labels}}
  let _: () -> (y: Int, k: Int) = fn2
  // expected-swift6-warning@-1 {{tuple conversion from '(x: Int, Int)' to '(y: Int, k: Int)' mismatches labels}}
  // expected-swift7-error@-2 {{tuple conversion from '(x: Int, Int)' to '(y: Int, k: Int)' mismatches labels}}

  // Attempting to shuffle has always been illegal here
  let _: () -> (y: Int, x: Int) = fn2
  // expected-error@-1 {{cannot convert value of type '() -> (x: Int, Int)' to specified type '() -> (y: Int, x: Int)'}}

  // Losing labels is okay though.
  let _: () -> (Int, Int) = fn2

  // Gaining labels also okay.
  let _: ((x: Int, Int)) -> Void = fn1
  let _: () -> (x: Int, y: Int) = fn2
  let _: () -> (Int, y: Int) = fn2
}

func testErasure() {
  func checkCall<T, Arg0>(
    _ lhs: T, calling functionCall: (T, Arg0) throws -> Bool, _ argument0: Arg0
  ) {}

  func test(data: [(key: String, data: String)]) {
    checkCall(
      data.self,
      calling: { $0.contains(where: $1) },
      { $0 == (key: "Name", data: "kernel") } // Ok
    )

    checkCall(
      data.self,
      calling: { $0.contains(where: $1) },
      { $0 == ("Name", "kernel") } // Ok
    )

    checkCall(
      data.self,
      calling: { $0.contains(where: $1) },
      { $0 == (key: "Name", value: "kernel") } // Ok
    )
  }
}
