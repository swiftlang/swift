// RUN: %target-typecheck-verify-swift

enum MyError: Error { case bad }

func something() throws {}
func dangerous() throws { throw MyError.bad }
func safe() {}

func testStrictCatch() {
  // 1. Standard Empty Catch -> WARN
  do {
    try something()
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 2. Explicit Ignore -> PASS
  do {
    try something()
  } catch _ {
    // Valid: Explicit intent to ignore
  }

  // 3. Explicit Ignore -> PASS
  do {
    try something()
  } catch {
    _ = error // Valid: User silenced it their own way
  }

  // 4. Other Statements -> PASS
  do {
    try something()
  } catch {
    if true {}
  }

  // 5. Catch All with Explicit Binding -> WARN
  do {
    try something()
  } catch let e { // expected-warning {{immutable value 'e' was never used; consider replacing with '_' or removing it}}
    // expected-warning@-1 {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }
}

func testCatchChain() {
  // 6. Catch-All in a Chain -> PASS
  do {
    try something()
  } catch MyError.bad {
    print("bad")
  } catch {
    // Valid: implicitly ignore the rest
  }
}

func testDeepRecursion(flag: Bool, val: Int) {
  // 7. Hidden in If/Else -> WARN
  do {
    if flag {
      try something()
    } else {
      try something()
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 8. Hidden in Guard -> WARN
  do {
    guard flag else {
      try something()
      return
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 9. Hidden in Loops -> WARN
  do {
    for _ in 0..<5 {
      try something()
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 10. Hidden in Switch -> WARN
  do {
    switch val {
    case 1:
      try something()
    default:
      break
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 11. Explicit Throw Stmt -> WARN
  do {
    if flag {
      throw MyError.bad
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 12. Nested Rethrow -> WARN
  do {
    do {
      try something()
    } catch {
      throw MyError.bad
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }
}

func testReachabilityConflict(flag: Bool) {
  // 13. True Unreachable -> Should warn 'unreachable', NOT 'empty catch'
  do {
    safe()
  } catch { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  }

  // 14. Deeply Reachable -> Should warn 'empty catch', NOT 'unreachable'
  do {
    if flag {
      try dangerous()
    }
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }
}

func testConditionalCompilation() {
  // 15. Throw in active #if block -> WARN
  do {
    #if true
    try something()
    #else
    safe()
    #endif
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 16. Throw in inactive #if block (Active block safe) -> NO WARN
  do {
    #if false
    try something()
    #else
    safe()
    #endif
  } catch {
  }

  // 17. Throw in both blocks -> WARN
  do {
    #if true
    try something()
    #else
    try something()
    #endif
  } catch { // expected-warning {{empty catch block silences all errors; consider using 'try?', or use 'catch _' explicitly to silence this warning}}
  }

  // 18. Safe in both blocks -> WARN (Unreachable)
  do {
    #if true
    safe()
    #else
    safe()
    #endif
  } catch { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  }
}
