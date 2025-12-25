// RUN: %target-typecheck-verify-swift

func canThrow() throws {}

func test() {
  // 1. Should warn and suggest Fix-it
  do {
    try canThrow()
  } catch { // expected-warning {{catch block is empty, errors will silently be ignored}} {{12-12= _ = error }}
  }

  // 2. Should NOT warn (User used the Fix-it)
  do {
    try canThrow()
  } catch {
    _ = error
  }

  // 3. Should NOT warn (User has other logic)
  do {
    try canThrow()
  } catch {
    print("Logged")
  }

  // 4. Test case mentioned in the issue
  do {
    try canThrow()
  } catch { // expected-warning {{catch block is empty, errors will silently be ignored}} {{12-12= _ = error }}
    /* do nothing */
  }
}
