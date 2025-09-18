// RUN: %target-typecheck-verify-swift -enable-experimental-feature ForExpressions -enable-experimental-feature ThenStatements

func f() -> String {
  for (i, x) in "hello".enumerated() {
    then if i % 2 == 0 {
      x.uppercased()
    } else  {
      x
    }
  }
}
print(f())
