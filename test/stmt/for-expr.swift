// RUN: %target-run-simple-swift(-enable-experimental-feature ForExpressions) | %FileCheck %s

// REQUIRES: swift_feature_ForExpressions
// REQUIRES: executable_test

func f() -> String {
  for (i, x) in "hello".enumerated() {
    if i % 2 == 0 {
      x.uppercased()
    } else  {
      "*skip*"
    }
  }
}

print(f()) // CHECK: H*skip*L*skip*O

// Not fully exercising this one, as it will fail at run time due to `OutputSpan` not having backing storage.
// But at least we need this to type check successfully.
func g() -> OutputSpan<Int> {
  for (i, x) in "hello".enumerated() {
    42
  }
}
