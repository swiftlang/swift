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
