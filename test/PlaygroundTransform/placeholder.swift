// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: not --crash %target-run %t/main --crash 2>&1 | %FileCheck -check-prefix=CRASH-CHECK %s
// REQUIRES: executable_test

func f(crash crash: Bool) -> Int {
  if crash {
    return <#T#>
    // CRASH-CHECK: {{[Ff]}}atal error: attempt to evaluate editor placeholder: file {{.*}}/main.swift, line [[@LINE-1]]
  } else {
    return 42
  }
}

if CommandLine.arguments.last == "--crash" {
  print("the value is \(f(crash: true))")
} else {
  print("the value is \(f(crash: false))")
  // CHECK: the value is 42
}
