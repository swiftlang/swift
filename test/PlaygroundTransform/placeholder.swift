// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/main --crash" 2>&1 | %FileCheck -check-prefix=CRASH-CHECK %s
// REQUIRES: executable_test

// NOTE: not.py is used above instead of "not --crash" because simctl's exit
// status doesn't reflect whether its child process crashed or not. So "not
// --crash %target-run ..." always fails when testing for the iOS Simulator.
// not.py also works on win32, where ! does not.
// Checking for ".[Ff]atal" because of d03a575279c.

func f(crash crash: Bool) -> Int {
  if crash {
    return <#T#>
    // CRASH-CHECK: {{[fF]}}atal error: attempt to evaluate editor placeholder: file {{.*}}/main.swift, line [[@LINE-1]]
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
