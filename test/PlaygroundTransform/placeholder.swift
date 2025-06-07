// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main5 %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main6 %t/main.swift

// RUN: %target-codesign %t/main5
// RUN: %target-codesign %t/main6

// RUN: %target-run %t/main5 | %FileCheck %s
// RUN: %target-run %t/main6 | %FileCheck %s

// RUN: %{python} %S/../Inputs/not.py "%target-run %t/main5 --crash" 2>&1 | %FileCheck -check-prefix=CRASH-CHECK %s
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/main6 --crash" 2>&1 | %FileCheck -check-prefix=CRASH-CHECK %s

// REQUIRES: executable_test

// The runtime error format changed after the 5.3 release.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// NOTE: not.py is used above instead of "not --crash" because simctl's exit
// status doesn't reflect whether its child process crashed or not. So "not
// --crash %target-run ..." always fails when testing for the iOS Simulator.
// not.py also works on win32, where ! does not.

func f(crash crash: Bool) -> Int {
  if crash {
    return <#T#>
    // CRASH-CHECK: {{.*}}/main.swift:[[@LINE-1]]: Fatal error: attempt to evaluate editor placeholder
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
