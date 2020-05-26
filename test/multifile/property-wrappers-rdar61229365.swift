// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -o %t/main %t/main.swift %S/Inputs/rdar61229365.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test

class YourClass : MyClass {
  override init() {
    super.init()
  }
}

let object = YourClass()
object.check()