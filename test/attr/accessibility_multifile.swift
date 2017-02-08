// RUN: rm -rf %t && mkdir -p %t
// RUN: %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -typecheck %t/file1.swift -primary-file %t/file2.swift -verify

// BEGIN file1.swift
private protocol P  {
  func privMethod()
}
public class C : P {
  public init() {}
  fileprivate func privMethod() {} // expected-note {{declared here}}
}

// BEGIN file2.swift
extension C {
  public func someFunc() {
    privMethod() // expected-error {{'privMethod' is inaccessible due to 'fileprivate' protection level}}
  }
}
