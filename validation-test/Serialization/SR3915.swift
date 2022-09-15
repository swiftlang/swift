// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -o %t/M.swiftmodule %s %S/Inputs/issue-46500-other.swift

// https://github.com/apple/swift/issues/46500

public enum A {}

public extension A {
  public enum B {}
}
