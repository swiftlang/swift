// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/main.swiftmodule -module-name main %s %S/Inputs/issue-50578-other.swift

// https://github.com/apple/swift/issues/50578

public class Sub: Super {
  public override func foo(_ f: @escaping Alias<Bool>) {}
}
