// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module-path %t/main.swiftmodule -module-name main %s %S/Inputs/SR8045-other.swift

public class Sub: Super {
  public override func foo(_ f: @escaping Alias<Bool>) {}
}
