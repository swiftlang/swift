// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/disabled_access_control_base.swiftmodule %S/Inputs/disabled_access_control_base.swift
// RUN: %target-swift-frontend -disable-access-control -I %t -typecheck %s

import disabled_access_control_base

class B : A {
  public override var foo: String {
    return "ok"
  }

  override init() { }

  override func bar() {
  }

  override subscript(_ x: Int) -> String {
    return "ok"
  }
}
