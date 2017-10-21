// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -parse-as-library
// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs
// RUN: %target-swift-frontend -typecheck %S/Inputs/MutualDependencyHelper.swift -enable-source-import -I %S

// RUN: %target-swift-frontend -interpret -I %S/Inputs -enable-source-import %s -verify


import MutualDependencyHelper

public class MyClass {
  public var delegate : MyDelegate // expected-note {{'self.delegate' not initialized}}

  public init() {} // expected-error {{return from initializer without initializing all stored properties}}
}
