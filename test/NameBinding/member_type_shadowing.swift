// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/MemberTypesInClasses.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -verify

import MemberTypesInClasses

protocol P {
  associatedtype Member
}

extension RootClass: P {
  typealias Member = SubClass.Member
}

class A {
  enum Reusable {
    case option1
    case option2
  }
}

class B: A {
  enum Reusable {
    case option1 // expected-note {{'option1' declared here}}
  }

  func process() {
    _ = B.Reusable.option1
    _ = B.Reusable.option2 // expected-error {{type 'B.Reusable' has no member 'option2'; did you mean 'option1'?}}
  }
}
