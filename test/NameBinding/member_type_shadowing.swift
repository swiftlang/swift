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


