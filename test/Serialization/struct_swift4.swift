// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_struct.swift
// RUN: %target-swift-frontend -emit-silgen -language-mode 4 -I %t %s -o /dev/null

import def_struct

// <rdar://problem/21933630> - setting let stored property in extension
// constructor
extension Burger {
  init(double pattyCount: Int) {
    self.pattyCount = pattyCount * 2
  }
}
