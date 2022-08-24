// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/nested_opened_archetype_other.swiftmodule %S/Inputs/nested_opened_archetype_other.swift
// RUN: %target-swift-frontend -emit-sil %s -I %t

import nested_opened_archetype_other

struct SP : P {
  func foo() -> SQ { return SQ() }
}

struct SQ : Q {}

func g() {
  f(p: SP())
}
