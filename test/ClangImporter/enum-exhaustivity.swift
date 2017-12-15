// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules -verify -warnings-as-errors -enable-nonfrozen-enum-exhaustivity-diagnostics

// RUN: %target-swift-ide-test -source-filename %s -print-module -module-to-print EnumExhaustivity -I %S/Inputs/custom-modules | %FileCheck -check-prefix=CHECK %s

// CHECK-LABEL: {{^}}enum RegularEnum : {{.+}} {
// CHECK:      case A
// CHECK-NEXT: case B
// CHECK-NEXT: {{^}$}}

// Note that we don't print '@_frozen' here yet.
// CHECK-LABEL: {{^}}enum ExhaustiveEnum : {{.+}} {
// CHECK:      case A
// CHECK-NEXT: case B
// CHECK-NEXT: {{^}$}}

import EnumExhaustivity

func test(_ value: RegularEnum, _ exhaustiveValue: ExhaustiveEnum) {
  switch value { // expected-error {{switch must be exhaustive}} expected-note {{do you want to add a default clause?}}
  case .A: break
  case .B: break
  }

  switch exhaustiveValue { // always okay
  case .A: break
  case .B: break
  }
}
