// RUN: %target-swift-ide-test -source-filename %s -print-module -module-to-print EnumExhaustivity -I %S/Inputs/custom-modules -swift-version 4 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s
// RUN: %target-swift-ide-test -source-filename %s -print-module -module-to-print EnumExhaustivity -I %S/Inputs/custom-modules -swift-version 5 | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s

// CHECK-4-LABEL: {{^}}_nonexhaustive enum RegularEnum : {{.+}} {
// CHECK-5-LABEL: {{^}}enum RegularEnum : {{.+}} {
// CHECK:      case A
// CHECK-NEXT: case B
// CHECK-NEXT: {{^}$}}

// CHECK-4-LABEL: {{^}}enum ExhaustiveEnum : {{.+}} {
// CHECK-5-LABEL: {{^}}_exhaustive enum ExhaustiveEnum : {{.+}} {
// CHECK:      case A
// CHECK-NEXT: case B
// CHECK-NEXT: {{^}$}}

// CHECK-5-LABEL: {{^}}_exhaustive enum RegularEnumTurnedExhaustive
// CHECK-5-LABEL: {{^}}_exhaustive enum AnotherRegularEnumTurnedExhaustive
// CHECK-4-LABEL: {{^}}_nonexhaustive enum RegularEnumTurnedExhaustiveThenBackViaAPINotes
// CHECK-5-LABEL: {{^}}_exhaustive enum ForwardDeclaredTurnedExhaustive
// CHECK-5-LABEL: {{^}}_exhaustive enum ForwardDeclaredOnly
