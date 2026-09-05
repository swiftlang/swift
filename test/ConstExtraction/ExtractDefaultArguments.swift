// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -module-name ResilientLib -enable-library-evolution -o %t/ResilientLib.swiftmodule %S/Inputs/DefaultArgumentsLib.swift

// RUN: %target-swift-frontend -typecheck -emit-const-values-path %t/ExtractDefaultArguments.swiftconstvalues -const-gather-top-level-constant value -I %t -primary-file %s
// RUN: cat %t/ExtractDefaultArguments.swiftconstvalues 2>&1 | %FileCheck %s

import ResilientLib

let value = Thing(name: "n", locallyPassed: false)

// CHECK:      "valueKind": "InitCall",
// CHECK:          "label": "name",
// CHECK-NEXT:     "type": "Swift.String",
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": "n"
// CHECK:          "label": "flag",
// CHECK-NEXT:     "type": "Swift.Bool",
// CHECK-NEXT:     "valueKind": "DefaultArgument"
// CHECK:          "label": "items",
// CHECK-NEXT:     "type": "Swift.Array<Swift.String>",
// CHECK-NEXT:     "valueKind": "DefaultArgument"
// CHECK:          "label": "opt",
// CHECK-NEXT:     "type": "Swift.Optional<Swift.String>",
// CHECK-NEXT:     "valueKind": "NilLiteral"
// CHECK:          "label": "locallyPassed",
// CHECK-NEXT:     "type": "Swift.Bool",
// CHECK-NEXT:     "valueKind": "RawLiteral",
// CHECK-NEXT:     "value": "false"
