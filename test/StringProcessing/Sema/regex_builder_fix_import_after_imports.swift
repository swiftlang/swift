// RUN: %empty-directory(%t)

// Generate some dummy modules to import
// RUN: %target-swift-frontend -target %target-swift-5.7-abi-triple -emit-module -module-name A -o %t/A.swiftmodule %S/Inputs/dummy.swift
// RUN: %target-swift-frontend -target %target-swift-5.7-abi-triple -emit-module -module-name B -o %t/B.swiftmodule %S/Inputs/dummy.swift
// RUN: %target-swift-frontend -target %target-swift-5.7-abi-triple -emit-module -module-name C -o %t/C.swiftmodule %S/Inputs/dummy.swift

// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -enable-bare-slash-regex -I %t

// REQUIRES: swift_swift_parser

import A

import B

Regex { // expected-error {{regex builder requires the 'RegexBuilder' module be imported'}} {{14:9-9=\nimport RegexBuilder}}
  /abc/
}

import C
