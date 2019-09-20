// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/rdar30933988_enum.swiftmodule %S/Inputs/rdar30933988_enum.swift
// RUN:  %target-typecheck-verify-swift -I %t  -disable-access-control

import rdar30933988_enum

let _: E = .foo
// expected-error@-1 {{generic parameter 'T' could not be inferred}}
let _: E<Int> = .foo // Ok
let _: E = .bar(42)  // Ok
let _: E<String> = .bar(42)
// expected-error@-1 {{cannot convert value of type 'Int' to expected argument type 'String'}}
let _: E<Int> = .bar(42) // Ok
