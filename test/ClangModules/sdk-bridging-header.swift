// RUN: %target-swift-frontend -parse -verify %s -import-objc-header %S/Inputs/sdk-bridging-header.h
// RUN: not %target-swift-frontend -parse %s -import-objc-header %S/Inputs/bad-bridging-header.h 2>&1 | FileCheck -check-prefix=CHECK-FATAL %s

// CHECK-FATAL: failed to import bridging header

// REQUIRES: objc_interop

import Foundation

let `true` = Predicate.truePredicate()
let not = Predicate.not()
let and = Predicate.and([])
let or = Predicate.or([not, and])

_ = Predicate.foobar() // expected-error{{value of type 'Predicate.Type' has no member 'foobar'}}
