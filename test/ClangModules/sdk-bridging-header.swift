// RUN: %target-swift-frontend -parse -verify %s -import-objc-header %S/Inputs/sdk-bridging-header.h

// REQUIRES: objc_interop

import Foundation

let `true` = Predicate.truePredicate()
let not = Predicate.not()
let and = Predicate.and([])
let or = Predicate.or([not, and])

_ = Predicate.foobar() // expected-error{{'Predicate.Type' does not have a member named 'foobar'}}
