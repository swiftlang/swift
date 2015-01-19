// RUN: %target-swift-frontend -parse -primary-file %s %S/Inputs/associated_types_multi_file_helper.swift -verify

var x: X.AssocType = 0.0 as Float

// FIXME: <rdar://problem/16123805> Inferred associated types can't be used in expression contexts
var y = Y.AssocType() // expected-error{{'Y.Type' does not have a member named 'AssocType'}}
