// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/associated_types_multi_file_helper.swift -verify

var x: X.AssocType = 0.0 as Float

var y = Y.AssocType()
