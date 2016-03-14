// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// Test is no longer valid as there is no longer `map` as a free function in Swift 3
// XFAIL: *
// DUPLICATE-OF: 26813-generic-enum-tuple-optional-payload.swift
// RUN: not --crash %target-swift-frontend %s -parse
let a{{map($0
