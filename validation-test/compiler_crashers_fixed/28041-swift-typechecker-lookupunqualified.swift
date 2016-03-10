// Test is no longer valid as there is no longer `map` as a free function in Swift 3
// XFAIL: *
// DUPLICATE-OF: 26813-generic-enum-tuple-optional-payload.swift
// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

let a{{map($0
