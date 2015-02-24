// RUN: not --crash %target-swift-frontend %s -parse

// REQUIRES: objc_interop

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import Foundation
class A : NSObject {
let i: NSObject {
super.E == {
