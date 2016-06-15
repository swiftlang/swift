// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
// This test does not crash on Linux.
import Foundation
let c: NSObject {
struct Q<T where I.E == c: U : AnyObject, object2: T) {
if true {
}
static let a = c
