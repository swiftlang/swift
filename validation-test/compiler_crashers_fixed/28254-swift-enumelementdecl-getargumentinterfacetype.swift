// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// REQUIRES: asserts
enum B<s{case c(c(func c
