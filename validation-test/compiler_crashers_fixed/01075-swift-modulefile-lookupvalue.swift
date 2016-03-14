// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d<f : e, g: e where g.h == f.h> {      typealias F = InA.B
