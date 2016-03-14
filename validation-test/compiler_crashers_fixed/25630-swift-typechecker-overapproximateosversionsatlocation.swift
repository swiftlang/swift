// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A:A{class b<T where g.h=f{}typealias e:P{}func g.protocol P
