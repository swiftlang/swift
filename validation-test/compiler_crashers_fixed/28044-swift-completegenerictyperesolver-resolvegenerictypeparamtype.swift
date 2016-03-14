// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<T{func a{if{protocol P{{{}}func b:T}}}class S{func a<h{func b<T where h.g=a
