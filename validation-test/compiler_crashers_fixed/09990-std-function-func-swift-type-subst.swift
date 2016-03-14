// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol P{{}{{}}typealias F}class d<c:P{{{}}func g<T{class a<T>:d
