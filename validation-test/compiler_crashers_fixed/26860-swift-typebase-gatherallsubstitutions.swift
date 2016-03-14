// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A{{}typealias b:A}class B<T:A{class c<T>:A{let a{{}}let f=a
