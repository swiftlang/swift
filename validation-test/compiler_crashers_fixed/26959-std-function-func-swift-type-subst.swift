// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum S<T{var _={class A{struct d<f where T:Collection{func d<T:a
