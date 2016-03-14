// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum S<D{func a{protocol P{func a}struct B:P{}enum f{enum S<T:T.E
