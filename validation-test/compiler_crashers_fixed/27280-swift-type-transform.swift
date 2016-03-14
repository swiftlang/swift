// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum S<T where h:t{class B{class B:a{struct X<T:a{}}class a
