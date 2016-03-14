// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
_== where{a{}{struct B{struct S<T where I:A{var:T.E={
