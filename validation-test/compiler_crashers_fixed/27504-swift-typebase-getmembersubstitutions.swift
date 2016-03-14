// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T where g:a{class B<c{class A{let t:A{class B:A{let t:T
