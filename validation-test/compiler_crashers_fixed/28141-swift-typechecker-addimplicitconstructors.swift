// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var _={class A{}struct Q{enum B{struct B{struct Q{class C<T where g:A{struct c<T{struct c{}let c{var _=c
