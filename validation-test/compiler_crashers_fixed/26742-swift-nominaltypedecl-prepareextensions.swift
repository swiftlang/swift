// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T where g:A{class T{class B{var l}let a=B{}struct B<class B<T
