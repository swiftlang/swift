// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q{init(func a{struct D{func b{struct B<T where g:a{class A{class B<T{var _=B}struct B}class a{let i{var _={
