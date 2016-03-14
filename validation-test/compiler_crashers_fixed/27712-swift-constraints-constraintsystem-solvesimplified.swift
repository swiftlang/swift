// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<T where g:a{class a<T where g:T{{}let f=a<T}class a{func f<var f=a{
