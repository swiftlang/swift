// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<T where I:a{{{}}struct Q{{
}struct B{class b{var f=B<T
var f=B{
