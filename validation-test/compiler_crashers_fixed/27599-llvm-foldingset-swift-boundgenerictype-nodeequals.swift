// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<d{let a=0
struct B<T where g:d{struct Q<T{
class a
func b{let st=a{
