// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a{
struct S<T where g:T{
{
}
class B
struct S{
func d{
let a=B
a{{}}}}
struct B{var _=a{
