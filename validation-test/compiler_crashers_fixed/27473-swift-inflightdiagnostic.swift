// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A
class B<T where T:b{class B<T{
enum e
struct B
{{{
}
}let a=A{{}}{
var f=e
