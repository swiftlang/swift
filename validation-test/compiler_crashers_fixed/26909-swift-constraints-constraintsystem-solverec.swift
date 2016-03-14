// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d<T where h:T
{
let r=b
protocol b struct c{
var f=b
