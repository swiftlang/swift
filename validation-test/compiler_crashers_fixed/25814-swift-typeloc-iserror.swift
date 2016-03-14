// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B{
let a{protocol A:A
protocol A}
class A:d struct d:a
