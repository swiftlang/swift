// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let c{func b{}
protocol A
func b
enum S<h:A{class A{
enum B<T where h:b
