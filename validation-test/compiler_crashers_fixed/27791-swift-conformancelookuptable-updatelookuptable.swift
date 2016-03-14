// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func b<T{
protocol A{
let:a
class a:A
func a:T.e
