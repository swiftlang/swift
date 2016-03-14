// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a {
func c(b[c: b[c
func a(""foo"""foobar""
class b: b {
var b = c
