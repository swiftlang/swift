// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if true{func b{
protocol a
let a
enum b{var _=d<a
var d:a
