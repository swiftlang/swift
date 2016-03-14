// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var b{{
}
protocol a{
var d:e
}
protocol e
class b<T where a=d{
class A:a{
var d:e
