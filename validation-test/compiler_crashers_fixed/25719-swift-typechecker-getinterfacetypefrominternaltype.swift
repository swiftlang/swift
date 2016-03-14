// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class A{
struct A:b
protocol b
func a{
class a{}class a{struct A:a
