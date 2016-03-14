// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a{
protocol A{{{}}
func r typealias e:a
class a
typealias e:b
class b
