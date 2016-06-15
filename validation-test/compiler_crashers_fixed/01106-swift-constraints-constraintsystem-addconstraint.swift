// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
func a<g>() -> (g, g -> g) -> g {
return !(a)
enum c e() {
for f in 0..<1 {
d[f]  }
}
protocol f {
class func i()
}
class e: f{  class func i {}
