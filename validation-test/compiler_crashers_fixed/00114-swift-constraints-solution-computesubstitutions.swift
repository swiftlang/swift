// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol l : p {
}
protocol m {
  j f = p
}
f m : m {
  j f = o
}
func i<o : o, m : m n m.f == o> (l: m) {
}
k: m
}
func p<m>() -> [l<m>] {
    return []
}
f
m)
func f<o>() -> (o, o -> o) -> o {
   m o m.i = {
}
 {
   o) {
        p  }
}
protocol f {
   class func i()
}
class m: f{  class func i {}
protocol p {
    class func l()
}
class o: p {
    class func l() { }
