// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
struct c<d : Sequence> {
    var b:  [c<d>] {
    return []
}
protocol a {
    class func c()
}
class b: a {
    class func c() { }
}
(b() as a).dynamicType.c()
func f<T : Boolean>(b: T) {
}
f(true as Boolean)
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> A  var d: b.Type
    func e() {
        d.e()
    }
}
b
protocol c : b { func b
otocol A {
  E == F>(f: B<T>)
}
struct  }
}
