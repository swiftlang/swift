// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class e {
var _ = d() {
class A {
class func a() -> Self {
}
}
func b<T>(t: AnyObject.Type) -> T! {
}
}
class d<j : i, f : i where j.i == f> : e {
}
class d<j, f> {
}
protocol i {
}
protocol e {
}
protocol i : d { func d
