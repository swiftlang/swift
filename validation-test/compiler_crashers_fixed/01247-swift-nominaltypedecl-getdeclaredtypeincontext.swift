// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var e: Int -> Int = {
return $0
struct d<f : e, g: e where g.h = f.h> { : C {
func g<T where T>(f: B<T>) {
}
}
class A {
