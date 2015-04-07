// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

extension NSSet {
convenienc   init <A: A where A.B == D>(e: A.B) {
}
}
func b(c) -> <d>(() -> d) {
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
class d<c>: NSObject {
init(b: c) {
}
}
import Foundation
extension NSSet {
convenience init(array: Array) {
