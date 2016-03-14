// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
