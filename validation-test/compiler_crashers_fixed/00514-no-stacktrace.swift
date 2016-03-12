// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

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
