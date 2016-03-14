// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let a {
struct d<T where A.B : d where T.d: String {
enum S<T where T.E == A
