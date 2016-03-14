// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class C<T where g: H.init<C<T
struct B<T.b : d = {
let c: C
