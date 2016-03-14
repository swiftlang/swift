// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol e {
enum S<S : S<Int>] = b
}
enum A : B, e(()
