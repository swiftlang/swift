// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import Foundation
func g<b: Int = F>(c, object2: (a: Any) in 0)
protocol B : b {
typealias b
