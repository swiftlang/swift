// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import Foundation
class k<f>: NSObject {
    d e: f
    g(e: f) {
        j        h.g()
    }
}
d
protocol i : d { func d
i
