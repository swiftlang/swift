// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import Foundation
class d<c>: NSObject {
var b: c
func f<g>() -> (g, g -> g) -> g {
e e: ((g, g -> g) -> g)!
}
protocol e {
protocol d : b { func b
