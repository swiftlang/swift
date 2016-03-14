// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
import Foundation
class c
protocol a {
var a: A {
("""
}
protocol A : A<T> {
typealias A :
