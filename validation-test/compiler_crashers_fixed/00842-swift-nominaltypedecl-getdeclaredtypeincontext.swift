// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<T where I.c = compose<T> () {
typealias R = {
}
let a {
class d<I : N
