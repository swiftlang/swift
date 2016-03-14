// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
extension Array {
struct X<Element : A: (s: Element, Bool]
return ")
protocol A : X<Element>
