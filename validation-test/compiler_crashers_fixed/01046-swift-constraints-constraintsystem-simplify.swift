// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let x {
struct c : ExtensibleCollectionType>? {
}
}
class a {
class func b<H : b(([1
