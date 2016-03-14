// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if true {
func f: NSObject {
}
struct S<T where T: NSObject {
class A
