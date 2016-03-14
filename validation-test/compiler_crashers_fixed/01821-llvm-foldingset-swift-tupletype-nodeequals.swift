// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// REQUIRES: objc_interop
import Foundation
extension NSData {
class A, let n1: C {
super.d.init():
