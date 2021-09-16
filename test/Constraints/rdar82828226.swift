// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

let int: Int = 0
func a(_ a: NSNumber) {}
a(int)// expected-error {{cannot convert value of type 'Int' to expected argument type 'NSNumber'}} {{6-6= as NSNumber}}
