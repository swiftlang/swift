// RUN: not %target-swift-frontend %s -typecheck
//
// REQUIRES: objc_interop

import Foundation

class GenericClass<T>: NSObject {
    var someVar = [String: UndefinedClass]()
}
