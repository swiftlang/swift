// RUN: not %target-swift-frontend %s -typecheck
//
// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/45725

import Foundation

class GenericClass<T>: NSObject {
    var someVar = [String: UndefinedClass]()
}
