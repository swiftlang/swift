// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %s -I %S/Inputs/issue-47904/ -module-name TEST
// RUN: echo 'import TEST; x' | not %target-swift-frontend -typecheck - -I %S/Inputs/issue-47904/ -I %t

// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/47904

import ObjCPart

public typealias Alias = MyCollection

extension Alias: Collection {
    public subscript(index: Int) -> Any {
        return object(at: index)
    }

    public func index(after i: Int) -> Int {
        return i + 1
    }

    public var startIndex: Int {
        return 0
    }

    public var endIndex: Int {
        return count
    }
}
