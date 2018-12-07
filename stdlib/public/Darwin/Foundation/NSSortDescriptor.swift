//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

extension NSSortDescriptor {
    public convenience init<Root, Value>(keyPath: KeyPath<Root, Value>, ascending: Bool) {
        self.init(key: _bridgeKeyPathToString(keyPath), ascending: ascending)
    }
    
    public convenience init<Root, Value>(keyPath: KeyPath<Root, Value>, ascending: Bool, comparator cmptr: @escaping Foundation.Comparator) {
        self.init(key: _bridgeKeyPathToString(keyPath), ascending: ascending, comparator: cmptr)
    }
    
    public var keyPath: AnyKeyPath? {
        guard let key = self.key else { return nil }
        return _bridgeStringToKeyPath(key)
    }
}
