//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
import _SwiftFoundationOverlayShims

extension FileManager {
    /*
     renamed syntax should be:
     public func replaceItem(at originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options : FileManager.ItemReplacementOptions = []) throws -> URL?
    */
    
    @available(*, deprecated, renamed:"replaceItemAt(_:withItemAt:backupItemName:options:)")
    public func replaceItemAtURL(originalItemURL: NSURL, withItemAtURL newItemURL: NSURL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> NSURL? {
        var error: NSError? = nil
        guard let result = __NSFileManagerReplaceItemAtURL(self, originalItemURL as URL, newItemURL as URL, backupItemName, options, &error) else { throw error! }
        return result as NSURL
    }

    @available(swift, obsoleted: 4)
    @available(macOS 10.6, iOS 4.0, *)
    public func replaceItemAt(_ originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> NSURL? {
        var error: NSError?
        guard let result = __NSFileManagerReplaceItemAtURL(self, originalItemURL, newItemURL , backupItemName, options, &error) else { throw error! }
        return result as NSURL
    }
    
    @available(swift, introduced: 4)
    @available(macOS 10.6, iOS 4.0, *)
    public func replaceItemAt(_ originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> URL? {
        var error: NSError?
        guard let result = __NSFileManagerReplaceItemAtURL(self, originalItemURL, newItemURL , backupItemName, options, &error) else { throw error! }
        return result
    }

    @available(macOS 10.6, iOS 4.0, *)
    @nonobjc
    public func enumerator(at url: URL, includingPropertiesForKeys keys: [URLResourceKey]?, options mask: FileManager.DirectoryEnumerationOptions = [], errorHandler handler: ((URL, Error) -> Bool)? = nil) -> FileManager.DirectoryEnumerator? {
        return __NSFileManagerEnumeratorAtURL(self, url, keys, mask, { (url, error) in
            var errorResult = true
            if let h = handler {
                errorResult = h(url as URL, error)
            }
            return errorResult
        })
    }
}
