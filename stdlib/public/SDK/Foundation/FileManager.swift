//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

@_silgen_name("NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options")
internal func NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(
    _ self_: AnyObject,
    _ originalItemURL: AnyObject,
    _ newItemURL: AnyObject,
    _ backupItemName: String?,
    _ options: FileManager.ItemReplacementOptions,
    _ error: NSErrorPointer) -> NSURL?


extension FileManager {
    /*
     renamed syntax should be:
     public func replaceItem(at originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options : FileManager.ItemReplacementOptions = []) throws -> URL?
    */
    
    @available(*, deprecated, renamed:"replaceItemAt(_:withItemAt:backupItemName:options:)")
    public func replaceItemAtURL(originalItemURL: NSURL, withItemAtURL newItemURL: NSURL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> NSURL? {
        var error: NSError? = nil
        if let result = NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(self, originalItemURL, newItemURL, backupItemName, options, &error) {
            return result
        }
        throw error!
    }

    @available(OSX 10.6, iOS 4.0, *)
    public func replaceItemAt(_ originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> NSURL? {
        var error: NSError? = nil
        if let result = NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(self, originalItemURL as NSURL, newItemURL as NSURL, backupItemName, options, &error) {
            return result
        }
        throw error!
    }
}
