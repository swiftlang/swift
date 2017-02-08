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

@_silgen_name("NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options")
internal func NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(
    _ self_: AnyObject,
    _ originalItemURL: AnyObject,
    _ newItemURL: AnyObject,
    _ backupItemName: NSString?,
    _ options: FileManager.ItemReplacementOptions,
    _ error: NSErrorPointer) -> NSURL?

@_silgen_name("NS_Swift_NSFileManager_enumeratorAt_includingPropertiesForKeys_options_errorHandler")
internal func NS_Swift_NSFileManager_enumeratorAt_includingPropertiesForKeys_options_errorHandler(
    _ self_ : AnyObject,
    _ url: AnyObject,
    _ keys: NSArray?,
    _ options: FileManager.DirectoryEnumerationOptions,
    _ errorHandler: @escaping @convention(block) (NSURL, NSError) -> Bool) -> FileManager.DirectoryEnumerator?

extension FileManager {
    /*
     renamed syntax should be:
     public func replaceItem(at originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options : FileManager.ItemReplacementOptions = []) throws -> URL?
    */
    
    @available(*, deprecated, renamed:"replaceItemAt(_:withItemAt:backupItemName:options:)")
    public func replaceItemAtURL(originalItemURL: NSURL, withItemAtURL newItemURL: NSURL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> NSURL? {
        var error: NSError?
        if let result = NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(self, originalItemURL, newItemURL, backupItemName as NSString?, options, &error) {
            return result
        }
        throw error!
    }

    @available(OSX 10.6, iOS 4.0, *)
    public func replaceItemAt(_ originalItemURL: URL, withItemAt newItemURL: URL, backupItemName: String? = nil, options: FileManager.ItemReplacementOptions = []) throws -> NSURL? {
        var error: NSError?
        if let result = NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(self, originalItemURL as NSURL, newItemURL as NSURL, backupItemName as NSString?, options, &error) {
            return result
        }
        throw error!
    }
    
    @available(OSX 10.6, iOS 4.0, *)
    public func enumerator(at url: URL, includingPropertiesForKeys keys: [URLResourceKey]?, options mask: FileManager.DirectoryEnumerationOptions = [], errorHandler handler: ((URL, Error) -> Bool)? = nil) -> FileManager.DirectoryEnumerator? {
        return NS_Swift_NSFileManager_enumeratorAt_includingPropertiesForKeys_options_errorHandler(self, url as NSURL, keys as NSArray?, mask, { (url, error) in
            var errorResult = true
            if let h = handler {
                errorResult = h(url as URL, error)
            }
            return errorResult
        })
    }

}
