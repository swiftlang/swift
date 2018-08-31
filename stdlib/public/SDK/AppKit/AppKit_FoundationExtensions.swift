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

import Foundation
@_exported import AppKit

// NSCollectionView extensions
public extension IndexPath {
    
    /// Initialize for use with `NSCollectionView`.
    public init(item: Int, section: Int) {
        self.init(indexes: [section, item])
    }
    
    /// The item of this index path, when used with `NSCollectionView`.
    ///
    /// - precondition: The index path must have exactly two elements.
    public var item : Int {
        get {
            precondition(count == 2, "Invalid index path for use with NSCollectionView. This index path must contain exactly two indices specifying the section and item.")
            return self[1]
        }
        set {
            precondition(count == 2, "Invalid index path for use with NSCollectionView. This index path must contain exactly two indices specifying the section and item.")
            self[1] = newValue
        }
    }
    
    /// The section of this index path, when used with `NSCollectionView`.
    ///
    /// - precondition: The index path must have exactly two elements.
    public var section : Int {
        get {
            precondition(count == 2, "Invalid index path for use with NSCollectionView. This index path must contain exactly two indices specifying the section and item.")
            return self[0]
        }
        set {
            precondition(count == 2, "Invalid index path for use with NSCollectionView. This index path must contain exactly two indices specifying the section and item.")
            self[0] = newValue
        }
    }
    
}

public extension URLResourceValues {
    /// Returns all thumbnails as a single NSImage.
    @available(macOS 10.10, *)
    public var thumbnail : NSImage? {
        return allValues[URLResourceKey.thumbnailKey] as? NSImage
    }
    
    /// The color of the assigned label.
    public var labelColor: NSColor? {
        return allValues[URLResourceKey.labelColorKey] as? NSColor
    }
    
    /// The icon normally displayed for the resource
    public var effectiveIcon: AnyObject? {
        return allValues[URLResourceKey.effectiveIconKey] as? NSImage
    }
    
    /// The custom icon assigned to the resource, if any (Currently not implemented)
    public var customIcon: NSImage? {
        return allValues[URLResourceKey.customIconKey] as? NSImage
    }
    
    /// Returns a dictionary of NSImage objects keyed by size.
    @available(macOS 10.10, *)
    public var thumbnailDictionary : [URLThumbnailDictionaryItem : NSImage]? {
        return allValues[URLResourceKey.thumbnailDictionaryKey] as? [URLThumbnailDictionaryItem : NSImage]
    }

}
