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

import Foundation
@_exported import AppKit

public extension URLResourceValues {
    /// Returns all thumbnails as a single NSImage.
    @available(OSX 10.10, *)
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
    @available(OSX 10.10, *)
    public var thumbnailDictionary : [URLThumbnailSizeKey : NSImage]? {
        return allValues[URLResourceKey.thumbnailDictionaryKey] as? [URLThumbnailSizeKey : NSImage]
    }

}
