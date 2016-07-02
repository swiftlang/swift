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
@_exported import UIKit


// UITableView extensions
public extension IndexPath {
    
    /// Initialize for use with `UITableView` or `UICollectionView`.
    public init(row: Int, section: Int) {
        self.init(indexes: [section, row])
    }
    
    /// The section of this index path, when used with `UITableView`.
    ///
    /// - precondition: The index path must have exactly two elements.
    public var section: Int {
        get {
            precondition(count == 2, "Invalid index path for use with UITableView. This index path must contain exactly two indices specifying the section and row.")
            return self[0]
        }
        set {
            precondition(count == 2, "Invalid index path for use with UITableView. This index path must contain exactly two indices specifying the section and row.")
            self[0] = newValue
        }
    }
    
    /// The row of this index path, when used with `UITableView`.
    ///
    /// - precondition: The index path must have exactly two elements.
    public var row: Int {
        get {
            precondition(count == 2, "Invalid index path for use with UITableView. This index path must contain exactly two indices specifying the section and row.")
            return self[1]
        }
        set {
            precondition(count == 2, "Invalid index path for use with UITableView. This index path must contain exactly two indices specifying the section and row.")
            self[1] = newValue
        }
    }
}

// UICollectionView extensions
public extension IndexPath {
    
    /// Initialize for use with `UITableView` or `UICollectionView`.
    public init(item: Int, section: Int) {
        self.init(indexes: [section, item])
    }

    /// The item of this index path, when used with `UICollectionView`.
    ///
    /// - precondition: The index path must have exactly two elements.
    public var item: Int {
        get {
            precondition(count == 2, "Invalid index path for use with UICollectionView. This index path must contain exactly two indices specifying the section and item.")
            return self[1]
        }
        set {
            precondition(count == 2, "Invalid index path for use with UICollectionView. This index path must contain exactly two indices specifying the section and item.")
            self[1] = newValue
        }
    }}

public extension URLResourceValues {
    
    /// Returns a dictionary of UIImage objects keyed by size.
    @available(iOS 8.0, *)
    public var thumbnailDictionary : [URLThumbnailSizeKey : UIImage]? {
        return allValues[URLResourceKey.thumbnailDictionaryKey] as? [URLThumbnailSizeKey : UIImage]
    }
}
