//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Photos

#if os(macOS)
@available(macOS 10.13, *)
extension PHProjectChangeRequest {
    @available(macOS 10.14, *)
    public func removeAssets<T>(_ assets: T) where T: Collection, T.Element == PHAsset {
        let array = Array(assets) as NSArray
        self.__removeAssets(array)
    }

    @available(macOS 10.14, *)
    public func removeAssets(_ assets: PHFetchResult<PHAsset>) {
        self.__removeAssets(assets)
    }
}
#endif
