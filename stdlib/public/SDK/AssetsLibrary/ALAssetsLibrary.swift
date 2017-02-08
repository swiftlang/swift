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

@_exported import AssetsLibrary // Clang module

extension ALAssetsLibrary { 
  @nonobjc
  public func enumerateGroupsWithTypes(_ types: UInt32,
      usingBlock enumerationBlock: ALAssetsLibraryGroupsEnumerationResultsBlock!,
      failureBlock: ALAssetsLibraryAccessFailureBlock!) {
    var types = types
    if types == ALAssetsGroupAll {
      types = ALAssetsGroupLibrary | ALAssetsGroupAlbum | ALAssetsGroupEvent | 
              ALAssetsGroupFaces | ALAssetsGroupSavedPhotos | 
              ALAssetsGroupPhotoStream
    }
    return enumerateGroups(
      withTypes: ALAssetsGroupType(types),
      using: enumerationBlock, 
      failureBlock: failureBlock)
  }
}
