//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// ALAssetsLibrary.h
//===----------------------------------------------------------------------===//
extension ALAssetsLibrary { 
  @nonobjc
  public func enumerateGroupsWithTypes(var types: UInt32, 
      usingBlock enumerationBlock: ALAssetsLibraryGroupsEnumerationResultsBlock!,
      failureBlock: ALAssetsLibraryAccessFailureBlock!) {
    if (types == ALAssetsGroupAll) {
      types = ALAssetsGroupLibrary | ALAssetsGroupAlbum | ALAssetsGroupEvent | 
              ALAssetsGroupFaces | ALAssetsGroupSavedPhotos | 
              ALAssetsGroupPhotoStream
    }
    return enumerateGroupsWithTypes(ALAssetsGroupType(types), 
                                    usingBlock: enumerationBlock, 
                                    failureBlock: failureBlock)

  }
}
