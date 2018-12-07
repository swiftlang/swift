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

@_exported import Photos

// These methods need to be generic, so that the type parameter of the input
// argument is carried through as the type parameter of the return value.
#if os(iOS) || os(tvOS)
@available(iOS 8.0, tvOS 10.0, *)
extension PHChange {
  public func changeDetails<
    T : PHObject
  >(for object: T) -> PHObjectChangeDetails<T>? {
    return self.__changeDetails(for: object) as! PHObjectChangeDetails<T>?
  }

  public func changeDetails<
    T : PHObject
  >(for fetchResult: PHFetchResult<T>) -> PHFetchResultChangeDetails<T>? {
    return self.__changeDetails(
      for: fetchResult as! PHFetchResult<AnyObject>
    ) as! PHFetchResultChangeDetails<T>?
  }
}
#endif
