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

#import <Foundation/Foundation.h>

extern /*"C"*/ NS_RETURNS_RETAINED id
NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(
    NSFileManager *NS_RELEASES_ARGUMENT _Nonnull self_,
    NSURL *NS_RELEASES_ARGUMENT _Nonnull originalItemURL,
    NSURL *NS_RELEASES_ARGUMENT _Nonnull newItemURL,
    NSString *NS_RELEASES_ARGUMENT _Nonnull backupItemName, NSUInteger options,
    NSError *_Nullable *_Nullable error) {

  NSURL *result = nil;
  [self_ replaceItemAtURL:originalItemURL
            withItemAtURL:originalItemURL
           backupItemName:backupItemName
                  options:(NSFileManagerItemReplacementOptions)options
         resultingItemURL:&result
                    error:error];
  [self_ release];
  [originalItemURL release];
  [newItemURL release];
  [backupItemName release];
  return [result retain];
}
