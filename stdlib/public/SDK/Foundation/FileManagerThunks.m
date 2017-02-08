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

#import <Foundation/Foundation.h>

extern /*"C"*/ NS_RETURNS_RETAINED id
NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(
    NSFileManager *NS_RELEASES_ARGUMENT _Nonnull self_,
    NSURL *NS_RELEASES_ARGUMENT _Nonnull originalItemURL,
    NSURL *NS_RELEASES_ARGUMENT _Nonnull newItemURL,
    NSString *NS_RELEASES_ARGUMENT _Nullable backupItemName, NSUInteger options,
    NSError *_Nullable *_Nullable error) {

  NSURL *result = nil;
  BOOL success = [self_ replaceItemAtURL:originalItemURL
                           withItemAtURL:newItemURL
                          backupItemName:backupItemName
                                 options:(NSFileManagerItemReplacementOptions)options
                        resultingItemURL:&result
                                   error:error];
  [self_ release];
  [originalItemURL release];
  [newItemURL release];
  [backupItemName release];
  return success ? [result retain] : nil;
}

extern /*"C"*/ NS_RETURNS_RETAINED id
NS_Swift_NSFileManager_enumeratorAt_includingPropertiesForKeys_options_errorHandler(
    NSFileManager *NS_RELEASES_ARGUMENT _Nonnull self_,
    NSURL *NS_RELEASES_ARGUMENT _Nonnull url,
    NSArray *NS_RELEASES_ARGUMENT _Nullable keys,
    NSUInteger options,
    BOOL (^_Nonnull errorHandler)(NSURL * _Nonnull url, NSError * _Nonnull error) ) {

  NSDirectoryEnumerator *result = [self_ enumeratorAtURL:url
                              includingPropertiesForKeys:keys
                                                 options:(NSDirectoryEnumerationOptions)options
                                            errorHandler:^(NSURL * url, NSError *error) {

    NSURL *realURL = url ?: error.userInfo[NSURLErrorKey];
    if (!realURL) {
      NSString *path = error.userInfo[NSFilePathErrorKey];
      realURL = [NSURL fileURLWithPath:path];
    }
    return errorHandler(realURL, error);

  }];
  [self_ release];
  [url release];
  [keys release];
  [errorHandler release];
  return [result retain];
}
