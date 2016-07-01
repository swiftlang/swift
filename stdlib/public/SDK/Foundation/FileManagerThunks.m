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

extern /*"C"*/ NS_RETURNS_RETAINED id NS_Swift_NSFileManager_replaceItemAtURL_withItemAtURL_backupItemName_options(
    NSFileManager *NS_RELEASES_ARGUMENT __nonnull self_,
    NSURL *NS_RELEASES_ARGUMENT __nonnull originalItemURL,
    NSURL *NS_RELEASES_ARGUMENT __nonnull newItemURL,
    NSString *NS_RELEASES_ARGUMENT __nonnull backupItemName,
    NSUInteger options,
    NSError *__nullable *__nullable error) {

    NSURL *result = nil;
    [self_ replaceItemAtURL:originalItemURL withItemAtURL:originalItemURL backupItemName:backupItemName options:(NSFileManagerItemReplacementOptions)options resultingItemURL:&result error:error];
    [self_ release];
    [originalItemURL release];
    [newItemURL release];
    [backupItemName release];
    return [result retain];
}