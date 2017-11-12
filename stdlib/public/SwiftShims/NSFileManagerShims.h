//===--- NSFileManagerShims.h - Foundation decl. for FileManager overlay --===//
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

#import "FoundationShimSupport.h"

NS_BEGIN_DECLS

NS_INLINE NS_RETURNS_RETAINED NSURL *_Nullable __NSFileManagerReplaceItemAtURL(NSFileManager *self_, NSURL *originalItemURL, NSURL *newItemURL, NSString *_Nullable backupItemName, NSFileManagerItemReplacementOptions options, NSError **_Nullable error) {
    NSURL *result = nil;
    BOOL success = [self_ replaceItemAtURL:originalItemURL withItemAtURL:newItemURL backupItemName:backupItemName options:options resultingItemURL:&result error:error];
    return success ? result : nil;
}

NS_INLINE NS_RETURNS_RETAINED NSDirectoryEnumerator<NSURL *> *_Nullable __NSFileManagerEnumeratorAtURL(NSFileManager *self_, NSURL *url, NSArray<NSURLResourceKey> *_Nullable keys, NSDirectoryEnumerationOptions options, BOOL (^errorHandler)(NSURL *url, NSError *error) ) {
    return [self_ enumeratorAtURL:url includingPropertiesForKeys:keys options:options errorHandler:^(NSURL *url, NSError *error) {
        NSURL *realURL = url ?: error.userInfo[NSURLErrorKey];
        if (!realURL) {
            NSString *path = error.userInfo[NSFilePathErrorKey];
            realURL = [NSURL fileURLWithPath:path];
        }
        return errorHandler(realURL, error);
    }];
}

NS_END_DECLS
