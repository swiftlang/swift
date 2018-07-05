//===--- NSKeyedArchiverShims.h - Found. decl. for NSKeyedArchiver overlay ===//
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

NS_INLINE NS_RETURNS_RETAINED _Nullable id __NSKeyedUnarchiverUnarchiveObject(id Self_, NS_NON_BRIDGED(NSData *)data, NSError **_Nullable error) {
    if (error) {
        return [Self_ unarchiveTopLevelObjectWithData:(NSData *)data error:error];
    } else {
        return [Self_ unarchiveObjectWithData:(NSData *)data];
    }
}

NS_INLINE NS_RETURNS_RETAINED id _Nullable __NSKeyedUnarchiverSecureUnarchiveObjectOfClass(Class cls, NSData *data, NSError * _Nullable * _Nullable error) {
    return [NSKeyedUnarchiver unarchivedObjectOfClass:cls fromData:data error:error];
}

NS_INLINE NS_RETURNS_RETAINED id _Nullable __NSKeyedUnarchiverSecureUnarchiveObjectOfClasses(NS_NON_BRIDGED(NSSet *) classes, NSData *data, NSError * _Nullable * _Nullable error) {
    return [NSKeyedUnarchiver unarchivedObjectOfClasses:classes fromData:data error:error];
}

NS_END_DECLS
