//===--- NSCoderShims.h - Foundation declarations for NSCoder overlay -----===//
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

NS_INLINE NS_RETURNS_RETAINED _Nullable id __NSCoderDecodeObject(NSCoder *self_, NSError **_Nullable error) {
    if (error) {
        return [self_ decodeTopLevelObjectAndReturnError:error];
    } else {
        return [self_ decodeObject];
    }
}

NS_INLINE NS_RETURNS_RETAINED _Nullable id __NSCoderDecodeObjectForKey(NSCoder *self_, NSString *key, NSError **_Nullable error) {
    if (error) {
        return [self_ decodeTopLevelObjectForKey:key error:error];
    } else {
        return [self_ decodeObjectForKey:key];
    }
}

NS_INLINE NS_RETURNS_RETAINED _Nullable id __NSCoderDecodeObjectOfClassForKey(NSCoder *self_, Class cls, NSString *key, NSError **_Nullable error) {
    if (error) {
        return [self_ decodeTopLevelObjectOfClass:cls forKey:key error:error];
    } else {
        return [self_ decodeObjectOfClass:cls forKey:key];
    }
}

NS_INLINE NS_RETURNS_RETAINED _Nullable id __NSCoderDecodeObjectOfClassesForKey(NSCoder *self_, NS_NON_BRIDGED(NSSet *)_Nullable classes, NSString *key, NSError **_Nullable error) {
    if (error) {
        return [self_ decodeTopLevelObjectOfClasses:(NSSet<Class> *)classes forKey:key error:error];
    } else {
        return [self_ decodeObjectOfClasses:(NSSet<Class> *)classes forKey:key];
    }
}

NS_END_DECLS
