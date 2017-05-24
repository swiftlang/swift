//===--- NSCharacterSetShims.h - Foundation decl. for CharacterSet overlay ===//
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

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCharacterSet *) _NSURLComponentsGetURLUserAllowedCharacterSet(void) {
    return NSCharacterSet.URLUserAllowedCharacterSet;
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCharacterSet *) _NSURLComponentsGetURLPasswordAllowedCharacterSet(void) {
    return NSCharacterSet.URLPasswordAllowedCharacterSet;
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCharacterSet *) _NSURLComponentsGetURLHostAllowedCharacterSet(void) {
    return NSCharacterSet.URLHostAllowedCharacterSet;
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCharacterSet *) _NSURLComponentsGetURLPathAllowedCharacterSet(void) {
    return NSCharacterSet.URLPathAllowedCharacterSet;
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCharacterSet *) _NSURLComponentsGetURLQueryAllowedCharacterSet(void) {
    return NSCharacterSet.URLQueryAllowedCharacterSet;
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCharacterSet *) _NSURLComponentsGetURLFragmentAllowedCharacterSet(void) {
    return NSCharacterSet.URLFragmentAllowedCharacterSet;
}

NS_END_DECLS
