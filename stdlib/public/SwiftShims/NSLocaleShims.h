//===--- NSLocaleShims.h - Foundation declarations for Locale overlay -----===//
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

NS_INLINE BOOL __NSLocaleIsAutoupdating(NS_NON_BRIDGED(NSLocale *)locale) {
    static dispatch_once_t onceToken;
    static Class autoLocaleClass;
    dispatch_once(&onceToken, ^{
        autoLocaleClass = (Class)objc_lookUpClass("NSAutoLocale");
    });
    return [locale isKindOfClass:autoLocaleClass];
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSLocale *)__NSLocaleCurrent() {
    return [NSLocale currentLocale];
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSLocale *)__NSLocaleAutoupdating() {
    return [NSLocale autoupdatingCurrentLocale];
}

NS_END_DECLS
