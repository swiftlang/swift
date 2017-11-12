//===--- NSTimeZoneShims.h - Foundation declarations for TimeZone overlay -===//
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

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSTimeZone *)__NSTimeZoneAutoupdating() {
    return [NSTimeZone localTimeZone];
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSTimeZone *)__NSTimeZoneCurrent() {
    return [NSTimeZone systemTimeZone];
}

NS_INLINE BOOL __NSTimeZoneIsAutoupdating(NS_NON_BRIDGED(NSTimeZone *)timeZone) {
    static dispatch_once_t onceToken;
    static Class autoTimeZoneClass;
    dispatch_once(&onceToken, ^{
        autoTimeZoneClass = (Class)objc_lookUpClass("__NSLocalTimeZone");
    });
    return [timeZone isKindOfClass:autoTimeZoneClass];
}

NS_END_DECLS
