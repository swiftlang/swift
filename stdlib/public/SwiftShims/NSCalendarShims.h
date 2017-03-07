//===--- NSCalendarShims.h - Foundation declarations for Calendar overlay ------===//
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

NS_INLINE BOOL __NSCalendarIsAutoupdating(NS_NON_BRIDGED(NSCalendar *) calendar) {
    static dispatch_once_t onceToken;
    static Class autoCalendarClass;
    static Class olderAutoCalendarClass; // Pre 10.12/10.0
    dispatch_once(&onceToken, ^{
        autoCalendarClass = (Class)objc_lookUpClass("_NSAutoCalendar");
        olderAutoCalendarClass = (Class)objc_lookUpClass("NSAutoCalendar");
    });
    return (autoCalendarClass && [calendar isKindOfClass:autoCalendarClass]) || (olderAutoCalendarClass && [calendar isKindOfClass:olderAutoCalendarClass]);
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCalendar *) __NSCalendarCreate(NSCalendarIdentifier identifier) {
  return [[NSCalendar alloc] initWithCalendarIdentifier:identifier];
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCalendar *) __NSCalendarAutoupdating() {
    return [NSCalendar autoupdatingCurrentCalendar];
}

NS_INLINE NS_RETURNS_RETAINED NS_NON_BRIDGED(NSCalendar *) __NSCalendarCurrent() {
    return [NSCalendar currentCalendar];
}

NS_END_DECLS
