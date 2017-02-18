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
#include "swift/Runtime/Config.h"

@interface NSIndexSet (NSRanges)
- (NSUInteger)rangeCount;
- (NSRange)rangeAtIndex:(NSUInteger)rangeIndex;
- (NSUInteger)_indexOfRangeContainingIndex:(NSUInteger)value;
@end

extern NSUInteger __NSIndexSetRangeCount(id NS_RELEASES_ARGUMENT __nonnull self_) {
    NSIndexSet *indexSet = self_;
    NSUInteger result = [indexSet rangeCount];
    [indexSet release];
    return result;
}

SWIFT_CC(swift)
extern void __NSIndexSetRangeAtIndex(id NS_RELEASES_ARGUMENT __nonnull self_, NSUInteger rangeIndex, NSUInteger * __nonnull location, NSUInteger * __nonnull length) {
    NSIndexSet *indexSet = self_;
    NSRange result = [indexSet rangeAtIndex:rangeIndex];
    [indexSet release];
    *location = result.location;
    *length = result.length;
}

SWIFT_CC(swift)
extern NSUInteger __NSIndexSetIndexOfRangeContainingIndex(id NS_RELEASES_ARGUMENT __nonnull self_, NSUInteger index) {
    NSIndexSet *indexSet = self_;
    NSUInteger result = [indexSet _indexOfRangeContainingIndex:index];
    [indexSet release];
    return result;
}



