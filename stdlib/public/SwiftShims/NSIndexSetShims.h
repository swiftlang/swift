//===--- NSIndexSetShims.h - Foundation declarations for IndexSet overlay -===//
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

@interface NSIndexSet (NSRanges)
- (NSUInteger)rangeCount;
- (NSRange)rangeAtIndex:(NSUInteger)rangeIndex;
- (NSUInteger)_indexOfRangeContainingIndex:(NSUInteger)value;
@end

NS_INLINE NSUInteger __NSIndexSetRangeCount(NS_NON_BRIDGED(NSIndexSet *)self_) {
    return [(NSIndexSet *)self_ rangeCount];
}

NS_INLINE void __NSIndexSetRangeAtIndex(NS_NON_BRIDGED(NSIndexSet *)self_, NSUInteger rangeIndex, NSUInteger *location, NSUInteger *length) {
    NSRange result = [(NSIndexSet *)self_ rangeAtIndex:rangeIndex];
    *location = result.location;
    *length = result.length;
}

NS_INLINE NSUInteger __NSIndexSetIndexOfRangeContainingIndex(NS_NON_BRIDGED(NSIndexSet *)self_, NSUInteger index) {
    return [(NSIndexSet *)self_ _indexOfRangeContainingIndex:index];
}

NS_END_DECLS
