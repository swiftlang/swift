//===--- NSIndexPathShims.h - Found. decl. for IndexPath overl. -*- C++ -*-===//
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

NS_INLINE NS_NON_BRIDGED(NSIndexPath *)_NSIndexPathCreateFromIndexes(NSUInteger idx1, NSUInteger idx2) NS_RETURNS_RETAINED {
    NSUInteger indexes[] = {idx1, idx2};
    return [[NSIndexPath alloc] initWithIndexes:&indexes[0] length:2];
}

NS_END_DECLS
