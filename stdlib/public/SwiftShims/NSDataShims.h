//===--- NSDataShims.h - Foundation declarations for Data overlay ------===//
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

typedef void (^NSDataDeallocator)(void *, NSUInteger);
FOUNDATION_EXPORT const NSDataDeallocator NSDataDeallocatorVM;
FOUNDATION_EXPORT const NSDataDeallocator NSDataDeallocatorUnmap;
FOUNDATION_EXPORT const NSDataDeallocator NSDataDeallocatorFree;
FOUNDATION_EXPORT const NSDataDeallocator NSDataDeallocatorNone;

NS_END_DECLS
