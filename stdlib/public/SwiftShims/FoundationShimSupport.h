//===--- FoundationShimSupport.h - Helper macros for Foundation overlay ------===//
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

#ifndef NS_NON_BRIDGED
#define NS_NON_BRIDGED(type) NSObject *
#endif

#ifndef NS_BEGIN_DECLS

#define NS_BEGIN_DECLS \
    __BEGIN_DECLS \
    NS_ASSUME_NONNULL_BEGIN

#endif

#ifndef NS_END_DECLS

#define NS_END_DECLS \
    NS_ASSUME_NONNULL_END \
    __END_DECLS

#endif
