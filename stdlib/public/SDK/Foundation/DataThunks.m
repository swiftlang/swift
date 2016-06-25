//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#import <Foundation/Foundation.h>

typedef void (^NSDataDeallocator)(void *, NSUInteger);
extern const NSDataDeallocator NSDataDeallocatorVM;
extern const NSDataDeallocator NSDataDeallocatorUnmap;
extern const NSDataDeallocator NSDataDeallocatorFree;
extern const NSDataDeallocator NSDataDeallocatorNone;

void __NSDataInvokeDeallocatorVM(void *mem, NSUInteger length) {
  NSDataDeallocatorVM(mem, length);
}

void __NSDataInvokeDeallocatorUnmap(void *mem, NSUInteger length) {
  NSDataDeallocatorUnmap(mem, length);
}

void __NSDataInvokeDeallocatorFree(void *mem, NSUInteger length) {
  NSDataDeallocatorFree(mem, length);
}
