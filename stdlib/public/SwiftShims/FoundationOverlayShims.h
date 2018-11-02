//===------------------------------------------------------------*- C++ -*-===//
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
#import <objc/runtime.h>
#import <objc/message.h>
#import <sys/fcntl.h>
#import <alloca.h>
#import <stdlib.h>
#import <malloc/malloc.h>
#import <pthread.h>

#import "FoundationShimSupport.h"
#import "NSCalendarShims.h"
#import "NSCharacterSetShims.h"
#import "NSCoderShims.h"
#import "NSDataShims.h"
#import "NSDictionaryShims.h"
#import "NSErrorShims.h"
#import "NSFileManagerShims.h"
#import "NSIndexPathShims.h"
#import "NSIndexSetShims.h"
#import "NSKeyedArchiverShims.h"
#import "NSLocaleShims.h"
#import "NSTimeZoneShims.h"
#import "NSUndoManagerShims.h"

typedef struct {
    void *_Nonnull memory;
    size_t capacity;
    _Bool onStack;
} _ConditionalAllocationBuffer;

static inline _Bool _resizeConditionalAllocationBuffer(_ConditionalAllocationBuffer *_Nonnull buffer, size_t amt) {
    size_t amount = malloc_good_size(amt);
    if (amount <= buffer->capacity) { return true; }
    void *newMemory;
    if (buffer->onStack) {
        newMemory = malloc(amount);
        if (newMemory == NULL) { return false; }
        memcpy(newMemory, buffer->memory, buffer->capacity);
        buffer->onStack = false;
    } else {
        newMemory = realloc(buffer->memory, amount);
        if (newMemory == NULL) { return false; }
    }
    if (newMemory == NULL) { return false; }
    buffer->memory = newMemory;
    buffer->capacity = amount;
    return true;
}

static inline _Bool _withStackOrHeapBuffer(size_t amount, void (__attribute__((noescape)) ^ _Nonnull applier)(_ConditionalAllocationBuffer *_Nonnull)) {
    _ConditionalAllocationBuffer buffer;
    buffer.capacity = malloc_good_size(amount);
    buffer.onStack = (pthread_main_np() != 0 ? buffer.capacity < 2048 : buffer.capacity < 512);
    buffer.memory = buffer.onStack ? alloca(buffer.capacity) : malloc(buffer.capacity);
    if (buffer.memory == NULL) { return false; }
    applier(&buffer);
    if (!buffer.onStack) {
        free(buffer.memory);
    }
    return true;
}
