//===--- NSSwiftString.mm - String support ---------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift String support.
//
//===----------------------------------------------------------------------===//

#import <Foundation/Foundation.h>
#import <pthread.h>
#import <stdlib.h>
#import <objc/objc-internal.h>
#import <objc/runtime.h>
#import <objc/objc.h>

// TODO:
// * hand inline "setClass" an way that isn't OS locked
// * force fragile layout
// * switch off calloc

@interface _SwiftUTF16String : NSString {
  long _refCount;
  NSUInteger count;
  unichar data[1];
}
@end

@implementation _SwiftUTF16String
_OBJC_SUPPORTED_INLINE_REFCNT(_refCount)
- (unichar)characterAtIndex:(NSUInteger)index {
  if (index >= count) __builtin_trap();
  return data[index];
}
- (NSUInteger)length {
  return count;
}
@end

extern "C" _SwiftUTF16String *_swift_allocUTF16Buffer(size_t size) {
  _SwiftUTF16String *string
    = (_SwiftUTF16String *)calloc(1, sizeof(void*) * 3 + size);
  object_setClass(string, [_SwiftUTF16String class]);
  return string;
}

@interface _SwiftASCIIString : NSString {
  long _refCount;
  NSUInteger count;
  unsigned char data[1];
}
@end

@implementation _SwiftASCIIString
_OBJC_SUPPORTED_INLINE_REFCNT(_refCount)
- (unichar)characterAtIndex:(NSUInteger)index {
  if (index >= count) __builtin_trap();
  return data[index];
}
- (NSUInteger)length {
  return count;
}
@end

extern "C" _SwiftASCIIString *_swift_allocASCIIBuffer(size_t size) {
  _SwiftASCIIString *string
    = (_SwiftASCIIString *)calloc(1, sizeof(void*) * 3 + size);
  object_setClass(string, [_SwiftASCIIString class]);
  return string;
}
