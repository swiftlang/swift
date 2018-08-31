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
#import <CoreGraphics/CoreGraphics.h>

#include <swift/Runtime/Debug.h>
#include <cstdint>
#include "swift/Basic/Lazy.h"

// This file assumes that `sizeof(NSInteger) == Swift.Int` and
// `sizeof(NSUInteger) == sizeof(Swift.UInt)`.

@interface _SwiftTypePreservingNSNumber : NSNumber
@end

// This enum has a matching counterpart in Foundation.swift, please
// update both copies when changing it.
enum _SwiftTypePreservingNSNumberTag: uint8_t {
  SwiftInt     =  0,
  SwiftInt64   =  1,
  SwiftInt32   =  2,
  SwiftInt16   =  3,
  SwiftInt8    =  4,
  SwiftUInt    =  5,
  SwiftUInt64  =  6,
  SwiftUInt32  =  7,
  SwiftUInt16  =  8,
  SwiftUInt8   =  9,
  SwiftFloat   = 10,
  SwiftDouble  = 11,
  SwiftCGFloat = 12,
  SwiftBool    = 13,
  /// A non-type-preserving NSNumber from Cocoa.
  ///
  /// Note that this value is *not* in the corresponding Swift enum in
  /// Foundation.swift. It should never be used as the tag of a valid
  /// _SwiftTypePreservingNSNumber instance; it is used as a return value
  /// for _swift_Foundation_TypePreservingNSNumberGetKind to indicate that
  /// an NSNumber instance does not know its originating Swift type.
  ///
  /// The numbering scheme here, with `NonSwift` coming after the valid values
  /// for the enum, matches Swift's tag assignment scheme for single-payload
  /// enums. For an Optional<_SwiftTypePreservingNSNumberTag>, Swift uses the
  /// first invalid integer value to represent `.none`. This representation on
  /// the C side allows _SwiftTypePreservingNSNumberTag(rawValue:) on the Swift
  /// side to compile down to a no-op.
  NonSwift     = 14,
};

static const unsigned StorageSize = 8;

@implementation _SwiftTypePreservingNSNumber {
  char storage[StorageSize];
  _SwiftTypePreservingNSNumberTag tag;
}

- (id)init {
  self = [super init];
  if (self) {
    self->tag = SwiftInt;
    memset(self->storage, 0, StorageSize);
  }
  return self;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wobjc-designated-initializers"
- (id)initWithCoder:(NSCoder *)coder {
  swift::swift_reportError(
      /* flags = */ 0,
      "_SwiftTypePreservingNSNumber should not be archived.\n");
  abort();
}
#pragma GCC diagnostic pop

- (id)copyWithZone:(NSZone *)zone {
  return [self retain];
}

// When returning the `objCType` encoding, use the traditional C names,
// because these are what are documented as corresponding to specific
// encode strings, and `int` vs `long` or `long` vs `long long` may have
// different encodings. Ensure the traditional C types have the sizes we
// expect, though.
static_assert(sizeof(long long) == 8, "long long should be 64 bit");
static_assert(sizeof(int) == 4, "int should be 32 bit");
static_assert(sizeof(short) == 2, "short should be 16 bit");

- (const char *)objCType {
  // When changing this method, make sure to keep the `getValue:` method in
  // sync (it should copy as many bytes as this property promises).
  switch(tag) {
  case SwiftInt:
    return @encode(NSInteger);
  case SwiftInt64:
    return @encode(long long);
  case SwiftInt32:
    return @encode(int);
  case SwiftInt16:
    return @encode(short);
  case SwiftInt8:
    return @encode(signed char);
  case SwiftUInt:
    return @encode(NSUInteger);
  case SwiftUInt64:
    return @encode(unsigned long long);
  case SwiftUInt32:
    return @encode(unsigned);
  case SwiftUInt16:
    return @encode(unsigned short);
  case SwiftUInt8:
    return @encode(unsigned char);
  case SwiftFloat:
    return @encode(float);
  case SwiftDouble:
    return @encode(double);
  case SwiftCGFloat:
    return @encode(CGFloat);
  case NonSwift:
  case SwiftBool:
    // Bool is represented by CFBoolean.
    break;
  }
  swift::swift_reportError(
      /* flags = */ 0,
      "_SwiftTypePreservingNSNumber.tag is corrupted.\n");
  abort();
}

- (void)getValue:(void *)value {
  // This method should copy as many bytes as the `objCType` property promises.
  switch(tag) {
  case SwiftInt:
    memcpy(value, self->storage, sizeof(NSInteger));
    return;
  case SwiftInt64:
    memcpy(value, self->storage, sizeof(int64_t));
    return;
  case SwiftInt32:
    memcpy(value, self->storage, sizeof(int32_t));
    return;
  case SwiftInt16:
    memcpy(value, self->storage, sizeof(int16_t));
    return;
  case SwiftInt8:
    memcpy(value, self->storage, sizeof(int8_t));
    return;
  case SwiftUInt:
    memcpy(value, self->storage, sizeof(NSUInteger));
    return;
  case SwiftUInt64:
    memcpy(value, self->storage, sizeof(uint64_t));
    return;
  case SwiftUInt32:
    memcpy(value, self->storage, sizeof(uint32_t));
    return;
  case SwiftUInt16:
    memcpy(value, self->storage, sizeof(uint16_t));
    return;
  case SwiftUInt8:
    memcpy(value, self->storage, sizeof(uint8_t));
    return;
  case SwiftFloat:
    memcpy(value, self->storage, sizeof(float));
    return;
  case SwiftDouble:
    memcpy(value, self->storage, sizeof(double));
    return;
  case SwiftCGFloat:
    memcpy(value, self->storage, sizeof(CGFloat));
    return;
  case NonSwift:
  case SwiftBool:
    // Bool is represented by CFBoolean.
    break;
  }
  swift::swift_reportError(
      /* flags = */ 0,
      "_SwiftTypePreservingNSNumber.tag is corrupted.\n");
  abort();
}

#define DEFINE_ACCESSOR(C_TYPE, METHOD_NAME) \
  - (C_TYPE)METHOD_NAME { \
    switch(tag) { \
    case SwiftInt: { \
      NSInteger result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftInt64: { \
      int64_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftInt32: { \
      int32_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftInt16: { \
      int16_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftInt8: { \
      int8_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftUInt: { \
      NSUInteger result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftUInt64: { \
      uint64_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftUInt32: { \
      uint32_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftUInt16: { \
      uint16_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftUInt8: { \
      uint8_t result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftFloat: { \
      float result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftDouble: { \
      double result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftCGFloat: { \
      CGFloat result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case NonSwift: \
    case SwiftBool: { \
      /* Bool is represented by CFBoolean. */ \
      break; \
    } \
    } \
    swift::swift_reportError( \
        /* flags = */ 0, \
        "_SwiftTypePreservingNSNumber.tag is corrupted.\n"); \
    abort(); \
  }

DEFINE_ACCESSOR(char, charValue)
DEFINE_ACCESSOR(unsigned char, unsignedCharValue)
DEFINE_ACCESSOR(short, shortValue)
DEFINE_ACCESSOR(unsigned short, unsignedShortValue)
DEFINE_ACCESSOR(int, intValue)
DEFINE_ACCESSOR(unsigned int, unsignedIntValue)
DEFINE_ACCESSOR(long long, longLongValue)
DEFINE_ACCESSOR(unsigned long long, unsignedLongLongValue)
DEFINE_ACCESSOR(float, floatValue)
DEFINE_ACCESSOR(double, doubleValue)
DEFINE_ACCESSOR(NSInteger, integerValue)
DEFINE_ACCESSOR(NSUInteger, unsignedIntegerValue)

#undef DEFINE_ACCESSOR

- (Class)classForCoder {
  return [NSNumber class];
}

#define DEFINE_INIT(C_TYPE, FUNCTION_NAME) \
  SWIFT_CC(swift) extern "C" NS_RETURNS_RETAINED NSNumber * \
  _swift_Foundation_TypePreservingNSNumberWith ## FUNCTION_NAME(C_TYPE value) { \
    _SwiftTypePreservingNSNumber *result = \
      [[_SwiftTypePreservingNSNumber alloc] init]; \
    result->tag = Swift ## FUNCTION_NAME; \
    memcpy(result->storage, &value, sizeof(value)); \
    return result; \
  }

DEFINE_INIT(NSInteger, Int)
DEFINE_INIT(int64_t, Int64)
DEFINE_INIT(int32_t, Int32)
DEFINE_INIT(int16_t, Int16)
DEFINE_INIT(int8_t,  Int8)
DEFINE_INIT(NSUInteger, UInt)
DEFINE_INIT(uint64_t, UInt64)
DEFINE_INIT(uint32_t, UInt32)
DEFINE_INIT(uint16_t, UInt16)
DEFINE_INIT(uint8_t,  UInt8)
DEFINE_INIT(float, Float)
DEFINE_INIT(double, Double)
DEFINE_INIT(CGFloat, CGFloat)

#undef DEFINE_INIT

SWIFT_CC(swift) extern "C" uint8_t
_swift_Foundation_TypePreservingNSNumberGetKind(
  NSNumber *_Nonnull self_) {
  uint8_t result = NonSwift;
  if ([self_ isKindOfClass:
                   SWIFT_LAZY_CONSTANT([_SwiftTypePreservingNSNumber class])]) {
    result = ((_SwiftTypePreservingNSNumber *) self_)->tag;
  } else if (CFGetTypeID(self_) == CFBooleanGetTypeID()) {
    result = SwiftBool;
  }
  return result;
}

#define DEFINE_GETTER(C_TYPE, FUNCTION_NAME) \
  SWIFT_CC(swift) extern "C" C_TYPE \
  _swift_Foundation_TypePreservingNSNumberGetAs ## FUNCTION_NAME( \
      _SwiftTypePreservingNSNumber *_Nonnull self_) { \
    if (self_->tag != Swift ## FUNCTION_NAME) { \
      swift::swift_reportError( \
        /* flags = */ 0, "Incorrect tag.\n"); \
    } \
    C_TYPE result; \
    memcpy(&result, self_->storage, sizeof(result)); \
    return result; \
  }

DEFINE_GETTER(NSInteger, Int)
DEFINE_GETTER(int64_t, Int64)
DEFINE_GETTER(int32_t, Int32)
DEFINE_GETTER(int16_t, Int16)
DEFINE_GETTER(int8_t,  Int8)
DEFINE_GETTER(NSUInteger, UInt)
DEFINE_GETTER(uint64_t, UInt64)
DEFINE_GETTER(uint32_t, UInt32)
DEFINE_GETTER(uint16_t, UInt16)
DEFINE_GETTER(uint8_t,  UInt8)
DEFINE_GETTER(float, Float)
DEFINE_GETTER(double, Double)
DEFINE_GETTER(CGFloat, CGFloat)

#undef DEFINE_GETTER

@end

