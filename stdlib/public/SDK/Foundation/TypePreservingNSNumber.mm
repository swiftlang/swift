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
#import <CoreGraphics/CoreGraphics.h>

#include <swift/Runtime/Debug.h>

// This file assumes that `sizeof(NSInteger) == Swift.Int` and
// `sizeof(NSUInteger) == sizeof(Swift.UInt)`.

@interface _SwiftTypePreservingNSNumber : NSNumber
@end

// This enum has a matching counterpart in Foundation.swift, please
// update both copies when changing it.
enum _SwiftTypePreservingNSNumberTag {
  SwiftInt = 1,
  SwiftUInt = 2,
  SwiftFloat = 3,
  SwiftDouble = 4,
  SwiftCGFloat = 5,
  SwiftBool = 6
};

@implementation _SwiftTypePreservingNSNumber {
  _SwiftTypePreservingNSNumberTag tag;
  char storage[8];
}

- (id)init {
  self = [super init];
  if (self) {
    self->tag = SwiftInt;
    const int64_t value = 0;
    memcpy(self->storage, &value, 8);
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

- (const char *)objCType {
  // When changing this method, make sure to keep the `getValue:` method in
  // sync (it should copy as many bytes as this property promises).
  switch(tag) {
  case SwiftInt:
    return @encode(NSInteger);
  case SwiftUInt:
    return @encode(NSUInteger);
  case SwiftFloat:
    return @encode(float);
  case SwiftDouble:
    return @encode(double);
  case SwiftCGFloat:
    return @encode(CGFloat);
  case SwiftBool:
    return @encode(bool);
  }
  swift::swift_reportError(
      /* flags = */ 0,
      "_SwiftTypePreservingNSNumber.tag is corrupted.\n");
}

- (void)getValue:(void *)value {
  // This method should copy as many bytes as the `objCType` property promises.
  switch(tag) {
  case SwiftInt:
    memcpy(value, self->storage, sizeof(NSInteger));
    return;
  case SwiftUInt:
    memcpy(value, self->storage, sizeof(NSUInteger));
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
  case SwiftBool:
    memcpy(value, self->storage, sizeof(bool));
    return;
  }
  swift::swift_reportError(
      /* flags = */ 0,
      "_SwiftTypePreservingNSNumber.tag is corrupted.\n");
}

#define DEFINE_ACCESSOR(C_TYPE, METHOD_NAME) \
  - (C_TYPE)METHOD_NAME { \
    switch(tag) { \
    case SwiftInt: { \
      NSInteger result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    case SwiftUInt: { \
      NSUInteger result; \
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
    case SwiftBool: { \
      bool result; \
      memcpy(&result, self->storage, sizeof(result)); \
      return result; \
    } \
    } \
    swift::swift_reportError( \
        /* flags = */ 0, \
        "_SwiftTypePreservingNSNumber.tag is corrupted.\n"); \
  }

DEFINE_ACCESSOR(char, charValue)
DEFINE_ACCESSOR(int, intValue)
DEFINE_ACCESSOR(unsigned int, unsignedIntValue)
DEFINE_ACCESSOR(long long, longLongValue)
DEFINE_ACCESSOR(unsigned long long, unsignedLongLongValue)
DEFINE_ACCESSOR(float, floatValue)
DEFINE_ACCESSOR(double, doubleValue)

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
DEFINE_INIT(NSUInteger, UInt)
DEFINE_INIT(float, Float)
DEFINE_INIT(double, Double)
DEFINE_INIT(CGFloat, CGFloat)
DEFINE_INIT(bool, Bool)

#undef DEFINE_INIT

SWIFT_CC(swift) extern "C" uint32_t
_swift_Foundation_TypePreservingNSNumberGetKind(
  NSNumber *NS_RELEASES_ARGUMENT _Nonnull self_) {
  uint32_t result = 0;
  if ([self_ isKindOfClass: [_SwiftTypePreservingNSNumber class]]) {
    result = ((_SwiftTypePreservingNSNumber *) self_)->tag;
  }
  [self_ release];
  return result;
}

#define DEFINE_GETTER(C_TYPE, FUNCTION_NAME) \
  SWIFT_CC(swift) extern "C" C_TYPE \
  _swift_Foundation_TypePreservingNSNumberGetAs ## FUNCTION_NAME( \
      _SwiftTypePreservingNSNumber *NS_RELEASES_ARGUMENT _Nonnull self_) { \
    if (self_->tag != Swift ## FUNCTION_NAME) { \
      swift::swift_reportError( \
        /* flags = */ 0, "Incorrect tag.\n"); \
    } \
    C_TYPE result; \
    memcpy(&result, self_->storage, sizeof(result)); \
    [self_ release]; \
    return result; \
  }

DEFINE_GETTER(NSInteger, Int)
DEFINE_GETTER(NSUInteger, UInt)
DEFINE_GETTER(float, Float)
DEFINE_GETTER(double, Double)
DEFINE_GETTER(CGFloat, CGFloat)
DEFINE_GETTER(bool, Bool)

#undef DEFINE_GETTER

@end

