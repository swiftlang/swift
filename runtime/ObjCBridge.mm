//===--- ObjCBridge.mm - Swift <-> Objective-C Bridging -------------------===//
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
// This implements runtime support for bridging between Swift and Objective-C
// types in cases where they aren't trivial.
//
//===----------------------------------------------------------------------===//

#include <Foundation/Foundation.h>
#include <objc/runtime.h>
#include "Alloc.h"

struct SwiftString;

extern "C" {

int64_t
_TNSs6String4sizefRS_FT_NSs5Int64(void *swiftString);

uint32_t
_TNSs6String11__subscriptFT3idxNSs5Int64_NSs4Charg(uint64_t idx,
                                                   void *swiftString);

void
swift_NSStringToString(NSString *nsstring, SwiftString *string);

NSString *
swift_StringToNSString(SwiftString *string);

}; // extern "C"

struct SwiftString {
  const char *base;
  size_t len;
  SwiftHeapObject *owner;
};

struct _NSSwiftString_s {
  Class isa;
  SwiftString swiftString;
};

@interface _NSSwiftString : NSString {
@public
  SwiftString swiftString;
}
- (unichar)characterAtIndex: (NSUInteger)index;
- (NSUInteger)length;
@end

@implementation _NSSwiftString
- (unichar)characterAtIndex: (NSUInteger)idx {
  static_assert(sizeof(unichar) == 2, "NSString is no longer UTF16?");
  // XXX FIXME
  // Become bug-for-bug compatible with NSString being UTF16.
  // In practice, this API is oblivious to UTF16 surrogate pairs.
  return _TNSs6String11__subscriptFT3idxNSs5Int64_NSs4Charg(idx, &swiftString);
}

- (NSUInteger)length {
  return _TNSs6String4sizefRS_FT_NSs5Int64(&swiftString);
}

// Disable the warning about chaining dealloc to super, we *specifically* don't
// want to do that here.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-missing-super-calls"
- (void)dealloc {
  swift_release(swiftString.owner);
  objc_destructInstance(self); // fixup weak references
  swift_rawDealloc(self, 4);
}
@end
#pragma clang diagnostic pop

// FIXME: This causes static constructors, which isn't awesome.  It would be
// spiffier to use ifunc's if possible.
static const Class stringClasses[] = {
  [_NSSwiftString self],
  objc_lookUpClass("__NSCFConstantString"),
  objc_lookUpClass("__NSCFString"),
};

__attribute__((noinline,used))
static void
_swift_NSStringToString_slow(NSString *nsstring, SwiftString *string) {
  // XXX FIXME -- NSString is oblivious to surrogate pairs
  string->owner = 0;

  if (*(Class *)nsstring == stringClasses[2]) {
    // XXX FIXME -- leaking and we need to sort out intermediate boxing
    string->base  = [nsstring UTF8String];
    string->len   = [nsstring length];
    if (string->base) {
      string->base = strdup(string->base);
    }
  }

  if (string->base) {
    return;
  }

  __builtin_trap();

  size_t len = [nsstring length];
  size_t bufSize = len * 2 + 1;
  char *buf = static_cast<char *>(malloc(bufSize));
  assert(buf);
  if (![nsstring getCString: buf maxLength: bufSize
                   encoding: NSUTF8StringEncoding]) {
    __builtin_trap();
  }
  string->base  = buf;
  string->len   = len;
}

void
swift_NSStringToString(NSString *nsstring, SwiftString *string) {
  auto boxedString = reinterpret_cast<_NSSwiftString_s *>(nsstring);
  if (boxedString->isa == stringClasses[0]) {
    string->base  = boxedString->swiftString.base;
    string->len   = boxedString->swiftString.len;
    string->owner = boxedString->swiftString.owner;
    _swift_retain(string->owner);
    return;
  } else if (*(Class *)nsstring == stringClasses[1]) {
    // constant string
    string->base  = ((char **)nsstring)[2];
    string->len   = ((size_t *)nsstring)[3];
    string->owner = NULL;
    return;
  }
  _swift_NSStringToString_slow(nsstring, string);
}


NSString *
swift_StringToNSString(SwiftString *string) {
  // sizeof(_NSSwiftString) is not allowed
  auto r = static_cast<_NSSwiftString *>(swift_rawAlloc(4));
  *((Class *)r) = stringClasses[0];
  r->swiftString = *string;
  _swift_retain(r->swiftString.owner);
  return r;
}
