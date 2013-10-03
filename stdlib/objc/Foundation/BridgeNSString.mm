//===--- BridgeNSString.mm - String <-> NSString Bridging -----------------===//
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
#include "swift/Runtime/Alloc.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/ObjCBridge.h"

using namespace swift;

struct SwiftString;

// String.size()
extern "C" int64_t
String_size(SwiftString *swiftString) 
  asm("__TSS4sizefRSSFT_Si");

// String[] getter
extern "C" uint32_t
String_getSubscript(uint64_t idx, SwiftString *swiftString) 
  asm("__TSS9subscriptFT3idxSi_Scg");


struct SwiftString {
  const char *base;
  size_t len;
  HeapObject *owner;
};

struct _NSSwiftString_s {
  Class isa  __attribute__((unavailable));
  SwiftString swiftString;
};

__attribute__((visibility("hidden")))
@interface _NSSwiftString : NSString 
- (unichar)characterAtIndex: (NSUInteger)index;
- (NSUInteger)length;
@end

@implementation _NSSwiftString {
  SwiftString swiftString;
}

- (unichar)characterAtIndex: (NSUInteger)idx {
  static_assert(sizeof(unichar) == 2, "NSString is no longer UTF16?");
  // XXX FIXME
  // Become bug-for-bug compatible with NSString being UTF16.
  // In practice, this API is oblivious to UTF16 surrogate pairs.
  return String_getSubscript(idx, &swiftString);
}

- (NSUInteger)length {
  return String_size(&swiftString);
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
#pragma clang diagnostic pop

// FIXME: This causes static constructors, which isn't awesome.  It would be
// spiffier to use ifunc's if possible.
static Class NSSwiftStringClass = [_NSSwiftString class];
static Class NSCFConstantStringClass = objc_getClass("__NSCFConstantString");

/// Convert an NSString to a Swift String in the worst case, where we have to
/// use -[NSString getBytes:...:] to reencode the string value.
__attribute__((noinline,used))
static void
_swift_NSStringToString_slow(NSString *nsstring, SwiftString *string) {
  // FIXME: Measure trade-off of memory overhead vs two-pass encoding 
  // 1 UTF-16 unit becomes at most 3 bytes of UTF-8, plus the null terminator.
  NSUInteger utf16Length = [nsstring length];
  assert(utf16Length < NSIntegerMax / 3  &&  "NSString too long");
  NSUInteger bufSize = utf16Length * 3 + 1;
  
  // Allocate a POD heap object to hold the data.
  BoxPair box = swift_allocPOD(bufSize, alignof(void*) - 1);
  char *buf = reinterpret_cast<char *>(box.value);

  NSUInteger utf8Length = 0;
  if (utf16Length != 0) {
    // Copy the encoded string to our buffer.
    NSRange unencodedRange;
    BOOL ok = [nsstring getBytes:buf 
                       maxLength:bufSize - 1
                      usedLength:&utf8Length
                        encoding:NSUTF8StringEncoding
                         options:0
                           range:NSMakeRange(0, utf16Length)
                  remainingRange:&unencodedRange];

    // The operation should have encoded the entire string.
    (void)ok;
    assert(ok  &&  "NSString encoding failed");
    assert(unencodedRange.length == 0  &&  "NSString encoding failed");
  } 

  buf[utf8Length] = '\0';

  string->base  = buf;
  string->len   = utf8Length;
  string->owner = box.heapObject;
}

extern "C" void
swift_NSStringToString(NSString *nsstring, SwiftString *string) {
  Class cls = object_getClass(nsstring);
  if (cls == NSSwiftStringClass) {
    // Swift boxed string
    auto boxedString = reinterpret_cast<_NSSwiftString_s *>(nsstring);
    string->base  = boxedString->swiftString.base;
    string->len   = boxedString->swiftString.len;
    string->owner = boxedString->swiftString.owner;
    _swift_retain(string->owner);
  } else if (cls == NSCFConstantStringClass) {
    // Constant string
    string->base  = ((char **)nsstring)[2];
    string->len   = ((size_t *)nsstring)[3];
    string->owner = NULL;
  } else {
    // String that needs to be copied or re-encoded.
    _swift_NSStringToString_slow(nsstring, string);
  } 
}

extern "C" NSString *
swift_StringToNSString(SwiftString *string) {
  // sizeof(_NSSwiftString) is not allowed
  auto r = static_cast<_NSSwiftString *>(swift_rawAlloc(4));
  r->swiftString = *string;
  _swift_retain(r->swiftString.owner);
  return objc_constructInstance(NSSwiftStringClass, r);
}

/// (String, UnsafePointer<BOOL>) -> () block shim
using block_type = void (^)(id, BOOL*);
using code_type = void (*)(const char*, size_t, HeapObject*, BOOL*, HeapObject*);

extern "C"
block_type _TTbbTSSGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_(
                                                    code_type code,
                                                    HeapObject *data) {
  SwiftRAII dataRAII(data, true);
  return Block_copy(^void (id a, BOOL *b) {
    [a retain];
    SwiftString s;
    swift_NSStringToString((NSString*)a, &s);
    return code(s.base, s.len, s.owner, b, swift_retain(*dataRAII));
  });
}

@end
