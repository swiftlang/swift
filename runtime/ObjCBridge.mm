#include <Foundation/Foundation.h>
#include <objc/runtime.h>
#include "Alloc.h"

extern "C" {

int64_t
_TNSs6String4sizefRS_FT_NSs5Int64(void *swiftString);

uint32_t
_TNSs6String11__subscriptFT3idxNSs5Int64_NSs4Charg(void *swiftString,
                                                   uint64_t idx);

void
swift_NSStringToString(void *object, void *string);

void *
swift_StringToNSString(void *string);

}; // extern "C"

@interface _NSSwiftString : NSString
{
@public
  struct SwiftString {
    uintptr_t magic[3];
  } swiftString;
}
- (unichar)characterAtIndex: (NSUInteger)index;
- (NSUInteger)length;
@end

@implementation _NSSwiftString
- (unichar)characterAtIndex: (NSUInteger)idx
{
  static_assert(sizeof(unichar) == 2, "NSString is no longer UTF16?");
  // XXX FIXME
  // Become bug-for-bug compatible with NSString being UTF16.
  // In practice, this API is oblivious to UTF16 surrogate pairs.
  return _TNSs6String11__subscriptFT3idxNSs5Int64_NSs4Charg(&swiftString, idx);
}
- (NSUInteger)length
{
  return _TNSs6String4sizefRS_FT_NSs5Int64(&swiftString);
}
- (void)dealloc
{
  swift_release(reinterpret_cast<struct SwiftHeapObject *>(swiftString.magic));
  objc_destructInstance(self); // fixup weak references
  swift_rawDealloc(self, 4);
}
@end

static Class stringClasses[] = {
  [_NSSwiftString self],
  objc_lookUpClass("__NSCFConstantString"),
  objc_lookUpClass("__NSCFString"),
};

void
swift_NSStringToString(void *object, void *string)
{
  auto boxedString = static_cast<_NSSwiftString *>(object);
  auto swiftString = static_cast<SwiftString *>(string);
  
  if (*(Class *)boxedString == stringClasses[0]) {
    *swiftString = boxedString->swiftString;
    swift_retain(reinterpret_cast<struct SwiftHeapObject *>(swiftString->magic));
  } else if (*(Class *)boxedString == stringClasses[1]) {
    // constant string
    // XXX FIXME -- detect and deal with non-ASCII/UTF8 pains
    swiftString->magic[0] = 0;
    swiftString->magic[1] = reinterpret_cast<uintptr_t>(
                              [(NSString *)object UTF8String]);
    swiftString->magic[2] = [(NSString *)object length];
  } else {
    // XXX FIXME -- leaking and we need to sort out intermediate boxing
    swiftString->magic[0] = 0;
    swiftString->magic[1] = reinterpret_cast<uintptr_t>(
                              [(NSString *)object UTF8String]);
    swiftString->magic[2] = [(NSString *)object length];
  }
}

void *
swift_StringToNSString(void *string)
{
  // sizeof(_NSSwiftString) is not allowed
  auto r = static_cast<_NSSwiftString *>(swift_rawAlloc(4));
  *((Class *)r) = stringClasses[0];
  r->swiftString = *static_cast<SwiftString *>(string);
  swift_retain(reinterpret_cast<struct SwiftHeapObject *>(r->swiftString.magic));
  return r;
}
