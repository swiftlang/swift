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
  size_t i;
  if (*(Class *)boxedString == stringClasses[0]) {
    *swiftString = boxedString->swiftString;
    swift_retain(reinterpret_cast<struct SwiftHeapObject *>(swiftString->magic));
    return;
  }
  // XXX FIXME -- handle known classes and copy the rest
#if 0
  for (i = 1; i < (sizeof(stringClasses) / sizeof(stringClasses[0])); i++) {
    if (*c != stringClasses[i]) {
      continue;
    }
    return;
  }
#endif
  __builtin_trap();
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
