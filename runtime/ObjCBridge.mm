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


@interface _NSSwiftString : NSString
{
@public
  struct SwiftString {
    const char *base;
    size_t len;
    SwiftHeapObject *owner;
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
  return _TNSs6String11__subscriptFT3idxNSs5Int64_NSs4Charg(idx, &swiftString);
}
- (NSUInteger)length
{
  return _TNSs6String4sizefRS_FT_NSs5Int64(&swiftString);
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-missing-super-calls"
- (void)dealloc
{
  swift_release(swiftString.owner);
  objc_destructInstance(self); // fixup weak references
  swift_rawDealloc(self, 4);
}
@end
#pragma clang diagnostic pop

static Class stringClasses[] = {
  [_NSSwiftString self],
  objc_lookUpClass("__NSCFConstantString"),
  objc_lookUpClass("__NSCFString"),
};

void
swift_NSStringToString(NSString *nsstring, SwiftString *string)
{
  // XXX FIXME -- NSString is oblivious to surrogate pairs

  string->owner = 0;

  if (*(Class *)nsstring == stringClasses[0]) {
    auto boxedString = static_cast<_NSSwiftString *>(nsstring);
    *string = boxedString->swiftString;
    swift_retain(string->owner);
  } else if (*(Class *)nsstring == stringClasses[1]) {
    // constant string
    string->base  = [nsstring UTF8String];
    string->len   = [nsstring length];
  } else if (*(Class *)nsstring == stringClasses[2]) {
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

NSString *
swift_StringToNSString(SwiftString *string)
{
  // sizeof(_NSSwiftString) is not allowed
  auto r = static_cast<_NSSwiftString *>(swift_rawAlloc(4));
  *((Class *)r) = stringClasses[0];
  r->swiftString = *string;
  swift_retain(r->swiftString.owner);
  return r;
}
