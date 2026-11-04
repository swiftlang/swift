#include <CoreFoundation/CoreFoundation.h>

struct CFBase {
  CFMutableArrayRef arr;

  void consume(CF_CONSUMED CFMutableArrayRef a) const { CFRelease(a); }
  CFMutableArrayRef borrow() const CF_RETURNS_NOT_RETAINED { return arr; }
  CFMutableArrayRef copy() const CF_RETURNS_RETAINED {
    return (CFMutableArrayRef)CFRetain(arr);
  }

CF_IMPLICIT_BRIDGING_ENABLED
  CFMutableArrayRef getAudited() const { return arr; }
CF_IMPLICIT_BRIDGING_DISABLED
};

struct CFDerived : CFBase {};
