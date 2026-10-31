#pragma once

#include <CoreFoundation/CoreFoundation.h>

struct CFHolder {
  CFStringRef str;

  CFStringRef copyString() const CF_RETURNS_RETAINED {
    return (CFStringRef)CFRetain(str);
  }
  CFStringRef getString() const { return str; }
  CFStringRef getStringNotRetained() const CF_RETURNS_NOT_RETAINED {
    return str;
  }

  static CFStringRef staticCopyString(CFStringRef s) CF_RETURNS_RETAINED {
    return (CFStringRef)CFRetain(s);
  }
  static CFStringRef staticGetString(CFStringRef s) { return s; }
};

inline CFStringRef freeCopyString(CFStringRef s) CF_RETURNS_RETAINED {
  return (CFStringRef)CFRetain(s);
}
