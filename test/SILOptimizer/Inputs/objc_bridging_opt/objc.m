#include "objc.h"

#include <stdio.h>

void useNSString(NSString * _Nonnull s) {
  const char *str = [s cStringUsingEncoding: NSUTF8StringEncoding];
  if (str[0] == '\0') {
    puts("<empty>");
  } else {
    puts(str);
  }
}

void useOptNSString(NSString * _Nullable s) {
  if (s) {
    const char *str = [s cStringUsingEncoding: NSUTF8StringEncoding];
    puts(str);
  } else {
    puts("NULL");
  }
}

NSString * _Nonnull returnNSString() {
  NSString *str = [[NSString alloc] initWithCString: "This is an ObjectiveC string!" encoding: NSUTF8StringEncoding];
  return str;
}

NSString *g;

NSString * _Nonnull returnNullNSString() {
  return g;
}

NSString * _Nullable returnOptNSString(BOOL some) {
  if (some) {
    NSString *str = [[NSString alloc] initWithCString: "This is an optional ObjectiveC string!" encoding: NSUTF8StringEncoding];
    return str;
  }

  return NULL;
}


