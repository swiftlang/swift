#ifndef OBJC_OBJC_H_
#define OBJC_OBJC_H_

#define OBJC_ARC_UNAVAILABLE __attribute__((unavailable("not available in automatic reference counting mode")))
#define NS_AUTOMATED_REFCOUNT_UNAVAILABLE OBJC_ARC_UNAVAILABLE

typedef unsigned long NSUInteger;
typedef long NSInteger;
typedef signed char BOOL;

typedef struct objc_selector    *SEL;
SEL sel_registerName(const char *str);

void NSDeallocateObject(id object) NS_AUTOMATED_REFCOUNT_UNAVAILABLE;

#undef NS_AUTOMATED_REFCOUNT_UNAVAILABLE

#endif
