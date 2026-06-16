#ifndef SWIFT_GNUSTEP_COMPAT_COREFOUNDATION_CFBASE_H
#define SWIFT_GNUSTEP_COMPAT_COREFOUNDATION_CFBASE_H

#if defined(__LP64__) || defined(_LP64)
typedef signed long CFIndex;
typedef unsigned long CFHashCode;
#else
typedef signed int CFIndex;
typedef unsigned int CFHashCode;
#endif

typedef unsigned long CFTypeID;
typedef const void *CFTypeRef;

#endif
