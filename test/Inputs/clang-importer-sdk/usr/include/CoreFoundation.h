#if !defined(__COREFOUNDATION_COREFOUNDATION__)
#define __COREFOUNDATION_COREFOUNDATION__ 1
#define __COREFOUNDATION__ 1

#include <MacTypes.h>

typedef const struct __CFAllocator * CFAllocatorRef;
const CFAllocatorRef kCFAllocatorDefault;


typedef const void *CFTypeRef;
typedef struct __attribute__((objc_bridge_mutable(NSMutableString))) __CFString *CFMutableStringRef;
typedef struct __attribute__((objc_bridge(NSString))) __CFString const *CFStringRef;
typedef struct __CFTree *CFTreeRef;
typedef const struct __attribute__((objc_bridge(CFURL))) __CFURL * CFURLRef;


typedef signed long CFIndex;
extern CFIndex CFIndex_test;

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type

#endif
