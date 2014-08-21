typedef const void *CFTypeRef;
typedef struct __attribute__((objc_bridge_mutable(NSMutableString))) __CFString *CFMutableStringRef;
typedef struct __attribute__((objc_bridge(NSString))) __CFString const *CFStringRef;
typedef struct __CFTree *CFTreeRef;

typedef signed long CFIndex;
extern CFIndex CFIndex_test;
