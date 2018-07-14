#if !defined(__COREFOUNDATION_COREFOUNDATION__)
#define __COREFOUNDATION_COREFOUNDATION__ 1
#define __COREFOUNDATION__ 1

typedef const struct __CFAllocator * CFAllocatorRef;
extern const CFAllocatorRef kCFAllocatorDefault;


typedef const void *CFTypeRef;
typedef struct __attribute__((objc_bridge_mutable(NSMutableString))) __CFString *CFMutableStringRef;
typedef struct __attribute__((objc_bridge(NSString))) __CFString const *CFStringRef;
typedef struct __CFTree *CFTreeRef;
typedef const struct __attribute__((objc_bridge(CFURL))) __CFURL * CFURLRef;

typedef struct __attribute__((objc_bridge(NSDictionary))) __CFDictionary const *CFDictionaryRef;
typedef struct __attribute__((objc_bridge(NSArray))) __CFArray const *CFArrayRef;
typedef struct __attribute__((objc_bridge(NSSet))) __CFSet const *CFSetRef;

typedef CFTypeRef CFAliasForTypeRef;


typedef signed long CFIndex;
extern CFIndex CFIndex_test;

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type

#define CF_NOESCAPE __attribute__((noescape))

#endif
