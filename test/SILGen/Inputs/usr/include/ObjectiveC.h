typedef __typeof__(__objc_yes) BOOL;
typedef long NSInteger;

@class NSString;

typedef struct _NSZone NSZone;

@protocol NSCopying
- (_Null_unspecified id) copyWithZone: (nullable NSZone*)zone;
@end

_Null_unspecified SEL sel_registerName(const char * _Null_unspecified);

@interface NSObject
+ (NSObject* _Null_unspecified) alloc;
- (NSObject* _Null_unspecified) init;
+ (NSObject* _Null_unspecified) new;
+ (void) load;
+ (void) initialize;

- (_Null_unspecified id)performSelector:(_Null_unspecified SEL)selector withObject:(_Null_unspecified id)object;

@property (readonly, copy) NSString * _Null_unspecified description;

@property NSString * _Null_unspecified stringProperty;

@property Class _Null_unspecified classProp;
@end

#define NS_RETURNS_INNER_POINTER __attribute__((objc_returns_inner_pointer))
#define NS_RETURNS_RETAINED __attribute__((ns_returns_retained))
#define NS_CONSUMES_SELF __attribute__((ns_consumes_self))
#define NS_CONSUMED __attribute__((ns_consumed))
#define OBJC_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

