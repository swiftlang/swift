typedef __typeof__(__objc_yes) BOOL;
typedef long NSInteger;

@class NSString;

@protocol NSCopying
- (id) copyWithZone: (void*)zone;
@end

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;
+ (void) initialize;

@property (readonly, copy) NSString *description;

@property NSString *stringProperty;

@property Class classProp;
@end

#define NS_RETURNS_INNER_POINTER __attribute__((objc_returns_inner_pointer))
#define NS_RETURNS_RETAINED __attribute__((ns_returns_retained))
#define NS_CONSUMES_SELF __attribute__((ns_consumes_self))
#define NS_CONSUMED __attribute__((ns_consumed))
#define OBJC_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

