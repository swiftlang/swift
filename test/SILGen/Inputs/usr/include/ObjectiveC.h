typedef __typeof__(__objc_yes) BOOL;
typedef long NSInteger;

@class NSString;

typedef struct _NSZone NSZone;

@protocol NSCopying
- (id) copyWithZone: (nullable NSZone*)zone;
@end

SEL sel_registerName(const char *);

@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
+ (void) load;
+ (void) initialize;

- (id)performSelector:(SEL)selector withObject:(id)object;

@property (readonly, copy) NSString *description;

@property NSString *stringProperty;

@property Class classProp;
@end

#define NS_RETURNS_INNER_POINTER __attribute__((objc_returns_inner_pointer))
#define NS_RETURNS_RETAINED __attribute__((ns_returns_retained))
#define NS_CONSUMES_SELF __attribute__((ns_consumes_self))
#define NS_CONSUMED __attribute__((ns_consumed))
#define OBJC_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))

