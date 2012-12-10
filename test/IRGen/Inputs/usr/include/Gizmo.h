@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
@end

#define NS_RETURNS_RETAINED __attribute__((ns_returns_retained))
#define NS_CONSUMES_SELF __attribute__((ns_consumes_self))
#define NS_CONSUMED __attribute__((ns_consumed))


@interface Gizmo : NSObject
- (Gizmo*) clone NS_RETURNS_RETAINED;
- (Gizmo*) duplicate;
- (void) fork NS_CONSUMES_SELF;
+ (void) consume: (NS_CONSUMED Gizmo*) gizmo;
+ (void) inspect: (Gizmo*) gizmo;
@end
