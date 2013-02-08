@interface NSObject
+ (NSObject*) alloc;
- (NSObject*) init;
+ (NSObject*) new;
@end

#define NS_RETURNS_RETAINED __attribute__((ns_returns_retained))
#define NS_CONSUMES_SELF __attribute__((ns_consumes_self))
#define NS_CONSUMED __attribute__((ns_consumed))

struct Rect {
  float x;
  float y;
  float width;
  float height;
};

struct NSRect {
  struct NSPoint {
    double x;
    double y;
  } origin;
  struct NSSize {
    double width;
    double height;
  } size;
};

@interface Gizmo : NSObject
- (Gizmo*) clone NS_RETURNS_RETAINED;
- (Gizmo*) duplicate;
- (void) fork NS_CONSUMES_SELF;
- (void) enumerateSubGizmos: (void (^)(Gizmo*))f;
+ (void) consume: (NS_CONSUMED Gizmo*) gizmo;
+ (void) inspect: (Gizmo*) gizmo;
+ (void) runWithRect: (struct Rect) rect andGizmo: (Gizmo*) gizmo;
- (struct NSRect) frame;
- (void) setFrame: (struct NSRect) rect;
@end

@interface NSString : NSObject
@end

struct NSRect NSMakeRect(double, double, double, double);
NSString *NSStringFromRect(struct NSRect r);
