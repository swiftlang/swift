#import "ObjectiveC.h"
#import "BridgeTestFoundation.h"

@protocol NSAnsing
- (void) anse;
@end

@interface NSObject (NSAnsing)
@property Class<NSAnsing> qualifiedClassProp;
@end

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

typedef long NSInteger;

@interface Gizmo : NSObject
- (Gizmo*) clone NS_RETURNS_RETAINED;
- (Gizmo*) duplicate;
- (Gizmo*) init OBJC_DESIGNATED_INITIALIZER;
- (Gizmo*) initWithBellsOn:(NSInteger)x OBJC_DESIGNATED_INITIALIZER;
- (instancetype) initWithoutBells:(NSInteger)x;
- (void) fork NS_CONSUMES_SELF;
- (void) enumerateSubGizmos: (void (^ _Nullable)(Gizmo*))f;
+ (void) consume: (NS_CONSUMED Gizmo*) gizmo;
+ (void) inspect: (Gizmo*) gizmo;
+ (void) runWithRect: (struct Rect) rect andGizmo: (Gizmo*) gizmo;
- (struct NSRect) frame;
- (void) setFrame: (struct NSRect) rect;
- (void) frob;
+ (void) runce;
- (void) funge;
- (void) foo;
- (void* _Nonnull) getBytes NS_RETURNS_INNER_POINTER;

- (void)doTheThingWithOptions:(nonnull NSDictionary *)options;
- (void)doTheOtherThingWithOptionalOptions:(nullable NSDictionary *)options;

@property (nonnull) void *innerProperty;
- (void* _Nonnull) innerProperty NS_RETURNS_INNER_POINTER;
- (void) setInnerProperty: (void*)p;

@property void (^block)(void);
@property NSInteger count;

+ (instancetype)gizmoWithStuff:(NSInteger)x;
+ (Gizmo*)gizmoWithExactlyStuff:(NSInteger)x;

- (Gizmo*)nonNilGizmo __attribute__((swift_name("nonNilGizmo()")));
+ (Gizmo*)nonNilGizmo __attribute__((swift_name("nonNilGizmo()")));
@property Gizmo* nonNilGizmoProperty;
@property (unsafe_unretained) Gizmo* unownedNonNilGizmoProperty;

@property id originalName __attribute__((swift_name("renamedProp")));
@end

@interface Guisemeau : Gizmo
- (id)objectAtIndexedSubscript:(NSInteger)idx;
- (void)setObject:(id)obj atIndexedSubscript:(NSInteger)idx;
@end

@interface NSView : NSObject
- (struct NSRect) convertRectFromBase: (struct NSRect) r;
@end

@interface CurryTest : NSObject
// no bridging, pod
- (NSInteger)pod:(NSInteger)x;
// requires bridging
- (NSString*)bridged:(NSString*)x;
// normal ownership conventions
- (CurryTest*)normalOwnership:(CurryTest*)x;
// weird ownership conventions
- (CurryTest*)weirdOwnership:(NS_CONSUMED CurryTest*)x
    NS_RETURNS_RETAINED NS_CONSUMES_SELF;
// covariant result type
- (instancetype)returnsSelf;
// inner pointer result
- (void*)returnsInnerPointer NS_RETURNS_INNER_POINTER;
@end

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)

struct NSRect NSMakeRect(double, double, double, double);
struct NSRect NSInsetRect(struct NSRect, double, double);
NSString *NSStringFromRect(struct NSRect r);

typedef NS_ENUM(NSInteger, NSRuncingOptions) {
  NSRuncingMince = 123,
  NSRuncingQuinceSliced = 4567,
  NSRuncingQuinceJulienned = 5678,
  NSRuncingQuinceDiced = 6789
};

#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

typedef NS_OPTIONS(NSInteger, NSFungingMask) {
  NSFungingAsset = 1,
  NSFungingLiability = 2,
  NSFungingToTheMax = (NSInteger)1U << 31
};
