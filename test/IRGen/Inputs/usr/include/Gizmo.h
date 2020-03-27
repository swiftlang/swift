@import ObjectiveC;
@import Foundation;

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

struct Fob {
  unsigned long a;
  unsigned b;
  unsigned c;
  unsigned long d;
} Fob;

typedef long NSInteger;

@interface Gizmo : NSObject
- (Gizmo*) clone NS_RETURNS_RETAINED;
- (Gizmo*) duplicate;
- (Gizmo*) initWithBellsOn:(NSInteger)x;
- (void) fork NS_CONSUMES_SELF;
- (void) enumerateSubGizmos: (void (^)(Gizmo*))f;
+ (void) consume: (NS_CONSUMED Gizmo*) gizmo;
+ (void) inspect: (Gizmo*) gizmo;
+ (void) runWithRect: (struct Rect) rect andGizmo: (Gizmo*) gizmo;
- (struct NSRect) frame;
- (void) setFrame: (struct NSRect) rect;
- (void) frob;
- (void) test: (struct Fob) fob;
- (void) perform: (void (^)(NS_CONSUMED Gizmo*)) block;
+ (void) runce;
@end

@interface BaseClassForMethodFamilies : NSObject
- (BaseClassForMethodFamilies *)fakeInitFamily __attribute__((objc_method_family(init)));
@end

static inline int innerZero(void) { return 0; }
static inline int zero(void) { return innerZero(); }
static inline int wrappedZero(void) { return zero(); }

static inline int alwaysInlineNumber(void) __attribute__((always_inline)) {
  return 17;
}

extern int getInt(void);
static inline int wrappedGetInt(void) { return getInt(); }

static inline int zeroRedeclared(void);
static inline int wrappedZeroRedeclared(void) { return zeroRedeclared(); }
static inline int zeroRedeclared(void) { return innerZero(); }

static int staticButNotInline(void) { return innerZero(); }

@interface NSView : NSObject
- (struct NSRect) convertRectFromBase: (struct NSRect) r;
@end

struct NSRect NSMakeRect(double, double, double, double);
struct NSRect NSInsetRect(struct NSRect, double, double);
NSString *NSStringFromRect(struct NSRect r);

@protocol NSRuncing
- (void)runce;
- (void)foo;
@end

@protocol NSFunging
- (void)funge;
- (void)foo;
@end

@interface NSSpoon <NSRuncing, NSFunging>
- (void)runce;
- (void)funge;
- (void)foo;
@end

@protocol NSFungingAndRuncing <NSRuncing, NSFunging>
@end

@protocol NSDoubleInheritedFunging <NSFungingAndRuncing, NSFunging>
@end

typedef NS_ENUM(unsigned short, NSRuncingOptions) {
  NSRuncingMince = 123,
  NSRuncingQuinceSliced = 4567,
  NSRuncingQuinceJulienned = 5678,
  NSRuncingQuinceDiced = 6789,
};

typedef NS_ENUM(int, NSRadixedOptions) {
  NSRadixedOctal = 0755,
  NSRadixedHex = 0xFFFF,
};

typedef NS_ENUM(int, NSNegativeOptions) {
  NSNegativeFoo = -1,
  NSNegativeBar = -0x7FFFFFFF - 1,
};

typedef NS_ENUM(unsigned, NSNegativeUnsignedOptions) {
  NSNegativeUnsignedFoo = -1,
  NSNegativeUnsignedBar = -0x7FFFFFFF - 1,
};

typedef NS_ENUM(unsigned, NeverActuallyMentionedByName) {
  ValueOfThatEnumType = 5
};
@interface TestThatEnumType
- (instancetype)init;
- (NeverActuallyMentionedByName)getValue;
@end

#if defined(_WIN32)
enum RawEnumInGizmo : unsigned {
  InGizmoOne=0x7FFFFFFF,
  InGizmoTwo,
  InGizmoThree
};
#else
enum RawEnumInGizmo {
  InGizmoOne=0x7FFFFFFF,
  InGizmoTwo,
  InGizmoThree
};
#endif

struct StructOfNSStrings {
  __unsafe_unretained NSString *a;
  __unsafe_unretained NSString *b;
  __unsafe_unretained NSString *c;
  __unsafe_unretained NSString *d;
};

struct StructOfNSStrings useStructOfNSStringsInObjC(struct StructOfNSStrings);

@interface OuterType : NSObject
@end

__attribute__((swift_name("OuterType.InnerType")))
@interface OuterTypeInnerType : NSObject<NSRuncing>
@end
