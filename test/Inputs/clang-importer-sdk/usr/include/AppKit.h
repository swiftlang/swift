@import Foundation;
@import ctypes;

@protocol NSAppearanceCustomization <NSObject>
@end

@interface NSResponder : NSObject
@end

@protocol NSAccessibilityElement <NSObject>
@required
- (id)accessibilityParent;
@end
@protocol NSAccessibilityButton <NSAccessibilityElement>
@required
- (NSString *)accessibilityLabel;
- (BOOL)accessibilityPerformPress;
@end
// The complete accessibility protocol
@protocol NSAccessibility <NSObject>
@required
// Element containing this UIElement
// Invokes when clients request NSAccessibilityParentAttribute
@property (weak) id accessibilityParent;
// Description of UIElement
// Invokes when clients request NSAccessibilityDescriptionAttribute
@property (copy) NSString *accessibilityLabel;
- (NSString *)accessibilityLabel;
// Invokes when clients perform NSAccessibilityPressAction
- (BOOL)accessibilityPerformPress;
@property (getter = isAccessibilityFocused) BOOL accessibilityFocused;
@end

// Specifically testing re-adopting a protocol that's adopted by a base class.
@interface NSWindow : NSResponder <NSAppearanceCustomization>
@end

@interface NSDocument : NSObject
- (instancetype)init;
- (instancetype)initWithContentsOfURL:(NSURL *)url ofType:(NSString *)type error:(NSError **)error;

- (BOOL)readFromURL:(NSURL *)url ofType:(NSString *)typeName error:(NSError **)outError;
- (BOOL)writeToURL:(NSURL *)url ofType:(NSString *)typeName error:(NSError **)outError;

- (void)copyDocumentFromURL:(NSURL*)fromURL toURL:(NSURL*)toURL;
- (void)scaleXBy:(NSInteger)value;
@end

@interface NSDocument (NSDeprecated)
- (nullable id)initWithContentsOfURL:(NSURL *)url ofType:(NSString *)typeName __attribute__((availability(macosx, introduced=10.0, deprecated=10.4)));
- (BOOL)readFromURL:(NSURL *)url ofType:(NSString *)type __attribute__((availability(macosx, introduced=10.0, deprecated=10.4)));
- (BOOL)writeToURL:(NSURL *)url ofType:(NSString *)type __attribute__((availability(macosx, introduced=10.0, deprecated=10.4)));
@end

@interface NSAwesomeDocument : NSDocument
-(void)noReturnMethod:(int)arg  __attribute__((noreturn));
@end

@interface NSDocumentController : NSObject
- (nullable id)makeDocumentWithContentsOfURL:(NSURL *)url ofType:(NSString *)typeName error:(NSError **)outError;
@end

@interface NSDocumentController (NSDeprecated)
- (nullable id)makeDocumentWithContentsOfURL:(NSURL *)url ofType:(null_unspecified NSString *)type __attribute__((availability(macosx, introduced=10.0, deprecated=10.4)));
@end

@interface URLDocument : NSObject
- (instancetype)init NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithURL:(NSString *)urlString;
@end

@interface NSInterestingDesignated : NSObject
+ (instancetype)alloc;
- (instancetype)initWithString:(NSString*)str NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithURL:(NSString*)str;
@end

@interface NSInterestingDesignatedSub : NSInterestingDesignated
@end

@interface NSColor : NSObject
+ (instancetype)colorWithDeviceRed:(double)red green:(double)green blue:(double)blue alpha:(double)alpha;
+ (NSColor*)redColor;
- (instancetype)sameColor;
- (void)getRGBAComponents:(nullable char *)components;
@end

@protocol NSAnimatablePropertyContainer
- (instancetype)animator __attribute__((availability(macosx,introduced=10.5)));
@end

@interface NSLayoutConstraint : NSObject <NSAnimatablePropertyContainer>
@property double constant;
@end

@protocol MyDelegate
  -(BOOL)receiverShouldJumpOnTable:(NSObject *)table;
@end

@interface NSScrollView : NSObject
- (void)scrollItemAtIndexToTop:(int)index;
@end

@interface NSViewController : NSObject <NSCoding>
@end

@interface NSTableViewController : NSViewController
-(instancetype)initWithInt:(NSInteger)value NS_DESIGNATED_INITIALIZER;
@end

@interface NSObjectFactory : NSObject
+(instancetype)objectFactory;
+(instancetype)objectFactoryWithInteger:(NSInteger)i;
+(instancetype)factoryWithDouble:(double)i;
+(id)factoryWithString:(NSString *)s;
+(NSObjectFactory*)factoryWithFloat:(float)f;
+(instancetype)factoryBuildingWidgets;
@end

@interface NSObjectFactorySub : NSObjectFactory
@end

@interface NSHavingConvenienceFactoryAndLaterConvenienceInit : NSObject
-(instancetype)initWithStuff:(NSObject *)stuff NS_DESIGNATED_INITIALIZER;

// Convenience factory declaration followed by convenience init
+(instancetype)havingConvenienceFactoryAndLaterConvenienceInitWithFlim:(NSInteger)flim;
-(instancetype)initWithFlim:(NSInteger)flim __attribute__((availability(macosx,introduced=52)));

// Convenience init declaration followed by convenience factory
-(instancetype)initWithFlam:(NSInteger)flam __attribute__((availability(macosx,introduced=52)));
+(instancetype)havingConvenienceFactoryAndLaterConvenienceInitWithFlam:(NSInteger)flam;
@end

@interface NSHavingConvenienceFactoryAndEarlierConvenienceInit : NSObject
-(instancetype)init NS_DESIGNATED_INITIALIZER;

+(instancetype)havingConvenienceFactoryAndEarlierConvenienceInitWithFlim:(NSInteger)flim __attribute__((availability(macosx,introduced=52)));
-(instancetype)initWithFlim:(NSInteger)flim;

-(instancetype)initWithFlam:(NSInteger)flam;
+(instancetype)havingConvenienceFactoryAndEarlierConvenienceInitWithFlam:(NSInteger)flam __attribute__((availability(macosx,introduced=52)));
@end

@interface NSHavingConvenienceFactoryAndSameConvenienceInit : NSObject
-(instancetype)init NS_DESIGNATED_INITIALIZER;

// We distinguish between which of these the importer chose by the deprecation message.
+(instancetype)havingConvenienceFactoryAndSameConvenienceInitWithFlim:(NSInteger)flim __attribute__((availability(macosx,introduced=10.8, deprecated=51, message="ConvenienceFactory")));
-(instancetype)initWithFlim:(NSInteger)flim __attribute__((availability(macosx,introduced=10.8, deprecated=51, message="ConvenienceInit")));

-(instancetype)initWithFlam:(NSInteger)flam __attribute__((availability(macosx,introduced=10.8, deprecated=51, message="ConvenienceInit")));
+(instancetype)havingConvenienceFactoryAndSameConvenienceInitWithFlam:(NSInteger)flam __attribute__((availability(macosx,introduced=10.8, deprecated=51, message="ConvenienceFactory")));

+(instancetype)havingConvenienceFactoryAndSameConvenienceInitWithFlotsam:(NSInteger)flotsam __attribute__((deprecated("ConvenienceFactory")));
-(instancetype)initWithFlotsam:(NSInteger)flotsam __attribute__((deprecated("ConvenienceInit")));

-(instancetype)initWithJetsam:(NSInteger)jetsam __attribute__((deprecated("ConvenienceInit")));
+(instancetype)havingConvenienceFactoryAndSameConvenienceInitWithJetsam:(NSInteger)jetsam __attribute__((deprecated("ConvenienceFactory")));
@end


@interface NSHavingConvenienceFactoryAndLaterDesignatedInit : NSObject
+(instancetype)havingConvenienceFactoryAndLaterDesignatedInitWithFlim:(NSInteger)flim;
-(instancetype)initWithFlim:(NSInteger)flim NS_DESIGNATED_INITIALIZER __attribute__((availability(macosx,introduced=52)));

-(instancetype)initWithFlam:(NSInteger)flam NS_DESIGNATED_INITIALIZER __attribute__((availability(macosx,introduced=52)));
+(instancetype)havingConvenienceFactoryAndLaterDesignatedInitWithFlam:(NSInteger)flam;
@end

@interface NSHavingFactoryAndLaterConvenienceInit : NSObject
-(instancetype)init NS_DESIGNATED_INITIALIZER;

+(NSHavingFactoryAndLaterConvenienceInit *)havingFactoryAndLaterConvenienceInitWithFlim:(NSInteger)flim;
-(instancetype)initWithFlim:(NSInteger)flim __attribute__((availability(macosx,introduced=52)));

-(instancetype)initWithFlam:(NSInteger)flam __attribute__((availability(macosx,introduced=52)));
+(NSHavingFactoryAndLaterConvenienceInit *)havingFactoryAndLaterConvenienceInitWithFlam:(NSInteger)flam;
@end

@interface NSHavingUnavailableFactoryAndUnavailableConvenienceInit : NSObject
-(instancetype)init NS_DESIGNATED_INITIALIZER;

+(NSHavingUnavailableFactoryAndUnavailableConvenienceInit *)havingUnavailableFactoryAndUnavailableConvenienceInitWithFlim:(NSInteger)flim __attribute__((unavailable("Factory")));
-(instancetype)initWithFlim:(NSInteger)flim __attribute__((unavailable("ConvenienceInit")));

-(instancetype)initWithFlam:(NSInteger)flam __attribute__((unavailable("ConvenienceInit")));
+(NSHavingUnavailableFactoryAndUnavailableConvenienceInit *)havingUnavailableFactoryAndUnavailableConvenienceInit:(NSInteger)flam __attribute__((unavailable("Factory")));
@end

@interface NSString(Category)
- (NSString*)nsStringMethod;
+ (NSInteger)nsStringClassMethod;
@property (readonly) NSString *nsStringProperty;
+ (NSString*)someFactoryMethod;
@end

@interface NSDictionary(Category)
- (NSDictionary*)nsDictionaryMethod;
+ (NSInteger)nsDictionaryClassMethod;
@property (readonly) NSDictionary *nsDictionaryProperty;
@end

@interface CALayer
- (instancetype)init;
@end

@interface NSView : NSObject <NSCoding, NSAccessibility>
- (nullable instancetype)initWithCoder:(nonnull NSCoder *)aDecoder;
- (BOOL)isDescendantOf:(nonnull NSView *)aView;
- (nullable NSView *)ancestorSharedWithView:(nonnull NSView *)aView;
- (void)setSubviews:(nonnull NSArray *)newSubviews;
- (void)addSubview:(nonnull NSView *)aView;
- (void)addSubview:(nonnull NSView *)aView positioned:(unsigned)place relativeTo:(nullable NSView *)otherView;
@property (readonly, assign, nullable) NSView *superview;
@property (strong, nullable) CALayer *layer;
@property (readonly, copy, nonnull) NSArray *trackingAreas;
@property (copy, nonnull) NSArray *subviews;

- (void)print:(id)sender;
@end

@interface NSBox : NSView
@end

@interface NSView(NSKeyboardUI)
@property (assign, nullable) NSView *nextKeyView;
@end

@interface NSMenu : NSObject <NSCopying, NSCoding>
- (instancetype)initWithTitle:(NSString *)title;
@end

@interface NSMenuItem : NSObject <NSCopying, NSCoding>
// Setter is only for subclassers.
@property (assign, nullable) NSMenu *menu;

@property (copy, nonnull) NSString *title;
@property (copy, nullable) NSAttributedString *attributedTitle;

@property (weak) id target;
@property SEL action;
@end

extern NSString *NSViewFrameDidChangeNotification;
extern NSString *NSViewFocusDidChangeNotification;

@protocol NSApplicationDelegate
@end

struct Point3D { double x, y, z; };
@interface NSString (Drawing)
-(void)drawInAirAtPoint:(struct Point3D)point;
-(void)drawAtPoint:(struct Point3D)point withAttributes:(nullable NSDictionary<NSString *, id> *)attributes;
-(void)setTextColor:(nullable NSColor *)color;
-(void)drawInView:(nullable NSView *)view;
-(void)drawAnywhereInView:(nullable NSView *)view options:(nonnull NSDictionary *)options;
-(void)drawAnywhereWithOptions:(nonnull NSDictionary *)options;
-(void)drawAnywhereWithOptionalOptions:(nullable NSDictionary *)options;
@end

@interface NSBezierPath : NSObject
- (nonnull NSBezierPath *)bezierPathByReversingPath;
- (nonnull instancetype)bezierPathByInventingPath;
@property(readonly,nonnull) NSBezierPath *bezierPathByFlatteningPath;
@end

@interface NSViewController ()
- (void)dismissViewControllerAnimated:(BOOL)animated;
@end

@interface NSScrollView ()
- (BOOL)shouldCollapseAutoExpandedItemsForDeposited:(BOOL)deposited;
- (NSRect)rectForCancelButtonWhenCentered:(BOOL)isCentered;
@end

@interface NSDocumentController ()
- (void)openUntitledDocumentAndDisplay:(BOOL)displayDocument;
@end

typedef float NSLayoutPriority;
typedef NSPoint *NSPointPointer;

@interface NSView ()
-(void)setContentHuggingPriority:(NSLayoutPriority)priority;
-(void)layoutAtPoint:(NSPointPointer)point;
@end

@interface NSGestureRecognizer : NSObject
@end

@interface NSView (Gestures)
@property (readonly, copy) NSArray<__kindof NSGestureRecognizer *> *gestureRecognizers;
- (void)addGestureRecognizer:(nonnull NSGestureRecognizer *)gestureRecognizer;
- (void)removeGestureRecognizer:(nonnull NSGestureRecognizer *)gestureRecognizer;
- (nullable NSView *)favoriteViewForGestureRecognizer:(nonnull NSGestureRecognizer *)gestureRecognizer;

- (nonnull NSSet<NSLayoutConstraint *> *)layoutConstraints;
- (void)addLayoutConstraints:(nonnull NSSet<NSLayoutConstraint *> *)layoutConstraints;

+ (NSRect)rect;
- (void)addRect:(NSRect)rect;
+ (void)conjureRect:(NSRect)rect;
@end

@interface NSDocument (URL)
@property (copy,nonnull) NSURL *URL;
@end

typedef NS_ENUM(NSUInteger, NSBezierPathElement) {
    NSBezierPathElementMoveTo,
    NSBezierPathElementLineTo,
    NSBezierPathElementCubicCurveTo __attribute__((availability(macosx,introduced=52))),
    NSBezierPathElementClosePath,
    NSBezierPathElementQuadraticCurveTo __attribute__((availability(macosx,introduced=52))),
    NSBezierPathElementCurveTo __attribute__((availability(macosx,introduced=51,deprecated=52,message="Use NSBezierPathElementCubicCurveTo"))) = NSBezierPathElementCubicCurveTo,
};
