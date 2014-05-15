@import Foundation;

@protocol NSAppearanceCustomization <NSObject>
@end

@interface NSResponder : NSObject
@end

// Specifically testing re-adopting a protocol that's adopted by a base class.
@interface NSWindow : NSResponder <NSAppearanceCustomization>
@end

@interface NSDocument : NSObject
- (instancetype)init;
- (instancetype)initWithURL:(NSString*)url;

- (void)copyDocumentFromURL:(NSURL*)fromURL toURL:(NSURL*)toURL;
- (void)scaleXBy:(NSInteger)value;
@end

@interface NSAwesomeDocument : NSDocument
-(void)noReturnMethod:(int)arg  __attribute__((noreturn));
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

@interface NCWidgetController : NSObject
+ (instancetype)widgetController;
@end
