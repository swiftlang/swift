@import Foundation;

@protocol NSAppearanceCustomization <NSObject>
@end

@interface NSResponder : NSObject
@end

// Specifically testing re-adopting a protocol that's adopted by a base class.
@interface NSWindow : NSResponder <NSAppearanceCustomization>
@end

@interface NSDocument : NSObject
- (instancetype)init NS_DESIGNATED_INITIALIZER;
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
