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
@end

@interface NSAwesomeDocument : NSDocument
-(void)noReturnMethod:(int)arg  __attribute__((noreturn));
@end

@interface NSInterestingDesignated
+ (instancetype)alloc;
- (instancetype)initWithString:(NSString*)str NS_DESIGNATED_INITIALIZER;
- (instancetype)initWithURL:(NSString*)str;
@end

@interface NSInterestingDesignatedSub : NSInterestingDesignated
@end
