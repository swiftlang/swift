@import ObjectiveC;
@import CoreGraphics;

@protocol AVVideoCompositionInstruction<NSObject>
@required
@property (nonatomic, readonly) BOOL enablePostProcessing;
@end

@interface AVVideoCompositionInstruction : NSObject </*NSSecureCoding, NSCopying, NSMutableCopying,*/ AVVideoCompositionInstruction>

/* Indicates the background color of the composition. Solid BGRA colors only are supported; patterns and other color refs that are not supported will be ignored.
 If the background color is not specified the video compositor will use a default backgroundColor of opaque black.
 If the rendered pixel buffer does not have alpha, the alpha value of the backgroundColor will be ignored. */
@property (nonatomic, retain) __attribute__((NSObject)) CGColorRef backgroundColor;

@end
