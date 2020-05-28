void doSomethingInHead(int arg);
#define NS_REFINED_FOR_SWIFT __attribute__((swift_private))
@interface BaseInHead
- (void)doIt:(int)arg;
- (void)otherThing:(int)arg NS_REFINED_FOR_SWIFT;
@end

/// Awesome name.
/// ÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
/// Awesome name.
@interface SameName
@end

// random comment.

@protocol SameName
@end
