@class SwiftClass, SwiftClassWithCustomName;

@interface BridgingHeader
+ (void)takeForward:(SwiftClass *)class;
+ (void)takeRenamedForward:(SwiftClassWithCustomName *)class;
@end
