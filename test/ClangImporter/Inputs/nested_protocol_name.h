@protocol TrunkBranchProtocol;

__attribute__((objc_root_class))
@interface Trunk
- (instancetype)init;
- (void)addLimb:(id<TrunkBranchProtocol>)limb;
@end

// NS_SWIFT_NAME(Trunk.Branch)
__attribute__((swift_name("Trunk.Branch")))
@protocol TrunkBranchProtocol
- (void) flower;
@end

