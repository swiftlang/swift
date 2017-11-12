@import Foundation;

__attribute__((objc_root_class))
@interface Base
- (instancetype)init;
@end

@interface UnbridgedNonNSObject : Base
@end

__attribute__((swift_bridge("BridgedValue")))
@interface BridgedNonNSObject : Base
@end

typedef UnbridgedNonNSObject *WrappedRef __attribute__((swift_wrapper(struct)));
typedef BridgedNonNSObject *WrappedValue __attribute__((swift_wrapper(struct)));
