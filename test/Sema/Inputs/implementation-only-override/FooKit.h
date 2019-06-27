@interface Base
@end

@interface Parent : Base
- (nonnull instancetype)init __attribute__((objc_designated_initializer));

// The SECRET is on a non-secret property here because we need to enforce that
// it's not printed.
@property (readonly, strong, nullable) Parent *redefinedPropSECRET;
@end

@interface GenericParent<T: Base *> : Base
- (nonnull instancetype)init __attribute__((objc_designated_initializer));
@end

@interface SubscriptParent : Base
- (nullable Parent *)objectAtIndexedSubscript:(int)index;
@end
