@interface Base
@end

@interface Parent : Base
- (nonnull instancetype)init __attribute__((objc_designated_initializer));
@end

@interface GenericParent<T: Base *> : Base
- (nonnull instancetype)init __attribute__((objc_designated_initializer));
@end
