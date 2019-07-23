@import UnimportableMembers;

@interface DesignatedInitializerInAnotherModule (/*evil class extension*/)
- (instancetype)initFromOtherModule:(long)x __attribute__((objc_designated_initializer));
@end
