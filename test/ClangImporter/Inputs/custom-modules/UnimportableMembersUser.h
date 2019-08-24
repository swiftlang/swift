@import UnimportableMembers;

@interface DesignatedInitializerInAnotherModule (/*evil class extension*/)
- (instancetype)initFromOtherModule:(intptr_t)x __attribute__((objc_designated_initializer));
@end
