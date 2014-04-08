@import ObjectiveC;

__attribute__((objc_root_class))
@interface Base
- (instancetype)returnMyself;
@end


@interface PropertyAndMethodCollisionBase
- (void)object:(id)obj doSomething:(SEL)selector;
@end

@interface PropertyAndMethodCollision : PropertyAndMethodCollisionBase
@property id object;
@end

@interface PropertyAndMethodReverseCollisionBase
@property id object;
@end

@interface PropertyAndMethodReverseCollision : PropertyAndMethodReverseCollisionBase
- (void)object:(id)obj doSomething:(SEL)selector;
@end

@protocol PropertyProto
@property id protoProp;
@property(readonly) id protoPropRO;
@end

@interface PropertyAndMethodCollision () <PropertyProto>
- (id)protoProp;
- (id)protoPropRO;
@end
