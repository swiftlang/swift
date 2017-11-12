@protocol BaseProto
@end

@protocol SubProto <BaseProto>
@end

@interface UnrelatedBaseClass
@end

@interface Impl : UnrelatedBaseClass <SubProto>
- (instancetype)init;
+ (instancetype)implWithChild:(id<BaseProto>)child;
@end
