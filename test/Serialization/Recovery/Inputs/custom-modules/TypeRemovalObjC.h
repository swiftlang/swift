@protocol AProto
@end

#if !BAD
__attribute__((objc_root_class))
@interface Base
- (instancetype)init;
@end

@interface Generic<T>: Base
@end

@protocol SomeProto
@end
#endif

@protocol ZProto
@end
