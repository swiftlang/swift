@import Foundation;

@interface LifetimeGenericBase<__covariant ModelType> : NSObject

typedef ModelType _Nonnull (^LifetimeModelBlock)(ModelType _Nonnull model);

- (nonnull instancetype)initWithBlock:(nonnull LifetimeModelBlock)block
                              context:(nullable id)context NS_DESIGNATED_INITIALIZER;
- (nonnull instancetype)initWithBlock:(nonnull LifetimeModelBlock)block;

@end

@interface LifetimeConcreteSub : LifetimeGenericBase

- (nonnull instancetype)initWithBlock:(nonnull LifetimeModelBlock)block
                              context:(nullable id)context __attribute__((unavailable));
- (nonnull instancetype)initWithBlock:(nonnull LifetimeModelBlock)block __attribute__((unavailable));
- (nonnull instancetype)initWithValue:(nonnull id)value NS_DESIGNATED_INITIALIZER;

@end
