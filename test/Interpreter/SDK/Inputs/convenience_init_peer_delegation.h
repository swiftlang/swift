@import Foundation;

extern NSInteger baseCounter;
extern NSInteger subCounter;

@interface Base : NSObject
- (nonnull instancetype)init __attribute__((objc_designated_initializer));
- (nonnull instancetype)initConveniently;
+ (nonnull instancetype)baseWithConvenientFactory:(_Bool)unused;
+ (nonnull Base *)baseWithNormalFactory:(_Bool)unused;
@end

@interface Sub : Base
@end
