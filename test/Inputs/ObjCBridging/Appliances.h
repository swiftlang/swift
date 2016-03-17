@import Foundation;

__attribute__((swift_bridge("Refrigerator")))
@interface APPRefrigerator : NSObject <NSCopying>
-(nonnull instancetype)initWithTemperature:(double)temperature __attribute__((objc_designated_initializer));
@property (nonatomic) double temperature;
@end

@interface APPHouse : NSObject
@property (nonatomic,nonnull,copy) APPRefrigerator *fridge;
@end
