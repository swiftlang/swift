#if __has_feature(objc_modules)
@import Foundation;
#else
#import <Foundation/Foundation.h>
#endif

@interface APPRefrigerator : NSObject <NSCopying>
-(_Nonnull instancetype)initWithTemperature:(double)temperature __attribute__((objc_designated_initializer));
@property (nonatomic) double temperature;
@end

@interface APPHouse : NSObject
@property (nonatomic,nonnull,copy) APPRefrigerator *fridge;
@end


@interface APPManufacturerInfo <DataType> : NSObject
@property (nonatomic,nonnull,readonly) DataType value;
@end

@interface APPBroken : NSObject
@property (nonatomic,nonnull,readonly) id thing;
@end

void takesNonStandardBlock(__attribute__((__ns_returns_retained__)) _Null_unspecified id (^ _Null_unspecified)(void));
