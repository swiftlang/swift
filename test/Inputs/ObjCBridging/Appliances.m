#import "Appliances.h"

@implementation APPRefrigerator
-(instancetype)initWithTemperature:(double)temperature {
  self = [super init];
  if (self) {
    self->_temperature = temperature;
  }
  return self;
}

-(id)copyWithZone:(NSZone *)zone {
  return [[APPRefrigerator alloc] initWithTemperature: self.temperature];
}
@end

@implementation APPHouse
-(instancetype)init {
  self = [super init];
  if (self) {
    self->_fridge = [[APPRefrigerator alloc] initWithTemperature:50];
  }
  return self;
}
@end

@implementation APPManufacturerInfo
@end

@implementation APPBroken

- (id _Nonnull)thing {
  return (id _Nonnull)nil;
}

@end
