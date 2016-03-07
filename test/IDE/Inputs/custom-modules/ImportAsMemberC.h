typedef const void *CFTypeRef __attribute__((objc_bridge(id)));

typedef const struct __attribute__((objc_bridge(id))) __CCPowerSupply *CCPowerSupplyRef;
typedef const struct __attribute__((objc_bridge(id))) __CCRefrigerator *CCRefrigeratorRef;
typedef struct __CCRefrigerator *CCMutableRefrigeratorRef;

_Nonnull CCPowerSupplyRef CCPowerSupplyCreate(double watts)
  __attribute__((swift_name("CCPowerSupply.init(watts:)")));

_Nonnull CCRefrigeratorRef CCRefrigeratorCreate(CCPowerSupplyRef _Nonnull power)
  __attribute__((swift_name("CCRefrigerator.init(powerSupply:)")));

void CCRefrigeratorOpen(CCRefrigeratorRef fridge)
  __attribute__((swift_name("CCRefrigerator.open(self:)")));

_Nonnull CCMutableRefrigeratorRef CCRefrigeratorCreateMutable(_Nonnull CCPowerSupplyRef power)
  __attribute__((swift_name("CCMutableRefrigerator.init(powerSupply:)")));

_Nonnull CCPowerSupplyRef CCRefrigeratorGetPowerSupply(CCRefrigeratorRef fridge)
  __attribute__((swift_name("getter:CCRefrigerator.powerSupply(self:)")));

void CCRefrigeratorSetPowerSupply(CCRefrigeratorRef fridge,
                                  CCPowerSupplyRef _Nonnull powerSupply)
  __attribute__((swift_name("setter:CCRefrigerator.powerSupply(self:_:)")));





