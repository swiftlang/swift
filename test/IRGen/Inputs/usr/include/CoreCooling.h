typedef const struct __attribute__((objc_bridge(id))) __CCPowerSupply *CCPowerSupplyRef;

/// The standard power supply.
extern const CCPowerSupplyRef kCCPowerStandard;

typedef const struct __attribute__((objc_bridge(id))) __CCRefrigerator *CCRefrigeratorRef;
CCRefrigeratorRef CCRefrigeratorCreate(CCPowerSupplyRef power);

void CCRefrigeratorOpen(CCRefrigeratorRef fridge);
void CCRefrigeratorClose(CCRefrigeratorRef fridge);
