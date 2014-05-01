typedef const struct __CCPowerSupply *CCPowerSupplyRef;

/// The standard power supply.
extern const CCPowerSupplyRef kCCPowerStandard;

typedef const struct __CCRefrigerator *CCRefrigeratorRef;
CCRefrigeratorRef CCRefrigeratorCreate(CCPowerSupplyRef power);
