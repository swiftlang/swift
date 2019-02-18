typedef const void *CFTypeRef __attribute__((objc_bridge(id)));

typedef const struct __attribute__((objc_bridge(id))) CCPowerSupply *CCPowerSupplyRef;
typedef const struct __attribute__((objc_bridge(id))) __CCRefrigerator *CCRefrigeratorRef;
typedef struct __CCRefrigerator *CCMutableRefrigeratorRef;

#pragma clang arc_cf_code_audited begin
_Nonnull CCPowerSupplyRef CCPowerSupplyCreate(double watts)
  __attribute__((swift_name("CCPowerSupply.init(watts:)")));

_Nonnull CCRefrigeratorRef CCRefrigeratorCreate(CCPowerSupplyRef _Nonnull power)
  __attribute__((swift_name("CCRefrigerator.init(powerSupply:)")));

void CCRefrigeratorOpen(_Null_unspecified CCRefrigeratorRef fridge)
  __attribute__((swift_name("CCRefrigerator.open(self:)")));

_Nonnull CCMutableRefrigeratorRef CCRefrigeratorCreateMutable(_Nonnull CCPowerSupplyRef power)
  __attribute__((swift_name("CCMutableRefrigerator.init(powerSupply:)")));

_Nonnull CCPowerSupplyRef CCRefrigeratorGetPowerSupply(_Null_unspecified CCRefrigeratorRef fridge)
  __attribute__((swift_name("getter:CCRefrigerator.powerSupply(self:)")));

void CCRefrigeratorSetPowerSupply(_Null_unspecified CCRefrigeratorRef fridge,
                                  CCPowerSupplyRef _Nonnull powerSupply)
  __attribute__((swift_name("setter:CCRefrigerator.powerSupply(self:_:)")));

extern const _Null_unspecified CCPowerSupplyRef kCCPowerSupplySemiModular
  __attribute__((swift_name("CCPowerSupplyRef.semiModular")));

_Nonnull CCPowerSupplyRef CCPowerSupplyCreateDangerous(void)
  __attribute__((swift_name("CCPowerSupply.init(dangerous:)")));
#pragma clang arc_cf_code_audited end

extern const double kCCPowerSupplyDefaultPower
  __attribute__((swift_name("CCPowerSupply.defaultPower")));

extern const _Nonnull CCPowerSupplyRef kCCPowerSupplyAC
  __attribute__((swift_name("CCPowerSupply.AC")));

extern const _Nullable CCPowerSupplyRef kCCPowerSupplyDC
  __attribute__((swift_name("CCPowerSupply.DC")));
