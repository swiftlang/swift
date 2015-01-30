typedef const struct __attribute__((objc_bridge(id))) __CCPowerSupply *CCPowerSupplyRef;

/// The standard power supply.
extern const CCPowerSupplyRef kCCPowerStandard;

// Attribute: managed +0 result
__attribute__((cf_returns_not_retained))
CCPowerSupplyRef CCPowerSupplyGetDefault();

typedef const struct __attribute__((objc_bridge(id))) __CCRefrigerator *CCRefrigeratorRef;

// Unaudited function: unmanaged result despite name
CCRefrigeratorRef CCRefrigeratorCreate(CCPowerSupplyRef power);

// Attribute: managed +1 result
__attribute__((cf_returns_retained))
CCRefrigeratorRef CCRefrigeratorSpawn(CCPowerSupplyRef power);

// Parameter: managed +0 by default
void CCRefrigeratorOpen(CCRefrigeratorRef fridge);
void CCRefrigeratorClose(CCRefrigeratorRef fridge);

#pragma clang arc_cf_code_audited begin
// Audited function with Copy convention: managed +1 result
CCRefrigeratorRef CCRefrigeratorCopy(CCRefrigeratorRef fridge);

// Audited function without Copy convention: managed +0 result
CCRefrigeratorRef CCRefrigeratorClone(CCRefrigeratorRef fridge);
#pragma clang arc_cf_code_audited end

// Attribute: managed +1 parameter
void CCRefrigeratorDestroy(__attribute__((cf_consumed)) CCRefrigeratorRef);

@interface CCMagnetismModel
// Unattributed method: unmanaged result
- (CCRefrigeratorRef) refrigerator;
// Attribute: managed +0 result
- (CCRefrigeratorRef) getRefrigerator __attribute__((cf_returns_not_retained));
// Attribute: managed +1 result
- (CCRefrigeratorRef) takeRefrigerator __attribute__((cf_returns_retained));
// Attribute: managed +0 result
- (CCRefrigeratorRef) borrowRefrigerator __attribute__((objc_returns_inner_pointer));

// Parameter: managed +0 by default
- (void) setRefrigerator: (CCRefrigeratorRef) refrigerator;
// Attribute: managed +1 parameter
- (void) giveRefrigerator: (__attribute__((cf_consumed)) CCRefrigeratorRef) refrigerator;

@property CCRefrigeratorRef fridgeProp;
- (CCRefrigeratorRef) fridgeProp __attribute__((cf_returns_not_retained));
@end
