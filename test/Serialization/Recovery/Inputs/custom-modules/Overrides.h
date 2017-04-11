@interface Object
- (nonnull instancetype)init;
@end

@interface Base : Object
#if !BAD
- (void)disappearingMethod;
- (nullable id)nullabilityChangeMethod;
- (nonnull id)typeChangeMethod;
#else
//- (void)disappearingMethod;
- (nonnull id)nullabilityChangeMethod;
- (nonnull Base *)typeChangeMethod;
#endif
@end

@interface Base (ExtraMethodsToTriggerCircularReferences)
#if !BAD
- (void)disappearingMethodWithOverload;
#else
//- (void)disappearingMethodWithOverload;
#endif
@end

@interface GenericBase<Element>: Object
#if !BAD
- (void)disappearingMethod;
- (nullable Element)nullabilityChangeMethod;
- (nonnull id)typeChangeMethod;
#else
//- (void)disappearingMethod;
- (nonnull Element)nullabilityChangeMethod;
- (nonnull Element)typeChangeMethod;
#endif
@end
