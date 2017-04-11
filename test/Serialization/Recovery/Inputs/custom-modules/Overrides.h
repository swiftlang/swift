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


@interface IndexedSubscriptDisappearsBase : Object
#if !BAD
- (nonnull id)objectAtIndexedSubscript:(long)index;
#else
//- (nonnull id)objectAtIndexedSubscript:(long)index;
#endif
@end

@interface KeyedSubscriptDisappearsBase : Object
#if !BAD
- (nonnull id)objectForKeyedSubscript:(nonnull id)key;
#else
// - (nonnull id)objectForKeyedSubscript:(nonnull id)key;
#endif
@end

@interface GenericIndexedSubscriptDisappearsBase<Element> : Object
#if !BAD
- (nonnull Element)objectAtIndexedSubscript:(long)index;
#else
// - (nonnull Element)objectAtIndexedSubscript:(long)index;
#endif
@end

@interface GenericKeyedSubscriptDisappearsBase<Element> : Object
#if !BAD
- (nonnull Element)objectAtIndexedSubscript:(nonnull id)key;
#else
// - (nonnull Element)objectAtIndexedSubscript:(nonnull id)key;
#endif
@end

