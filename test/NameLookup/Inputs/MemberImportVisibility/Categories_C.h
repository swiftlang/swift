@import Categories_A;

@interface X (C)
- (void)fromC;
@end

@interface SubclassFromC : X
- (instancetype)init;
@end

@protocol ObjCProtoInC
- (void)witnessesObjCConformanceRequirementInC __attribute__((deprecated("ObjCProtoInC")));
@end

@interface Base () <ObjCProtoInC>
@end

@interface NSObject (Categories_C)
- (nullable id)overriddenInCategoryWithMethodReturningOptional;
- (void)optionalRequirementWitnessedInC;
@end
