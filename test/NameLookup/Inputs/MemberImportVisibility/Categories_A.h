@import Foundation;

@interface Base : NSObject
- (void)overriddenInOverlayForA __attribute__((deprecated("Categories_A.h")));
- (void)overriddenInOverlayForB __attribute__((deprecated("Categories_A.h")));
- (void)overriddenInOverlayForC __attribute__((deprecated("Categories_A.h")));
- (void)overriddenInSubclassInOverlayForC __attribute__((deprecated("Categories_A.h")));
- (void)witnessesObjCConformanceRequirementInA __attribute__((deprecated("Categories_A.h")));
- (void)witnessesObjCConformanceRequirementInB __attribute__((deprecated("Categories_A.h")));
- (void)witnessesObjCConformanceRequirementInC __attribute__((deprecated("Categories_A.h")));
@end

@interface X : Base
@property (readonly, nonnull) NSObject *overriddenInCategoryWithMethodReturningOptional;
@end

@interface X (A)
- (void)fromA;
@end

@protocol ObjCProtoInA1
- (void)witnessesObjCConformanceRequirementInA __attribute__((deprecated("ObjCProtoInA1")));
@end

@interface Base () <ObjCProtoInA1>
@end

@protocol ObjCProtoInA2
@optional
- (void)optionalRequirementWitnessedInC;
@end
