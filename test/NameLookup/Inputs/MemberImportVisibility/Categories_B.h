@import Categories_A;

@interface X (B)
- (void)fromB;
@end

@protocol ObjCProtoInB
- (void)witnessesObjCConformanceRequirementInB __attribute__((deprecated("ObjCProtoInB")));
@end

@interface Base () <ObjCProtoInB>
@end
