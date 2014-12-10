#ifndef NULLABILITY_H
#define NULLABILITY_H

typedef __nonnull id nonnull_id;

__nullable id getId1(void);
__nullable nonnull_id getId2(void);

extern __nullable id global_id;

@interface SomeClass
- (nonnull id)methodA:(nullable SomeClass *)obj;
- (nonnull nonnull_id)methodB:(nullable int (^)(int, int))block;
- (nullable nonnull_id)methodC;
@property (nullable) nonnull_id property;
- (id)methodD __attribute__((returns_nonnull));
- (void)methodE:(SomeClass *) __attribute__((nonnull)) obj;
- (void)methodF:(SomeClass *)obj second:(SomeClass *)obj2 __attribute__((nonnull));
- (void)methodG:(SomeClass *)obj second:(SomeClass *)obj2 __attribute__((nonnull(1)));

+(nonnull instancetype)someClassWithInt:(int)x;
+(nullable SomeClass*)someClassWithDouble:(double)x;
-(nonnull instancetype)returnMe;
@end

#endif
