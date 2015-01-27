#ifndef NULLABILITY_H
#define NULLABILITY_H

@import Foundation;

__nullable id getId1(void);

extern __nullable id global_id;

@interface SomeClass
- (nonnull id)methodA:(nullable SomeClass *)obj;
- (nonnull id)methodB:(nullable int (^)(int, int))block;
- (nullable id)methodC;
@property (nullable) id property;
- (id)methodD __attribute__((returns_nonnull));
- (void)methodE:(SomeClass *) __attribute__((nonnull)) obj;
- (void)methodF:(SomeClass *)obj second:(SomeClass *)obj2 __attribute__((nonnull));
- (void)methodG:(SomeClass *)obj second:(SomeClass *)obj2 __attribute__((nonnull(1)));
-(nonnull NSString *)stringMethod;
-(nullable NSArray *)optArrayMethod;

+(nonnull instancetype)someClassWithInt:(int)x;
+(nullable SomeClass*)someClassWithDouble:(double)x;
-(nonnull instancetype)returnMe;
@end

#define NON_NULL_MACRO(...) __attribute__ ((nonnull(__VA_ARGS__)))

void compare_classes(SomeClass *sc1, SomeClass *sc2, SomeClass *sc3) NON_NULL_MACRO(1,2);

#endif
