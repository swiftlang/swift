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
@end

#endif
