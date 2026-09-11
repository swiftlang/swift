#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>

// zero_result: _Bool return → throws, Void.
_Bool c_error_zero(_Bool shouldFail, NSError **err)
  __attribute__((swift_error(zero_result)));

// nonzero_result: int32_t return → throws, Void.
int32_t c_error_nonzero(int32_t code, NSError **err)
  __attribute__((swift_error(nonzero_result)));

// nonnull_error: void return → throws, Void.
void c_error_nonnull(_Bool shouldFail, NSError **err)
  __attribute__((swift_error(nonnull_error)));

// null_result: nullable pointer → throws, non-optional pointer.
void * _Nullable c_error_null(_Bool shouldFail, NSError **err)
  __attribute__((swift_error(null_result)));

// null_result with CFErrorRef*, CF_RETURNS_RETAINED.
void * _Nullable c_error_cf_null(_Bool shouldFail,
    CFErrorRef *err CF_RETURNS_RETAINED)
  __attribute__((swift_error(null_result)));

// nonnull_error with CFErrorRef*, CF_RETURNS_NOT_RETAINED.
void c_error_cf_nonnull(_Bool shouldFail,
    CFErrorRef *err CF_RETURNS_NOT_RETAINED)
  __attribute__((swift_error(nonnull_error)));

// zero_result with blocks on both sides of the error parameter.
// Exercises the parameter-index plumbing for non-trivial layouts.
_Bool c_error_blocks_both_sides(_Bool shouldFail,
    void (^before)(int), NSError **err, void (^after)(int))
  __attribute__((swift_error(zero_result)));
