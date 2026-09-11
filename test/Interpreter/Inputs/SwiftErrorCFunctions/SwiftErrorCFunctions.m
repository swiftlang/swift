#import "SwiftErrorCFunctions.h"

static NSError *makeError(int code) {
  return [NSError errorWithDomain:@"TestDomain" code:code userInfo:nil];
}

_Bool c_error_zero(_Bool shouldFail, NSError **err) {
  if (shouldFail) {
    if (err) *err = makeError(1);
    return false;
  }
  return true;
}

int32_t c_error_nonzero(int32_t code, NSError **err) {
  if (code != 0) {
    if (err) *err = makeError(code);
    return code;
  }
  return 0;
}

void c_error_nonnull(_Bool shouldFail, NSError **err) {
  if (shouldFail) {
    if (err) *err = makeError(3);
  }
}

void * _Nullable c_error_null(_Bool shouldFail, NSError **err) {
  if (shouldFail) {
    if (err) *err = makeError(4);
    return NULL;
  }
  static int sentinel = 42;
  return &sentinel;
}

void * _Nullable c_error_cf_null(_Bool shouldFail,
    CFErrorRef *err CF_RETURNS_RETAINED) {
  if (shouldFail) {
    if (err) *err = (__bridge_retained CFErrorRef)makeError(5);
    return NULL;
  }
  static int sentinel = 99;
  return &sentinel;
}

void c_error_cf_nonnull(_Bool shouldFail,
    CFErrorRef *err CF_RETURNS_NOT_RETAINED) {
  if (shouldFail) {
    if (err) *err = (__bridge CFErrorRef)makeError(6);
  }
}

_Bool c_error_blocks_both_sides(_Bool shouldFail,
    void (^before)(int), NSError **err, void (^after)(int)) {
  before(10);
  after(20);
  if (shouldFail) {
    if (err) *err = makeError(7);
    return false;
  }
  return true;
}
