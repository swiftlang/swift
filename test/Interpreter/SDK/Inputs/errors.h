@import Foundation;

#pragma clang assume_nonnull begin

@protocol ErrorTest
- (nullable id)succeedAndReturnError:(NSError **)error;
- (nullable id)failAndReturnError:(NSError **)error;
@end

static id _Nullable testSucceed(id<ErrorTest> _Nonnull testObj) {
  NSError *error = nil;
  return [testObj succeedAndReturnError:&error];
}

static id _Nullable testSucceedIgnoringError(id<ErrorTest> _Nonnull testObj) {
  return [testObj succeedAndReturnError:NULL];
}

static id _Nullable testFail(id<ErrorTest> _Nonnull testObj) {
  NSError *error = nil;
  return [testObj failAndReturnError:&error];
}

static id _Nullable testFailIgnoringError(id<ErrorTest> _Nonnull testObj) {
  return [testObj failAndReturnError:NULL];
}

#pragma clang assume_nonnull end
