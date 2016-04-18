@import Foundation;

#pragma clang assume_nonnull begin

@protocol ErrorTest
- (nullable id)succeedAndReturnError:(NSError **)error;
- (nullable id)failAndReturnError:(NSError **)error;
@end

static id __nullable testSucceed(id <ErrorTest> __nonnull testObj) {
  NSError *error = nil;
  return [testObj succeedAndReturnError:&error];
}

static id __nullable testSucceedIgnoringError(id <ErrorTest> __nonnull testObj) {
  return [testObj succeedAndReturnError:NULL];
}

static id __nullable testFail(id <ErrorTest> __nonnull testObj) {
  NSError *error = nil;
  return [testObj failAndReturnError:&error];
}

static id __nullable testFailIgnoringError(id <ErrorTest> __nonnull testObj) {
  return [testObj failAndReturnError:NULL];
}

#pragma clang assume_nonnull end
