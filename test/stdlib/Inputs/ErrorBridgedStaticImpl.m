#include "ErrorBridgedStaticImpl.h"

@implementation Foo

- (BOOL)foo:(int)x error:(NSError**)error {
  *error = nil;
  return NO;
}

- (BOOL)foothrows:(int)x error:(NSError**)error {
  *error = [NSError errorWithDomain: @"abcd" code: 1234 userInfo: nil];
  return NO;
}

@end

