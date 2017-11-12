#include "ErrorBridgedStaticImpl.h"

@implementation Foo

- (BOOL)foo:(int)x error:(NSError**)error {
  *error = nil;
  return NO;
}

@end

