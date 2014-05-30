// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %clang -framework Foundation %s -o %t/no_leaks
// RUN: %leaks-runner %t/no_leaks
// REQUIRES: leaks

#include <Foundation/Foundation.h>

@interface T1 : NSObject
@end

@implementation T1
@end

int main(int argc, char *argv[]) {
  for (unsigned i = 0; i < 10; i++) {
    T1 *T = [[T1 alloc] init];
    [T release];
  }
  return 0;
}
