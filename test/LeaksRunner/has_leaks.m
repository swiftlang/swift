// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %clang -framework Foundation %s -o %t/has_leaks
// RUN: not %leaks-runner %t/has_leaks
// REQUIRES: leaks

#include <Foundation/Foundation.h>

@interface T1 : NSObject
@end

@implementation T1
@end

int main(int argc, char *argv[]) {
  for (unsigned i = 0; i < 10; i++) {
    [[T1 alloc] init];
  }
  return 0;
}
