#import <Foundation/Foundation.h>
#include <mach/mach_time.h>
#include <stdio.h>

#define LAPS 10000000

int
main(void) {
  NSString *s = @"siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig";

  uint64_t count = 0;
  uint64_t start = mach_absolute_time();
  for (unsigned i = 0; i < LAPS; i++) {
    NSUInteger sLen = [s length];
    for (NSUInteger j = 0; j < sLen; j++) {
      [s characterAtIndex: j];
      count++;
    }
  }
  uint64_t delta = mach_absolute_time() - start;
  printf("%f ns/lap\n", (double)delta / (double)LAPS);

  return 0;
}
