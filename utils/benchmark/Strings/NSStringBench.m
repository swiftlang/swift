#import <Foundation/Foundation.h>
#include <mach/mach_time.h>
#include <stdio.h>

// NOTE: compile with ARC enabled

#define LAPS 10000000

int
main(void) {
  NSString *s = @"siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig";

  uint64_t count = 0;
  uint64_t start = mach_absolute_time();
  for (unsigned i = 0; i < LAPS; i++) {
    for (NSUInteger j = 0, sLen = [s length]; j < sLen; j++) {
      [s characterAtIndex: j];
      count++;
    }
  }
  uint64_t delta = mach_absolute_time() - start;
  printf("characterAtIndex: %f ns/lap\n", (double)delta / (double)LAPS);

  NSString *url = @"http://www.apple.com/iphone-5s/built-in-apps/";
  start = mach_absolute_time();
  for (unsigned i = 0; i < LAPS; i++) {
    NSString *proto = nil;
    NSString *rest = nil;
    for (NSUInteger j = 0, sLen = [s length]; j < sLen; j++) {
      if ([s characterAtIndex: j] == ':') {
        proto = [url substringToIndex: j];
        rest = [url substringFromIndex: j + 1];
        break;
      }
    }
  }
  delta = mach_absolute_time() - start;
  printf("URL parsing: %f ns/lap\n", (double)delta / (double)LAPS);

  return 0;
}
