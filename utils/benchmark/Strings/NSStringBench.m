#import <Foundation/Foundation.h>
#include <mach/mach_time.h>
#include <stdio.h>

// NOTE: compile with ARC enabled
// clang -fobjc-arc -O3 NSStringBench.m -o NSStringBench.bin

#define LAPS 10000

int
main(void) {

   NSString *complex = @"निरन्तरान्धकारिता-दिगन्तर-कन्दलदमन्द-सुधारस-बिन्दु-सान्द्रतर-घनाघन-वृन्द-सन्देहकर-स्यन्दमान-मकरन्द-बिन्दु-बन्धुरतर-माकन्द-तरु-कुल-तल्प-कल्प-मृदुल-सिकता-जाल-जटिल-मूल-तल-मरुवक-मिलदलघु-लघु-लय-कलित-रमणीय-पानीय-शालिका-बालिका-करार-विन्द-गलन्तिका-गलदेला-लवङ्ग-पाटल-घनसार-कस्तूरिकातिसौरभ-मेदुर-लघुतर-मधुर-शीतलतर-सलिलधारा-निराकरिष्णु-तदीय-विमल-विलोचन-मयूख-रेखापसारित-पिपासायास-पथिक-लोकान्";

  NSString *s = @"siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig";
  //s = complex;
  uint64_t count = 0;
  uint64_t start = mach_absolute_time();
  for (unsigned i = 0; i < LAPS; i++) {
    for (NSUInteger j = 0, sLen = [s length]; j < sLen; j++) {
      [s characterAtIndex: j];
      count++;
    }
  }
  uint64_t delta = mach_absolute_time() - start;
  printf("%f ns\n", (double)delta);
  printf("%f ns/lap\n", (double)delta / (double)LAPS);

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
