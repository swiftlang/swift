#include <mach/mach_time.h>
#include <string>
#include <stdio.h>

#define LAPS 10000000

std::wstring s = L"siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig";

int
main(void) {
  uint64_t count = 0;
  uint64_t start = mach_absolute_time();
  for (auto i = 0; i < LAPS; i++) {
    for (auto c : s) {
      count++;
      asm volatile("" : "+r" (count));
    }
  }
  uint64_t delta = mach_absolute_time() - start;
  printf("%f nanoseconds\n", (double)delta);
  printf("%f ns/lap\n", (double)delta / (double)LAPS);

  return 0;
}
