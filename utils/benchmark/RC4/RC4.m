#import <Foundation/Foundation.h>
#include <mach/mach_time.h>
#include <stdint.h>
#include "RC4.h"

@implementation RC4

- (id) init {
  return [self initWithKey:nil length:0];
}

- (id)initWithKey:(const unsigned char *)key length:(unsigned)len {
  if (self = [super init]) {
    _i = 0;
    _j = 0;

    _state = (uint8_t *) calloc(256, sizeof(uint8_t));

    // Allocation failed.
    if (!_state)
      return self;

    if (key) {
      for (unsigned i = 0; i < 256; ++i)
        _state[i] = i;

      uint8_t j = 0;
      for (unsigned i = 0; i < 256; i++) {
        uint8_t k = key[i % len];
        uint8_t s = _state[i];
        j = j + s + k;
        [self swapByIndex: i andIndex: j];
      }
    } else {
      for (unsigned i = 0; i < 256; ++i)
        _state[i] = 0;
    }
  }

  return self;
}

- (void) dealloc {
  free(_state);
}

- (void) swapByIndex:(unsigned)x andIndex:(unsigned)y {
  uint8_t t1 = _state[x];
  uint8_t t2 = _state[y];

  _state[x] = t2;
  _state[y] = t1;
}

- (unsigned char) next {
  _i = _i + 1;
  _j = _j + _state[_i];

  [self swapByIndex:_i andIndex:_j];

  return _state[(_state[_i] + _state[_j]) & 0xFF];
}

- (void) encrypt: (unsigned char *)data length:(unsigned)len {
  for (int i = 0; i < len; i++)
    data[i] = data[i] ^ [self next];
}

@end

void benchRC4(int messageLen, int numIterations, bool validate) {
  const char *Secret = "This is my secret message";
  unsigned SecretLen = strlen(Secret);
  const char *Key    = "This is my key";
  unsigned KeyLen = strlen(Key);

  unsigned char *LongData = calloc(messageLen, sizeof(unsigned char));
  for (int i = 0; i < messageLen ; i++) {
    LongData[i] = (unsigned char)Secret[i % SecretLen];
  }

  RC4 *Enc = [[RC4 alloc] initWithKey:(unsigned char*)Key length:KeyLen];

  uint64_t start = mach_absolute_time();

  for (int i=0; i < numIterations; i++) {
    [Enc encrypt:LongData length:messageLen];
  }

  uint64_t end = mach_absolute_time() - start;

  printf("%llu nanoseconds.\n", end);
  printf("%lf nanoseconds.\n", end/(double)numIterations);

  free(LongData);
}

int main(int argc, char **argv) {
  benchRC4(5000, 100000, false);
  return 0;
}
