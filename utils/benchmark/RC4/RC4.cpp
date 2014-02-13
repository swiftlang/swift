#include <vector>
#include <iostream>

// Turn off logging if we only want to time the routine.
#ifndef LOG
#define LOG(x) x
#else
#undef LOG
#define LOG(x)
#endif

extern "C" {
#include <mach/mach_time.h>
}

struct RC4 {
  std::vector<uint8_t> State;
  int I;
  int J;

  RC4() { I = 0; J = 0; }

  void initialize(std::vector<uint8_t> &Key) {
    State.clear();
    for (int i = 0; i < 256; i++)
      State.push_back(i);

    int j = 0;
    for (int i=0; i < 256; i++) {
      uint8_t K = Key[i % Key.size()];
      uint8_t S = State[i];
      j = (j + S + K) %256;
      swapByIndex(i, j);
    }
  }

  void swapByIndex(int x, int y) {
    uint8_t T1 = State[x];
    uint8_t T2 = State[y];
    State[x] = T2;
    State[y] = T1;
  }

  uint8_t next() {
    I = (I + 1) % 256;
    J = (J + State[I]) % 256;
    swapByIndex(I, J);
    return State[(State[I] + State[J]) & 0xFF];
  }

  void encrypt(std::vector<uint8_t> &Data) {
    int cnt = Data.size();
    for (int i = 0; i < cnt; i++)
      Data[i] = Data[i] ^ next();
  }
};

void benchRC4(int messageLen, int iterations, bool validate) {
  char *Secret = "This is my secret message";
  char *Key    = "This is my key";

  std::vector<uint8_t> SecretData(&Secret[0], &Secret[strlen(Secret)]);
  std::vector<uint8_t> KeyData(&Key[0], &Key[strlen(Key)]);

  std::vector<uint8_t> LongData(messageLen, 0);

  LOG(std::cout << "Generating data ... " << std::endl);

  for (int i = 0; i < messageLen ; i++)
    LongData[i] = SecretData[i % SecretData.size()];

  RC4 Enc;
  RC4 Dec;
  Enc.initialize(KeyData);
  Dec.initialize(KeyData);

  LOG(std::cout << "Starting benchmark..." << std::endl);
  uint64_t start = mach_absolute_time();
  for (int i=0; i < iterations; i++) {
    Enc.encrypt(LongData);
    Dec.encrypt(LongData);
  }
  uint64_t end = mach_absolute_time() - start;
  printf("%llu nanoseconds.\n", end);

  if (validate) {
    LOG(std::cout << "Validating..." << std::endl);
    for (int i = 0; i < messageLen ; i++)
      if (LongData[i] != SecretData[i % SecretData.size()]) {
        LOG(std::cout << "Error at " << i << "!!!" << std::endl);
      }
  }
  LOG(std::cout << "done" << std::endl);
}

int main() {
  benchRC4(5000, 2000, false);
}
