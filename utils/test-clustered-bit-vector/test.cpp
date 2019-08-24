#include "swift/Basic/ClusteredBitVector.h"
using namespace swift;
int main() {
  ClusteredBitVector cbvs[16];
  cbvs[7].appendSetBits(65);
  cbvs[3].appendClearBits(118);
  cbvs[7] = std::move(cbvs[3]);
  assert(!cbvs[7][64]);
}
