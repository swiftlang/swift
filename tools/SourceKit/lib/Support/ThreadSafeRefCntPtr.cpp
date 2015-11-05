#include "SourceKit/Support/ThreadSafeRefCntPtr.h"
#include <functional>

using namespace SourceKit;
using llvm::sys::Mutex;

static const size_t MtxCount = 16;
static Mutex Mtxs[MtxCount];

Mutex *ThreadSafeRefCntPtrImpl::getMutex(void *Ptr) {
  return Mtxs + (std::hash<const void*>()(Ptr) & (MtxCount-1));
}
