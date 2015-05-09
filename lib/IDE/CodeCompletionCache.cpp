#include "swift/IDE/CodeCompletionCache.h"
#include "swift/Basic/Cache.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;
using namespace ide;

namespace swift {
  namespace ide {
    struct CodeCompletionCacheImpl {
      using Key = CodeCompletionCache::Key;
      using Value = CodeCompletionCache::Value;
      using ValueRefCntPtr = CodeCompletionCache::ValueRefCntPtr;
      sys::Cache<Key, ValueRefCntPtr> TheCache{"swift.libIDE.CodeCompletionCache"};
    };
  } // end namespace ide
} // end namespace swift

namespace swift {
  namespace sys {
    template<>
    struct CacheValueCostInfo<swift::ide::CodeCompletionCacheImpl::Value> {
      static size_t
      getCost(const swift::ide::CodeCompletionCacheImpl::Value &V) {
        return V.Sink.Allocator->getTotalMemory();
      }
    };
  } // namespace sys
} // namespace swift

CodeCompletionCache::ValueRefCntPtr CodeCompletionCache::createValue() {
  return ValueRefCntPtr(new Value);
}

Optional<CodeCompletionCache::ValueRefCntPtr>
CodeCompletionCache::get(const Key &K) {
  auto &TheCache = Impl->TheCache;
  llvm::Optional<ValueRefCntPtr> V = TheCache.get(K);
  if (V) {
    // Check whether V is up to date.
    llvm::sys::fs::file_status ModuleStatus;
    if (llvm::sys::fs::status(K.ModuleFilename, ModuleStatus) ||
        V.getValue()->ModuleModificationTime !=
        ModuleStatus.getLastModificationTime()) {
      // Cache is stale.
      V = None;
      TheCache.remove(K);
    }
  }
  return V;
}

void CodeCompletionCache::set(const Key &K, ValueRefCntPtr V) {
  {
    assert(!K.ModuleFilename.empty());

    llvm::sys::fs::file_status ModuleStatus;
    if (llvm::sys::fs::status(K.ModuleFilename, ModuleStatus)) {
      V->ModuleModificationTime = llvm::sys::TimeValue::now();
    } else {
      V->ModuleModificationTime = ModuleStatus.getLastModificationTime();
    }
  }
  Impl->TheCache.set(K, V);
}

CodeCompletionCache::CodeCompletionCache()
: Impl(new CodeCompletionCacheImpl()) {}

CodeCompletionCache::~CodeCompletionCache() {}
