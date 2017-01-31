//===--- CodeCompletionCache.h - Routines for code completion caching -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODE_COMPLETIONCACHE_H
#define SWIFT_IDE_CODE_COMPLETIONCACHE_H

#include "swift/IDE/CodeCompletion.h"
#include "swift/Basic/ThreadSafeRefCounted.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/Chrono.h"
#include <system_error>

namespace swift {
namespace ide {

struct CodeCompletionCacheImpl;
class OnDiskCodeCompletionCache;

/// \brief In-memory per-module code completion result cache.
///
/// These results persist between multiple code completion requests and can be
/// used with different ASTContexts.
class CodeCompletionCache {
  std::unique_ptr<CodeCompletionCacheImpl> Impl;
  OnDiskCodeCompletionCache *nextCache;

public:
  /// \brief Cache key.
  struct Key {
    std::string ModuleFilename;
    std::string ModuleName;
    std::vector<std::string> AccessPath;
    bool ResultsHaveLeadingDot;
    bool ForTestableLookup;
    bool CodeCompleteInitsInPostfixExpr;

    friend bool operator==(const Key &LHS, const Key &RHS) {
      return LHS.ModuleFilename == RHS.ModuleFilename &&
        LHS.ModuleName == RHS.ModuleName &&
        LHS.AccessPath == RHS.AccessPath &&
        LHS.ResultsHaveLeadingDot == RHS.ResultsHaveLeadingDot &&
        LHS.ForTestableLookup == RHS.ForTestableLookup &&
        LHS.CodeCompleteInitsInPostfixExpr == RHS.CodeCompleteInitsInPostfixExpr;
    }
  };

  struct Value : public llvm::ThreadSafeRefCountedBase<Value> {
    llvm::sys::TimePoint<> ModuleModificationTime;
    CodeCompletionResultSink Sink;
  };
  using ValueRefCntPtr = llvm::IntrusiveRefCntPtr<Value>;

  CodeCompletionCache(OnDiskCodeCompletionCache *nextCache = nullptr);
  ~CodeCompletionCache();

  static ValueRefCntPtr createValue();
  Optional<ValueRefCntPtr> get(const Key &K);
  void set(const Key &K, ValueRefCntPtr V) { setImpl(K, V, /*setChain*/ true); }

private:
  void setImpl(const Key &K, ValueRefCntPtr V, bool setChain);
};

/// \brief On-disk per-module code completion result cache.
///
/// These results persist between multiple code completion requests and can be
/// used with different ASTContexts.
class OnDiskCodeCompletionCache {
  std::string cacheDirectory;

public:
  using Key = CodeCompletionCache::Key;
  using Value = CodeCompletionCache::Value;
  using ValueRefCntPtr = CodeCompletionCache::ValueRefCntPtr;

  OnDiskCodeCompletionCache(Twine cacheDirectory);
  ~OnDiskCodeCompletionCache();

  Optional<ValueRefCntPtr> get(const Key &K);
  std::error_code set(const Key &K, ValueRefCntPtr V);

  static Optional<ValueRefCntPtr> getFromFile(StringRef filename);
};

struct RequestedCachedModule {
  CodeCompletionCache::Key Key;
  const ModuleDecl *TheModule;
  bool OnlyTypes;
};

} // end namespace ide
} // end namespace swift

namespace llvm {
template<>
struct DenseMapInfo<swift::ide::CodeCompletionCache::Key> {
  using KeyTy = swift::ide::CodeCompletionCache::Key;
  static inline KeyTy getEmptyKey() {
    return KeyTy{"", "", {}, false, false, false};
  }
  static inline KeyTy getTombstoneKey() {
    return KeyTy{"", "", {}, true, false, false};
  }
  static unsigned getHashValue(const KeyTy &Val) {
    size_t H = 0;
    H ^= std::hash<std::string>()(Val.ModuleFilename);
    H ^= std::hash<std::string>()(Val.ModuleName);
    for (auto Piece : Val.AccessPath)
      H ^= std::hash<std::string>()(Piece);
    H ^= std::hash<bool>()(Val.ResultsHaveLeadingDot);
    H ^= std::hash<bool>()(Val.ForTestableLookup);
    return static_cast<unsigned>(H);
  }
  static bool isEqual(const KeyTy &LHS, const KeyTy &RHS) {
    return LHS == RHS;
  }
};
} // end namespace llvm

#endif // SWIFT_IDE_CODE_COMPLETIONCACHE_H
