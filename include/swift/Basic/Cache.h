//===--- Cache.h - Caching mechanism interface ------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_CACHE_H
#define SWIFT_BASIC_CACHE_H

#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Optional.h"

namespace swift {
namespace sys {

template <typename T>
struct CacheKeyHashInfo {
  static uintptr_t getHashValue(const T &Val) {
    return llvm::DenseMapInfo<T>::getHashValue(Val);
  }
  static bool isEqual(void *LHS, void *RHS) {
    return llvm::DenseMapInfo<T>::isEqual(*static_cast<T*>(LHS),
                                          *static_cast<T*>(RHS));
  }
};

template <typename T>
struct CacheKeyInfo : public CacheKeyHashInfo<T> {
  static void *enterCache(const T &Val) { return new T(Val); }
  static void exitCache(void *Ptr) { delete static_cast<T*>(Ptr); }
  static const void *getLookupKey(const T *Val) { return Val; }
  static const T &getFromCache(void *Ptr) { return *static_cast<T*>(Ptr); }
};

template <typename T>
struct CacheValueCostInfo {
  static size_t getCost(const T &Val) { return sizeof(Val); }
};

template <typename T>
struct CacheValueInfo : public CacheValueCostInfo<T> {
  static void *enterCache(const T &Val) { return new T(Val); }
  static void retain(void *Ptr) {}
  static void release(void *Ptr) { delete static_cast<T *>(Ptr); }
  static const T &getFromCache(void *Ptr) { return *static_cast<T *>(Ptr); }
};

/// The underlying implementation of the caching mechanism.
/// It should be inherently thread-safe.
class CacheImpl {
public:
  using ImplTy = void *;

  struct CallBacks {
    void *UserData;

    uintptr_t (*keyHashCB)(void *Key, void *UserData);
    bool (*keyIsEqualCB)(void *Key1, void *Key2, void *UserData);

    void (*keyDestroyCB)(void *Key, void *UserData);
    void (*valueRetainCB)(void *Value, void *UserData);
    void (*valueReleaseCB)(void *Value, void *UserData);
  };

protected:
  CacheImpl() = default;

  ImplTy Impl = nullptr;

  static ImplTy create(llvm::StringRef Name, const CallBacks &CBs);

  /// Sets value for key.
  ///
  /// \param Key Key to add.  Must not be nullptr.
  /// \param Value Value to add. If value is nullptr, key is associated with the
  /// value nullptr.
  /// \param Cost Cost of maintaining value in cache.
  ///
  /// Sets value for key.  Value is retained until released using
  /// \c releaseValue().
  ///
  /// Replaces previous key and value if present.  Invokes the key destroy
  /// callback immediately for the previous key.  Invokes the value destroy
  /// callback once the previous value's retain count is zero.
  ///
  /// Cost indicates the relative cost of maintaining value in the cache
  /// (e.g., size of value in bytes) and may be used by the cache under
  /// memory pressure to select which cache values to evict.  Zero is a
  /// valid cost.
  void setAndRetain(void *Key, void *Value, size_t Cost);

  /// Fetches value for key.
  ///
  /// \param Key Key used to lookup value.  Must not be nullptr.
  /// \param Value_out Value is stored here if found.  Must not be nullptr.
  /// \returns True if the key was found, false otherwise.
  ///
  /// Fetches value for key, retains value, and stores value in value_out.
  /// Caller should release value using \c releaseValue().
  bool getAndRetain(const void *Key, void **Value_out);

  /// Releases a previously retained cache value.
  ///
  /// \param Value Value to release.  Must not be nullptr.
  ///
  /// Releases a previously retained cache value. When the reference count
  /// reaches zero the cache may destroy the value.
  void releaseValue(void *Value);

  /// Removes a key and its value.
  ///
  /// \param Key Key to remove.  Must not be nullptr.
  /// \returns True if the key was found, false otherwise.
  ///
  /// Removes a key and its value from the cache such that \c getAndRetain()
  /// will return false.  Invokes the key destroy callback immediately.
  /// Invokes the value destroy callback once value's retain count is zero.
  bool remove(const void *Key);

  /// Invokes \c remove on all keys.
  void removeAll();

  /// Destroys cache.
  void destroy();
};

/// Caching mechanism, that is thread-safe and can evict its entries when there
/// is memory pressure.
///
/// This works like a dictionary, you use a key to store and retrieve a value.
/// The value is copied (during storing or retrieval), but an IntrusiveRefCntPtr
/// can be used directly as a value.
///
/// It is important to provide a proper 'cost' function for the value (via
/// \c CacheValueCostInfo trait); e.g. the cost for an ASTContext would be the
/// memory usage of the data structures it owns.
template <typename KeyT, typename ValueT,
          typename KeyInfoT = CacheKeyInfo<KeyT>,
          typename ValueInfoT = CacheValueInfo<ValueT> >
class Cache : CacheImpl {
public:
  explicit Cache(llvm::StringRef Name) {
    CallBacks CBs = {
      /*UserData=*/nullptr,
      keyHash,
      keyIsEqual,
      keyDestroy,
      valueRetain,
      valueRelease,
    };
    Impl = create(Name, CBs);
  }

  ~Cache() {
    destroy();
  }

  void set(const KeyT &Key, const ValueT &Value) {
    void *CacheKeyPtr = KeyInfoT::enterCache(Key);
    void *CacheValuePtr = ValueInfoT::enterCache(Value);
    setAndRetain(CacheKeyPtr, CacheValuePtr, ValueInfoT::getCost(Value));
    releaseValue(CacheValuePtr);
  }

  llvm::Optional<ValueT> get(const KeyT &Key) {
    const void *CacheKeyPtr = KeyInfoT::getLookupKey(&Key);
    void *CacheValuePtr;
    bool Found = getAndRetain(CacheKeyPtr, &CacheValuePtr);
    if (!Found)
      return llvm::None;

    ValueT Val(ValueInfoT::getFromCache(CacheValuePtr));
    releaseValue(CacheValuePtr);
    return std::move(Val);
  }

  /// \returns True if the key was found, false otherwise.
  bool remove(const KeyT &Key) {
    const void *CacheKeyPtr = KeyInfoT::getLookupKey(&Key);
    return CacheImpl::remove(CacheKeyPtr);
  }

  void clear() {
    removeAll();
  }

private:
  static uintptr_t keyHash(void *Key, void *UserData) {
    return KeyInfoT::getHashValue(*static_cast<KeyT*>(Key));
  }
  static bool keyIsEqual(void *Key1, void *Key2, void *UserData) {
    return KeyInfoT::isEqual(Key1, Key2);
  }

  static void keyDestroy(void *Key, void *UserData) {
    KeyInfoT::exitCache(Key);
  }
  static void valueRetain(void *Value, void *UserData) {
    ValueInfoT::retain(Value);
  }
  static void valueRelease(void *Value, void *UserData) {
    ValueInfoT::release(Value);
  }
};

template <typename T>
struct CacheValueInfo<llvm::IntrusiveRefCntPtr<T>>{
  static void *enterCache(const llvm::IntrusiveRefCntPtr<T> &Val) {
    return const_cast<T *>(Val.get());
  }
  static void retain(void *Ptr) {
    static_cast<T*>(Ptr)->Retain();
  }
  static void release(void *Ptr) {
    static_cast<T*>(Ptr)->Release();
  }
  static llvm::IntrusiveRefCntPtr<T> getFromCache(void *Ptr) {
    return static_cast<T*>(Ptr);
  }
  static size_t getCost(const llvm::IntrusiveRefCntPtr<T> &Val) {
    return CacheValueCostInfo<T>::getCost(*Val);
  }
};

} // end namespace sys
} // end namespace swift

#endif // SWIFT_BASIC_CACHE_H
