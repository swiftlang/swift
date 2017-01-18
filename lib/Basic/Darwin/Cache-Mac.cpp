//===--- Cache-Mac.cpp - Caching mechanism implementation -----------------===//
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
//
//  This file implements the caching mechanism using darwin's libcache.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Cache.h"
#include "llvm/ADT/SmallString.h"
#include <cache.h>

using namespace swift::sys;
using llvm::StringRef;

CacheImpl::ImplTy CacheImpl::create(StringRef Name, const CallBacks &CBs) {
  llvm::SmallString<32> NameBuf(Name);
  cache_attributes_t Attrs = {
    CACHE_ATTRIBUTES_VERSION_2,
    CBs.keyHashCB,
    CBs.keyIsEqualCB,
    nullptr,
    CBs.keyDestroyCB,
    CBs.valueDestroyCB,
    nullptr,
    nullptr,
    CBs.UserData,
    nullptr
  };

  cache_t *cache_out = nullptr;
  cache_create(NameBuf.c_str(), &Attrs, &cache_out);
  assert(cache_out);
  return cache_out;
}

void CacheImpl::setAndRetain(void *Key, void *Value, size_t Cost) {
  cache_set_and_retain(static_cast<cache_t*>(Impl), Key, Value, Cost);
}

bool CacheImpl::getAndRetain(const void *Key, void **Value_out) {
  int Ret = cache_get_and_retain(static_cast<cache_t*>(Impl),
                                 const_cast<void*>(Key), Value_out);
  return Ret == 0;
}

void CacheImpl::releaseValue(void *Value) {
  cache_release_value(static_cast<cache_t*>(Impl), Value);
}

bool CacheImpl::remove(const void *Key) {
  int Ret = cache_remove(static_cast<cache_t*>(Impl), const_cast<void*>(Key));
  return Ret == 0;
}

void CacheImpl::removeAll() {
  cache_remove_all(static_cast<cache_t*>(Impl));
}

void CacheImpl::destroy() {
  cache_destroy(static_cast<cache_t*>(Impl));
}
