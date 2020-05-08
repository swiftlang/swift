//===- StringSet.h - An efficient set built on StringMap --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  StringSet - A set-like wrapper for the StringMap.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_STRINGSET_H
#define LLVM_ADT_STRINGSET_H

#include "llvm/ADT/StringMap.h"

namespace llvm {

/// StringSet - A wrapper for StringMap that provides set-like functionality.
template <class AllocatorTy = MallocAllocator>
class StringSet : public StringMap<NoneType, AllocatorTy> {
  using Base = StringMap<NoneType, AllocatorTy>;

public:
  StringSet() = default;
  StringSet(std::initializer_list<StringRef> initializer) {
    for (StringRef str : initializer)
      insert(str);
  }
  explicit StringSet(AllocatorTy a) : Base(a) {}

  std::pair<typename Base::iterator, bool> insert(StringRef key) {
    return Base::insert(std::make_pair(key, None));
  }

  template <typename InputIt>
  void insert(const InputIt &begin, const InputIt &end) {
    for (auto it = begin; it != end; ++it)
      Base::insert(std::make_pair(*it, None));
  }

  template <typename ValueTy>
  std::pair<typename Base::iterator, bool>
  insert(const StringMapEntry<ValueTy> &mapEntry) {
    return insert(mapEntry.getKey());
  }
};

} // end namespace llvm

#endif // LLVM_ADT_STRINGSET_H
