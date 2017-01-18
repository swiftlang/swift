//===--- UIDRegistry.cpp --------------------------------------------------===//
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

#include "SourceKit/Support/UIdent.h"
#include "SourceKit/Support/Concurrency.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/raw_ostream.h"
#include <mutex>
#include <vector>

using namespace SourceKit;
using llvm::StringRef;

namespace {
class UIDRegistryImpl {
  typedef llvm::StringMap<void *, llvm::BumpPtrAllocator> HashTableTy;
  typedef llvm::StringMapEntry<void *> EntryTy;
  HashTableTy HashTable;
  WorkQueue Queue{ WorkQueue::Dequeuing::Concurrent, "UIDRegistryImpl" };

public:

  void *get(StringRef Str);
  static StringRef getName(void *Ptr);
  static void setTag(void *Ptr, void *Tag);
  static void *getTag(void *Ptr);
};
}

static UIDRegistryImpl *getGlobalRegistry() {
  static UIDRegistryImpl *GlobalRegistry = 0;
  if (!GlobalRegistry) {
    static std::once_flag flag;
    std::call_once(flag, [](){ GlobalRegistry = new UIDRegistryImpl(); });
  }
  return GlobalRegistry;
}

UIdent::UIdent(llvm::StringRef Str) {
  Ptr = getGlobalRegistry()->get(Str);
}

llvm::StringRef UIdent::getName() const {
  if (isInvalid())
    return StringRef();
  return UIDRegistryImpl::getName(Ptr);
}

const char *UIdent::c_str() const {
  if (isInvalid())
    return "";
  return getName().begin();
}

void UIdent::setTag(void *Tag) {
  assert(isValid());
  UIDRegistryImpl::setTag(Ptr, Tag);
}

void *UIdent::getTag() const {
  assert(isValid());
  return UIDRegistryImpl::getTag(Ptr);
}

void UIdent::dump() const {
  print(llvm::errs());
}

void UIdent::print(llvm::raw_ostream &OS) const {
  if (isInvalid())
    OS << "<<INVALID>>";
  else
    OS << getName();
}

void *UIDRegistryImpl::get(StringRef Str) {
  assert(!Str.empty());
  assert(Str.find(' ') == StringRef::npos);
  EntryTy *Ptr = 0;
  Queue.dispatchSync([&]{
    HashTableTy::iterator It = HashTable.find(Str);
    if (It != HashTable.end())
      Ptr = &(*It);
  });

  if (Ptr == 0) {
    Queue.dispatchBarrierSync([&]{
      EntryTy &Entry = *HashTable.insert(std::make_pair(Str, nullptr)).first;
      Ptr = &Entry;
    });
  }

  return Ptr;
}

StringRef UIDRegistryImpl::getName(void *Ptr) {
  EntryTy *Entry = static_cast<EntryTy*>(Ptr);
  return Entry->getKey();
}

void UIDRegistryImpl::setTag(void *Ptr, void *Tag) {
  EntryTy *Entry = static_cast<EntryTy*>(Ptr);
  Entry->setValue(Tag);
}

void *UIDRegistryImpl::getTag(void *Ptr) {
  EntryTy *Entry = static_cast<EntryTy*>(Ptr);
  return Entry->getValue();
}
