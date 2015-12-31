//===--- CompactArray.cpp -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "sourcekitd/CompactArray.h"
#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/UIdent.h"
#include "llvm/Support/MemoryBuffer.h"
#include <limits.h>

using namespace SourceKit;
using namespace sourcekitd;

template <typename T>
static void addScalar(T Val, SmallVectorImpl<uint8_t> &Buf) {
  const uint8_t *ValPtr = reinterpret_cast<const uint8_t *>(&Val);
  Buf.append(ValPtr, ValPtr + sizeof(Val));
}

CompactArrayBuilderImpl::CompactArrayBuilderImpl() {
  // Offset 0 is used for empty strings.
  StringBuffer += '\0';
}

void CompactArrayBuilderImpl::addImpl(uint8_t Val) {
  addScalar(Val, EntriesBuffer);
}

void CompactArrayBuilderImpl::addImpl(unsigned Val) {
  addScalar(Val, EntriesBuffer);
}

void CompactArrayBuilderImpl::addImpl(StringRef Val) {
  addScalar(getOffsetForString(Val), EntriesBuffer);
}

void CompactArrayBuilderImpl::addImpl(UIdent Val) {
  sourcekitd_uid_t uid = SKDUIDFromUIdent(Val);
  addScalar(uid, EntriesBuffer);
}

void CompactArrayBuilderImpl::addImpl(Optional<llvm::StringRef> Val) {
  if (Val.hasValue()) {
    addImpl(Val.getValue());
  } else {
    addImpl(unsigned(-1));
  }
}

unsigned CompactArrayBuilderImpl::getOffsetForString(StringRef Str) {
  if (Str.empty())
    return 0;

  unsigned Offset = StringBuffer.size();
  StringBuffer += Str;
  StringBuffer += '\0';
  return Offset;
}

std::unique_ptr<llvm::MemoryBuffer>
CompactArrayBuilderImpl::createBuffer() const {
  size_t EntriesBufSize = EntriesBuffer.size();
  std::unique_ptr<llvm::MemoryBuffer> Buf;
  Buf = llvm::MemoryBuffer::getNewUninitMemBuffer(
      sizeof(uint64_t) +
      EntriesBufSize +
      StringBuffer.size());

  char *BufPtr = (char*)Buf->getBufferStart();
  *reinterpret_cast<uint64_t*>(BufPtr) = EntriesBufSize;
  BufPtr += sizeof(uint64_t);
  memcpy(BufPtr, EntriesBuffer.data(), EntriesBufSize);
  BufPtr += EntriesBufSize;
  memcpy(BufPtr, StringBuffer.data(), StringBuffer.size());

  return Buf;
}

bool CompactArrayBuilderImpl::empty() const {
  return EntriesBuffer.empty();
}

template <typename T>
static void readScalar(const uint8_t *Buf, T &Val) {
  memcpy(&Val, Buf, sizeof(Val));
}

void CompactArrayReaderImpl::readImpl(size_t Offset, uint8_t &Val) {
  readScalar(getEntriesBufStart() + Offset, Val);
}

void CompactArrayReaderImpl::readImpl(size_t Offset, unsigned &Val) {
  readScalar(getEntriesBufStart() + Offset, Val);
}

void CompactArrayReaderImpl::readImpl(size_t Offset, const char * &Val) {
  unsigned StrOffset;
  readScalar(getEntriesBufStart() + Offset, StrOffset);
  if (StrOffset == unsigned(-1)) {
    Val = nullptr;
    return;
  }
  Val = getStringBufStart() + StrOffset;
}

void CompactArrayReaderImpl::readImpl(size_t Offset, sourcekitd_uid_t &Val) {
  readScalar(getEntriesBufStart() + Offset, Val);
}
