//===--- CompactArray.cpp -------------------------------------------------===//
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
  if (Val.isValid()) {
    sourcekitd_uid_t uid = SKDUIDFromUIdent(Val);
    addScalar(uid, EntriesBuffer);
  } else {
    addScalar(sourcekitd_uid_t(nullptr), EntriesBuffer);
  }
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
  std::unique_ptr<llvm::WritableMemoryBuffer> Buf;
  Buf = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(sizeInBytes());
  copyInto(Buf->getBufferStart(), Buf->getBufferSize());
  return std::move(Buf);
}

void CompactArrayBuilderImpl::appendTo(SmallVectorImpl<char> &Buf) const {
  size_t OrigSize = Buf.size();
  size_t NewSize = OrigSize + sizeInBytes();
  Buf.resize(NewSize);
  copyInto(Buf.data() + OrigSize, NewSize - OrigSize);
}

void CompactArrayBuilderImpl::copyInto(char *BufPtr, size_t Length) const {
  uint64_t EntriesBufSize = EntriesBuffer.size();
  assert(Length >= sizeInBytes());
  memcpy(BufPtr, &EntriesBufSize, sizeof(EntriesBufSize));
  BufPtr += sizeof(EntriesBufSize);
  memcpy(BufPtr, EntriesBuffer.data(), EntriesBufSize);
  BufPtr += EntriesBufSize;
  memcpy(BufPtr, StringBuffer.data(), StringBuffer.size());
}

bool CompactArrayBuilderImpl::empty() const {
  return EntriesBuffer.empty();
}

size_t CompactArrayBuilderImpl::sizeInBytes() const {
  return sizeof(uint64_t) + EntriesBuffer.size() + StringBuffer.size();
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
