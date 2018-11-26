//===--- ImmutableTextBuffer.cpp ------------------------------------------===//
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

#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "clang/Rewrite/Core/RewriteRope.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using namespace SourceKit;
using namespace llvm;
using clang::RewriteRope;

void ImmutableTextUpdate::anchor() {}

ImmutableTextBuffer::ImmutableTextBuffer(
    std::unique_ptr<llvm::MemoryBuffer> MemBuf, uint64_t Stamp)
  : ImmutableTextUpdate(Kind::Buffer, Stamp) {
    SrcMgr.reset(new SourceMgr);
    BufId = SrcMgr->AddNewSourceBuffer(std::move(MemBuf), SMLoc());
  }

ImmutableTextBuffer::ImmutableTextBuffer(StringRef Filename, StringRef Text,
                                         uint64_t Stamp)
  : ImmutableTextBuffer(
      std::unique_ptr<llvm::MemoryBuffer>(
        MemoryBuffer::getMemBufferCopy(Text, Filename)),
      Stamp) {
}

StringRef ImmutableTextBuffer::getFilename() const {
  return SrcMgr->getMemoryBuffer(BufId)->getBufferIdentifier();
}

StringRef ImmutableTextBuffer::getText() const {

  return SrcMgr->getMemoryBuffer(BufId)->getBuffer();
}

const llvm::MemoryBuffer *ImmutableTextBuffer::getInternalBuffer() const {
  return SrcMgr->getMemoryBuffer(BufId);
}

std::pair<unsigned, unsigned>
ImmutableTextBuffer::getLineAndColumn(unsigned ByteOffset) const {
  auto Buf = SrcMgr->getMemoryBuffer(BufId);
  if (ByteOffset > Buf->getBufferSize())
    return std::make_pair(0, 0);

  SMLoc Loc = SMLoc::getFromPointer(Buf->getBufferStart() + ByteOffset);
  return SrcMgr->getLineAndColumn(Loc, 0);
}

ReplaceImmutableTextUpdate::ReplaceImmutableTextUpdate(
    unsigned ByteOffset, unsigned Length,
    StringRef Text, uint64_t Stamp)
  : ImmutableTextUpdate(Kind::Replace, Stamp),
    Buf(llvm::MemoryBuffer::getMemBufferCopy(Text)),
    ByteOffset(ByteOffset), Length(Length) {
}

StringRef ReplaceImmutableTextUpdate::getText() const {
  return Buf->getBuffer();
}

StringRef ImmutableTextSnapshot::getFilename() const {
  return EditableBuf->getFilename();
}

uint64_t ImmutableTextSnapshot::getStamp() const {
  return DiffEnd->getStamp();
}

ImmutableTextBufferRef ImmutableTextSnapshot::getBuffer() const {
  return EditableBuf->getBufferForSnapshot(*this);
}

size_t ImmutableTextSnapshot::getSize() const {
  return EditableBuf->getSizeForSnapshot(*this);
}

bool ImmutableTextSnapshot::precedesOrSame(ImmutableTextSnapshotRef Other) {
  assert(Other);

  ImmutableTextUpdateRef Upd = this->DiffEnd;
  while (Upd) {
    if (Upd == Other->DiffEnd) {
      return true;
    }
    Upd = Upd->getNext();
  }

  return false;
}

bool ImmutableTextSnapshot::foreachReplaceUntil(
    ImmutableTextSnapshotRef EndSnapshot,
    std::function<bool(ReplaceImmutableTextUpdateRef Upd)> Fn) {
  
  assert(EndSnapshot);
  ImmutableTextUpdateRef Upd = DiffEnd;
  while (Upd != EndSnapshot->DiffEnd) {
    Upd = Upd->getNext();
    if (!Upd) {
      assert(0 && "Did not find end snapshot");
      break;
    }
    if (auto ReplaceUpd = dyn_cast<ReplaceImmutableTextUpdate>(Upd))
      if (!Fn(ReplaceUpd))
        return false;
  }

  return true;
}

static std::atomic<uint64_t> Generation{ 0 };

EditableTextBuffer::EditableTextBuffer(StringRef Filename, StringRef Text) {
  this->Filename = Filename;
  Root = new ImmutableTextBuffer(Filename, Text, ++Generation);
  CurrUpd = Root;
}

ImmutableTextSnapshotRef EditableTextBuffer::getSnapshot() const {
  llvm::sys::ScopedLock L(EditMtx);
  return new ImmutableTextSnapshot(const_cast<EditableTextBuffer*>(this), Root,
                                   CurrUpd);
}

ImmutableTextSnapshotRef EditableTextBuffer::insert(unsigned ByteOffset,
    StringRef Text) {
  ImmutableTextUpdateRef NewUpd =
      new ReplaceImmutableTextUpdate(ByteOffset, /*Length=*/0, Text,
                                     ++Generation);
  return addAtomicUpdate(std::move(NewUpd));
}

ImmutableTextSnapshotRef EditableTextBuffer::erase(unsigned ByteOffset,
                                                   unsigned Length) {
  ImmutableTextUpdateRef NewUpd =
      new ReplaceImmutableTextUpdate(ByteOffset, Length, StringRef(),
                                     ++Generation);
  return addAtomicUpdate(std::move(NewUpd));
}

ImmutableTextSnapshotRef EditableTextBuffer::replace(unsigned ByteOffset,
                                                     unsigned Length,
                                                     StringRef Text) {
  ImmutableTextUpdateRef NewUpd =
      new ReplaceImmutableTextUpdate(ByteOffset, Length, Text, ++Generation);
  return addAtomicUpdate(std::move(NewUpd));
}

ImmutableTextSnapshotRef EditableTextBuffer::addAtomicUpdate(
    ImmutableTextUpdateRef NewUpd) {

  llvm::sys::ScopedLock L(EditMtx);

  refresh();

  assert(CurrUpd->Next == nullptr);
  CurrUpd->Next = NewUpd;
  CurrUpd = NewUpd;

  return new ImmutableTextSnapshot(this, Root, CurrUpd);
}

static std::unique_ptr<llvm::MemoryBuffer>
getMemBufferFromRope(StringRef Filename, const RewriteRope &Rope) {
  size_t Length = 0;
  for (RewriteRope::iterator I = Rope.begin(), E = Rope.end(); I != E;
       I.MoveToNextPiece()) {
    Length += I.piece().size();
  }

  auto MemBuf =
    llvm::WritableMemoryBuffer::getNewUninitMemBuffer(Length, Filename);
  char *Ptr = (char*)MemBuf->getBufferStart();
  for (RewriteRope::iterator I = Rope.begin(), E = Rope.end(); I != E;
       I.MoveToNextPiece()) {
    StringRef Text = I.piece();
    memcpy(Ptr, Text.data(), Text.size());
    Ptr += Text.size();
  }

  return std::move(MemBuf);
}

ImmutableTextBufferRef EditableTextBuffer::getBufferForSnapshot(
    const ImmutableTextSnapshot &Snap) {
  if (auto Buf = dyn_cast<ImmutableTextBuffer>(Snap.DiffEnd))
    return Buf;
  ImmutableTextUpdateRef Next = Snap.DiffEnd->Next;
  // FIXME: dyn_cast_null does not work with IntrusiveRefCntPtr.
  if (Next)
    if (auto Buf = dyn_cast<ImmutableTextBuffer>(Next))
      return Buf;

  // Check if a buffer was created in the middle of the snapshot updates.
  ImmutableTextBufferRef StartBuf = Snap.BufferStart;
  ImmutableTextUpdateRef Upd = StartBuf;  
  while (Upd != Snap.DiffEnd) {
    Upd = Upd->Next;
    if (auto Buf = dyn_cast<ImmutableTextBuffer>(Upd))
      StartBuf = Buf;
  }
  StringRef StartText = StartBuf->getText();

  RewriteRope Rope;
  auto applyUpdate = [&](const ImmutableTextUpdateRef &Upd) {
    if (auto ReplaceUpd = dyn_cast<ReplaceImmutableTextUpdate>(Upd)) {
      Rope.erase(ReplaceUpd->getByteOffset(), ReplaceUpd->getLength());
      StringRef Text = ReplaceUpd->getText();
      Rope.insert(ReplaceUpd->getByteOffset(), Text.begin(), Text.end());
    }
  };

  Rope.assign(StartText.begin(), StartText.end());
  Upd = StartBuf;
  while (Upd != Snap.DiffEnd) {
    Upd = Upd->Next;
    applyUpdate(Upd);
  }

  auto MemBuf = getMemBufferFromRope(getFilename(), Rope);
  ImmutableTextBufferRef ImmBuf = new ImmutableTextBuffer(std::move(MemBuf),
                                                          Snap.getStamp());

  {
    llvm::sys::ScopedLock L(EditMtx);
    ImmBuf->Next = Snap.DiffEnd->Next;
    Snap.DiffEnd->Next = ImmBuf;
    refresh();
  }
  return ImmBuf;
}

size_t EditableTextBuffer::getSizeForSnapshot(
    const ImmutableTextSnapshot &Snap) const {
  if (auto Buf = dyn_cast<ImmutableTextBuffer>(Snap.DiffEnd))
    return Buf->getText().size();
  ImmutableTextUpdateRef Next = Snap.DiffEnd->Next;
  // FIXME: dyn_cast_null does not work with IntrusiveRefCntPtr.
  if (Next)
    if (auto Buf = dyn_cast<ImmutableTextBuffer>(Next))
      return Buf->getText().size();

  ImmutableTextBufferRef StartBuf = Snap.BufferStart;

  // Find the last ImmutableTextBuffer.
  ImmutableTextUpdateRef Upd = StartBuf;
  while (Upd != Snap.DiffEnd) {
    Upd = Upd->Next;
    if (auto Buf = dyn_cast<ImmutableTextBuffer>(Upd))
      StartBuf = Buf;
  }

  size_t Length = StartBuf->getText().size();
  Upd = StartBuf;
  while (Upd != Snap.DiffEnd) {
    Upd = Upd->Next;
    auto Edit = cast<ReplaceImmutableTextUpdate>(Upd);
    Length = Length - Edit->getLength() + Edit->getText().size();
  }
  return Length;
}

// This should always be called under the mutex lock.
void EditableTextBuffer::refresh() {
  while (CurrUpd->Next) {
    CurrUpd = CurrUpd->Next;
    if (auto Buf = dyn_cast<ImmutableTextBuffer>(CurrUpd))
      Root = Buf;
  }
}

EditableTextBufferRef EditableTextBufferManager::getOrCreateBuffer(
    StringRef Filename,
    StringRef Text) {

  llvm::sys::ScopedLock L(Mtx);

  assert(!Filename.empty());
  EditableTextBufferRef &EdBuf = FileBufferMap[Filename];
  if (!EdBuf)
    EdBuf = new EditableTextBuffer(Filename, Text);

  return EdBuf;
}

EditableTextBufferRef
EditableTextBufferManager::resetBuffer(StringRef Filename, StringRef Text) {
  llvm::sys::ScopedLock L(Mtx);

  assert(!Filename.empty());
  EditableTextBufferRef &EdBuf = FileBufferMap[Filename];
  EdBuf = new EditableTextBuffer(Filename, Text);
  return EdBuf;
}
