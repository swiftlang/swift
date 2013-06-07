//===- SerializedDiagnosticConsumer.cpp - Serialize Diagnostics --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the SerializedDiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#include "SerializedDiagnosticConsumer.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Bitcode/BitstreamWriter.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// These must match Clang's diagnostic IDs.  We can consider sharing the
// header files to avoid this copy-paste.
//===----------------------------------------------------------------------===//

enum BlockIDs {
  /// \brief A top-level block which represents any meta data associated
  /// with the diagostics, including versioning of the format.
  BLOCK_META = llvm::bitc::FIRST_APPLICATION_BLOCKID,

  /// \brief The this block acts as a container for all the information
  /// for a specific diagnostic.
  BLOCK_DIAG
};

enum RecordIDs {
  RECORD_VERSION = 1,
  RECORD_DIAG,
  RECORD_SOURCE_RANGE,
  RECORD_DIAG_FLAG,
  RECORD_CATEGORY,
  RECORD_FILENAME,
  RECORD_FIXIT,
  RECORD_FIRST = RECORD_VERSION,
  RECORD_LAST = RECORD_FIXIT
};

//===----------------------------------------------------------------------===//

namespace {
class AbbreviationMap {
  llvm::DenseMap<unsigned, unsigned> Abbrevs;
public:
  AbbreviationMap() {}

  void set(unsigned recordID, unsigned abbrevID) {
    assert(Abbrevs.find(recordID) == Abbrevs.end()
           && "Abbreviation already set.");
    Abbrevs[recordID] = abbrevID;
  }

  unsigned get(unsigned recordID) {
    assert(Abbrevs.find(recordID) != Abbrevs.end() &&
           "Abbreviation not set.");
    return Abbrevs[recordID];
  }
};

typedef SmallVector<uint64_t, 64> RecordData;
typedef SmallVectorImpl<uint64_t> RecordDataImpl;

struct SharedState : llvm::RefCountedBase<SharedState> {
  SharedState(raw_ostream *os)
    : Stream(Buffer), OS(os), EmittedAnyDiagBlocks(false) { }

  /// \brief The byte buffer for the serialized content.
  llvm::SmallString<1024> Buffer;

  /// \brief The BitStreamWriter for the serialized diagnostics.
  llvm::BitstreamWriter Stream;

  /// \brief The name of the diagnostics file.
  llvm::OwningPtr<raw_ostream> OS;

  /// \brief The set of constructed record abbreviations.
  AbbreviationMap Abbrevs;

  /// \brief A utility buffer for constructing record content.
  RecordData Record;

  /// \brief A text buffer for rendering diagnostic text.
  llvm::SmallString<256> diagBuf;

  /// \brief The collection of files used.
  llvm::DenseMap<const char *, unsigned> Files;

  typedef llvm::DenseMap<const void *, std::pair<unsigned, StringRef> >
  DiagFlagsTy;

  /// \brief Map for uniquing strings.
  DiagFlagsTy DiagFlags;

  /// \brief Whether we have already started emission of any DIAG blocks. Once
  /// this becomes \c true, we never close a DIAG block until we know that we're
  /// starting another one or we're done.
  bool EmittedAnyDiagBlocks;
};

/// \brief Diagnostic consumer that serializes diagnostics to a stream.
class SerializedDiagnosticConsumer : public DiagnosticConsumer {
  /// \brief State shared among the various clones of this diagnostic consumer.
  llvm::IntrusiveRefCntPtr<SharedState> State;
public:
  SerializedDiagnosticConsumer(raw_ostream *OS) : State(new SharedState(OS)) {}

  virtual void handleDiagnostic(llvm::SourceMgr &SM, SourceLoc Loc,
                                DiagnosticKind Kind, llvm::StringRef Text,
                                const DiagnosticInfo &Info);
private:
  void enterDiagBlock() {
    State->Stream.EnterSubblock(BLOCK_DIAG, 4);
  }

  void exitDiagBlock() {
    State->Stream.ExitBlock();
  }

  // Record identifier for the file.
  unsigned getEmitFile(StringRef Filename);

  /// \brief Add a source location to a record.
  void addLocToRecord(SourceLoc Loc,
                      llvm::SourceMgr &SM,
                      StringRef Filename,
                      RecordDataImpl &Record);

  void emitDiagnosticMessage(llvm::SourceMgr &SM, SourceLoc Loc,
                             DiagnosticKind Kind,
                             StringRef Text, const DiagnosticInfo &Info);
};
}

namespace swift { namespace serialized_diagnostics {
  DiagnosticConsumer *createConsumer(llvm::raw_ostream *OS) {
    return new SerializedDiagnosticConsumer(OS);
  }
}}

// FIXME: Copy-pasted from PrintingDiagnosticConsumer.
// Refactor when more code is wired up.  All of this is just
// rapid prototyping.
static llvm::SMRange getRawRange(llvm::SourceMgr &SM,
                                 DiagnosticInfo::Range R) {
  SourceLoc End;
  if (R.IsTokenRange)
    End = Lexer::getLocForEndOfToken(SM, R.End);
  else
    End = R.End;

  return llvm::SMRange(R.Start.Value, End.Value);
}

// FIXME: Copy-pasted from PrintingDiagnosticConsumer.
// Refactor when more code is wired up.  All of this is just
// rapid prototyping.
static llvm::SMFixIt getRawFixIt(llvm::SourceMgr &SM,
                                 DiagnosticInfo::FixIt F) {
  // FIXME: It's unfortunate that we have to copy the replacement text.
  return llvm::SMFixIt(getRawRange(SM, F.getRange()), F.getText());
}

unsigned SerializedDiagnosticConsumer::getEmitFile(StringRef Filename) {
  // NOTE: Using Filename.data() here relies on SourceMgr using
  // const char* as buffer identifiers.  This is fast, but may
  // be brittle.  We can always switch over to using a StringMap.
  unsigned &entry = State->Files[Filename.data()];
  if (entry)
    return entry;

  // Lazily generate the record for the file.  Note that in
  // practice we only expect there to be one file, but this is
  // general and is what the diagnostic file expects.
  entry = State->Files.size();
  RecordData Record;
  Record.push_back(RECORD_FILENAME);
  Record.push_back(entry);
  Record.push_back(0); // For legacy.
  Record.push_back(0); // For legacy.
  Record.push_back(Filename.size());
  State->Stream.EmitRecordWithBlob(State->Abbrevs.get(RECORD_FILENAME),
                                   Record, Filename.data());

  return entry;
}

void SerializedDiagnosticConsumer::addLocToRecord(SourceLoc Loc,
                                                  llvm::SourceMgr &SM,
                                                  StringRef Filename,
                                                  RecordDataImpl &Record)
{
  if (Loc.isInvalid()) {
    // Emit a "sentinel" location.
    Record.push_back((unsigned)0); // File.
    Record.push_back((unsigned)0); // Line.
    Record.push_back((unsigned)0); // Column.
    Record.push_back((unsigned)0); // Offset.
    return;
  }

  unsigned line, col;
  std::tie(line, col) = SM.getLineAndColumn(Loc.Value);

  Record.push_back(getEmitFile(Filename));
  Record.push_back(line);
  Record.push_back(col);
  Record.push_back(0);
}

/// \brief Map a Swift DiagosticKind to the diagnostic level expected
/// for serialized diagnostics.
static unsigned getDiagnosticLevel(DiagnosticKind Kind) {
  switch (Kind) {
    case DiagnosticKind::Error:
      return 3;
    case DiagnosticKind::Note:
      return 1;
    case DiagnosticKind::Warning:
      return 2;
  }
}

void SerializedDiagnosticConsumer::
emitDiagnosticMessage(llvm::SourceMgr &SM,
                      SourceLoc Loc,
                      DiagnosticKind Kind,
                      StringRef Text,
                      const DiagnosticInfo &Info) {

  // Determine what kind of diagnostic we're emitting.
  llvm::SourceMgr::DiagKind SMKind;
  switch (Kind) {
    case DiagnosticKind::Error:
      SMKind = llvm::SourceMgr::DK_Error;
      break;
    case DiagnosticKind::Warning:
      SMKind = llvm::SourceMgr::DK_Warning;
      break;
    case DiagnosticKind::Note:
      SMKind = llvm::SourceMgr::DK_Note;
      break;
  }

  // Translate ranges.
  SmallVector<llvm::SMRange, 2> Ranges;
  for (DiagnosticInfo::Range R : Info.Ranges)
    Ranges.push_back(getRawRange(SM, R));

  // Translate fix-its.
  SmallVector<llvm::SMFixIt, 2> FixIts;
  for (DiagnosticInfo::FixIt F : Info.FixIts)
    FixIts.push_back(getRawFixIt(SM, F));

  // Construct the diagnostic.
  const llvm::SMDiagnostic &D =
    SM.GetMessage(Loc.Value, SMKind, Text, Ranges, FixIts);

  // Emit the diagnostic to bitcode.
  llvm::BitstreamWriter &Stream = State->Stream;
  RecordData &Record = State->Record;
  AbbreviationMap &Abbrevs = State->Abbrevs;

  // Emit the RECORD_DIAG record.
  Record.clear();
  Record.push_back(RECORD_DIAG);
  Record.push_back(getDiagnosticLevel(Kind));
  addLocToRecord(Loc, SM, D.getFilename(), Record);

  // FIXME: Swift diagnostics currently have no category.
  Record.push_back(0);
  // FIXME: Swift diagnostics currently have no flags.
  Record.push_back(0);

  // Emit the message.
  Record.push_back(D.getMessage().size());
  Stream.EmitRecordWithBlob(Abbrevs.get(RECORD_DIAG), Record,
                            D.getMessage());
}

void
SerializedDiagnosticConsumer::handleDiagnostic(llvm::SourceMgr &SM,
                                               SourceLoc Loc,
                                               DiagnosticKind Kind,
                                               StringRef Text,
                                               const DiagnosticInfo &Info) {
  // Enter the block for a non-note diagnostic immediately, rather
  // than waiting for beginDiagnostic, in case associated notes
  // are emitted before we get there.
  if (Kind != DiagnosticKind::Note) {
    if (State->EmittedAnyDiagBlocks)
      exitDiagBlock();

    enterDiagBlock();
    State->EmittedAnyDiagBlocks = true;
  }

  // Special-case diagnostics with no location.
  // Make sure we bracket all notes as "sub-diagnostics".
  bool bracketDiagnostic = Loc.isInvalid() && Kind == DiagnosticKind::Note;

  if (bracketDiagnostic)
    enterDiagBlock();

  emitDiagnosticMessage(SM, Loc, Kind, Text, Info);

  if (bracketDiagnostic)
    exitDiagBlock();
}

