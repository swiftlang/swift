//===--- BasicSourceInfo.h - Simple source information ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BASIC_SOURCE_INFO_H
#define SWIFT_BASIC_BASIC_SOURCE_INFO_H

#include "swift/Basic/Fingerprint.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/Chrono.h"

namespace swift {

class SourceFile;

struct ExternalSourceLocs {
  struct LocationDirective {
    uint32_t Offset = 0;
    int32_t LineOffset = 0;
    uint32_t Length = 0;
    StringRef Name;

    bool isValid() const { return Length > 0; }
  };

  struct RawLoc {
    uint32_t Offset = 0;
    uint32_t Line = 0;
    uint32_t Column = 0;
    LocationDirective Directive;
  };

  struct RawLocs {
    StringRef SourceFilePath;
    SmallVector<std::pair<RawLoc, uint32_t>, 4> DocRanges;
    RawLoc Loc;
    RawLoc StartLoc;
    RawLoc EndLoc;
  };

  unsigned BufferID = 0;
  SourceLoc Loc;
  SmallVector<CharSourceRange, 4> DocRanges;
};

class BasicSourceFileInfo {
  /// If this is non-null, fields other than 'FilePath' hasn't been populated.
  /// The 'getInt()' part indicates this instance is constructed with a
  /// SourceFile.
  llvm::PointerIntPair<const SourceFile *, 1, bool> SFAndIsFromSF;

  StringRef FilePath;
  Fingerprint InterfaceHashIncludingTypeMembers = Fingerprint::ZERO();
  /// Does *not* include the type-body hashes of the top level types.
  /// Just the `SourceFile` hashes.
  /// Used for incremental imports.
  Fingerprint InterfaceHashExcludingTypeMembers = Fingerprint::ZERO();
  llvm::sys::TimePoint<> LastModified = {};
  uint64_t FileSize = 0;

  // Populate the from 'SF' member if exist. 'SF' will be cleared.
  void populateWithSourceFileIfNeeded();

public:
  BasicSourceFileInfo(StringRef FilePath,
                      Fingerprint InterfaceHashIncludingTypeMembers,
                      Fingerprint InterfaceHashExcludingTypeMembers,
                      llvm::sys::TimePoint<> LastModified, uint64_t FileSize)
      : FilePath(FilePath),
        InterfaceHashIncludingTypeMembers(InterfaceHashIncludingTypeMembers),
        InterfaceHashExcludingTypeMembers(InterfaceHashExcludingTypeMembers),
        LastModified(LastModified), FileSize(FileSize) {}

  ///  Construct with a `SourceFile`. `getInterfaceHashIncludingTypeMembers()`,
  ///  `getInterfaceHashExcludingTypeMembers()`, `getLastModified()` and
  /// `getFileSize()` are laizily populated when accessed.
  BasicSourceFileInfo(const SourceFile *SF);

  bool isFromSourceFile() const;

  StringRef getFilePath() const { return FilePath; }

  Fingerprint getInterfaceHashIncludingTypeMembers() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return InterfaceHashIncludingTypeMembers;
  }

  Fingerprint getInterfaceHashExcludingTypeMembers() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return InterfaceHashExcludingTypeMembers;
  }

  llvm::sys::TimePoint<> getLastModified() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return LastModified;
  }

  uint64_t getFileSize() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return FileSize;
  }
};

} // namespace swift

#endif // SWIFT_BASIC_BASIC_SOURCE_INFO_H

