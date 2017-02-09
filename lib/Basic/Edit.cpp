//===--- Edit.cpp - Misc edit utilities -----------------------------------===//
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

#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Edit.h"
#include "swift/Basic/SourceManager.h"

void swift::
writeEditsInJson(ArrayRef<SingleEdit> AllEdits, llvm::raw_ostream &OS) {
  OS << "[\n";
  for (auto &Edit : AllEdits) {
    SourceManager &SM = Edit.SM;
    CharSourceRange Range = Edit.Range;
    StringRef Text = Edit.Text;
    SourceLoc Loc = Range.getStart();
    unsigned BufID = SM.findBufferContainingLoc(Loc);
    unsigned Offset = SM.getLocOffsetInBuffer(Loc, BufID);
    unsigned Length = Range.getByteLength();
    StringRef Path(SM.getIdentifierForBuffer(BufID));

    OS << " {\n";
    OS << "  \"file\": \"";
    OS.write_escaped(Path) << "\",\n";
    OS << "  \"offset\": " << Offset << ",\n";
    if (Length != 0)
      OS << "  \"remove\": " << Length << ",\n";
    if (!Text.empty()) {
      OS << "  \"text\": \"";
      OS.write_escaped(Text) << "\",\n";
    }
    OS << " },\n";
  }
  OS << "]\n";
}
