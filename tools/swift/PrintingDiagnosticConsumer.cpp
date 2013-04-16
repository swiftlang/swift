//===- PrintingDiagnosticConsumer.cpp - Print Text Diagnostics---*- C++ -*-===//
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
//  This file implements the PrintingDiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#include "PrintingDiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

static llvm::SMRange getRawRange(llvm::SourceMgr &SM, DiagnosticInfo::Range R) {
  SourceLoc End;
  if (R.IsTokenRange)
    End = Lexer::getLocForEndOfToken(SM, R.End);
  else
    End = R.End;

  return llvm::SMRange(R.Start.Value, End.Value);
}

static llvm::SMFixIt getRawFixIt(llvm::SourceMgr &SM, DiagnosticInfo::FixIt F) {
  // FIXME: It's unfortunate that we have to copy the replacement text.
  return llvm::SMFixIt(getRawRange(SM, F.getRange()), F.getText());
}

void
PrintingDiagnosticConsumer::handleDiagnostic(llvm::SourceMgr &SM, SourceLoc Loc,
                                             DiagnosticKind Kind, 
                                             llvm::StringRef Text,
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

  // Translate fixits.
  SmallVector<llvm::SMFixIt, 2> FixIts;
  for (DiagnosticInfo::FixIt F : Info.FixIts)
    FixIts.push_back(getRawFixIt(SM, F));

  // Display the diagnostic.
  SM.PrintMessage(Loc.Value, SMKind, StringRef(Text), Ranges, FixIts);
}

