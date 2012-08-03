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
  for (SourceRange R : Info.Ranges) {
    SourceLoc End = Lexer::getLocForEndOfToken(SM, R.End);
    
    // FIXME: SMRange is an inclusive range [start, end], so step the
    // end location back by one to get SourceMgr to highlight ranges
    // properly.
    if (R.Start != End)
      End = End.getAdvancedLoc(-1);
    
    Ranges.push_back(llvm::SMRange(R.Start.Value, End.Value));
  }
  
  // Display the diagnostic.
  SM.PrintMessage(Loc.Value, SMKind, StringRef(Text), Ranges);
}

