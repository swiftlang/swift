//===- DiagnosticVerifier.cpp - Diagnostic Verifier (-verify) -------------===//
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
//  This file implements the DiagnosticVerifier class.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

namespace {
  struct ExpectedDiagnosticInfo {
    StringRef Str;
    unsigned LineNo;
    llvm::SourceMgr::DiagKind Classification;
  };

  struct ExpectedFixIt {
    unsigned StartCol;
    unsigned EndCol;
    std::string Text;
  };
}

static std::string getDiagKindString(llvm::SourceMgr::DiagKind Kind) {
  switch (Kind) {
  case llvm::SourceMgr::DK_Error: return "error";
  case llvm::SourceMgr::DK_Warning: return "warning";
  case llvm::SourceMgr::DK_Note: return "note";
  }
}



namespace {
  /// This class implements support for -verify mode in the compiler.  It
  /// buffers up diagnostics produced during compilation, then checks them
  /// against expected-error markers in the source file.
  class DiagnosticVerifier {
    SourceManager &SM;
    std::vector<llvm::SMDiagnostic> CapturedDiagnostics;
  public:
    explicit DiagnosticVerifier(SourceManager &SM) : SM(SM) {}

    void addDiagnostic(const llvm::SMDiagnostic &Diag) {
      CapturedDiagnostics.push_back(Diag);
    }

    /// verifyFile - After the file has been processed, check to see if we
    /// got all of the expected diagnostics and check to see if there were any
    /// unexpected ones.
    bool verifyFile(unsigned BufferID);

  private:
    std::vector<llvm::SMDiagnostic>::iterator
    findDiagnostic(const ExpectedDiagnosticInfo &Expected,
                   StringRef BufferName);

  };
} // end anonymous namespace




/// If we find the specified diagnostic in the list, return it.
/// Otherwise return CapturedDiagnostics.end().
std::vector<llvm::SMDiagnostic>::iterator
DiagnosticVerifier::findDiagnostic(const ExpectedDiagnosticInfo &Expected,
                                   StringRef BufferName) {
  for (auto I = CapturedDiagnostics.begin(), E = CapturedDiagnostics.end();
       I != E; ++I) {
    // Verify the file and line of the diagnostic.
    if (I->getLineNo() != (int)Expected.LineNo ||
        I->getFilename() != BufferName)
      continue;

    // Verify the classification and string.
    if (I->getKind() != Expected.Classification ||
        I->getMessage().find(Expected.Str) == StringRef::npos)
      continue;

    // Okay, we found a match, hurray!
    return I;
  }

  return CapturedDiagnostics.end();
}

static unsigned getColumnNumber(StringRef buffer, llvm::SMLoc loc) {
  assert(loc.getPointer() >= buffer.data());
  assert((size_t)(loc.getPointer() - buffer.data()) <= buffer.size());

  StringRef UpToLoc = buffer.slice(0, loc.getPointer() - buffer.data());

  size_t ColumnNo = UpToLoc.size();
  size_t NewlinePos = UpToLoc.find_last_of("\r\n");
  if (NewlinePos != StringRef::npos)
    ColumnNo -= NewlinePos;

  return static_cast<unsigned>(ColumnNo);
}

/// Return true if the given \p ExpectedFixIt is in the fix-its emitted by
/// diagnostic \p D.
static bool checkForFixIt(const ExpectedFixIt &Expected,
                          const llvm::SMDiagnostic &D,
                          StringRef buffer) {
  for (auto &ActualFixIt : D.getFixIts()) {
    if (ActualFixIt.getText() != Expected.Text)
      continue;

    llvm::SMRange Range = ActualFixIt.getRange();
    if (getColumnNumber(buffer, Range.Start) != Expected.StartCol)
      continue;
    if (getColumnNumber(buffer, Range.End) != Expected.EndCol)
      continue;

    return true;
  }

  return false;
}


/// \brief After the file has been processed, check to see if we got all of
/// the expected diagnostics and check to see if there were any unexpected
/// ones.
bool DiagnosticVerifier::verifyFile(unsigned BufferID) {
  const SourceLoc BufferStartLoc = SM.getLocForBufferStart(BufferID);
  CharSourceRange EntireRange = SM.getRangeForBuffer(BufferID);
  StringRef InputFile = SM.extractText(EntireRange);
  StringRef BufferName = SM.getIdentifierForBuffer(BufferID);

  // Queue up all of the diagnostics, allowing us to sort them and emit them in
  // file order.
  std::vector<std::pair<const char *, std::string> > Errors;

  unsigned PrevExpectedContinuationLine = 0;

  // Scan the memory buffer looking for expected-note/warning/error.
  for (size_t Match = InputFile.find("expected-");
       Match != StringRef::npos; Match = InputFile.find("expected-", Match+1)) {
    // Process this potential match.  If we fail to process it, just move on to
    // the next match.
    StringRef MatchStart = InputFile.substr(Match);
    const char *ExpectedStringStart = MatchStart.data();

    llvm::SourceMgr::DiagKind ExpectedClassification;
    if (MatchStart.startswith("expected-note")) {
      ExpectedClassification = llvm::SourceMgr::DK_Note;
      MatchStart = MatchStart.substr(strlen("expected-note"));
    } else if (MatchStart.startswith("expected-warning")) {
      ExpectedClassification = llvm::SourceMgr::DK_Warning;
      MatchStart = MatchStart.substr(strlen("expected-warning"));
    } else if (MatchStart.startswith("expected-error")) {
      ExpectedClassification = llvm::SourceMgr::DK_Error;
      MatchStart = MatchStart.substr(strlen("expected-error"));
    } else
      continue;

    // Skip any whitespace before the {{.
    MatchStart = MatchStart.substr(MatchStart.find_first_not_of(" \t"));

    size_t TextStartIdx = MatchStart.find("{{");
    if (TextStartIdx == StringRef::npos) {
      Errors.push_back(std::make_pair(MatchStart.data(),
                                      "expected {{ in expected-warning/note/error line"));
      continue;
    }

    bool AtLeastOne = false;
    unsigned MatchCount = 1;
    int LineOffset = 0;
    if (TextStartIdx > 0 && MatchStart[0] == '@') {
      if (MatchStart[1] != '+' && MatchStart[1] != '-') {
        Errors.push_back({ MatchStart.data(),
          "expected '+'/'-' for line offset" });
        continue;
      }
      StringRef Offs;
      if (MatchStart[1] == '+')
        Offs = MatchStart.slice(2, TextStartIdx).rtrim();
      else
        Offs = MatchStart.slice(1, TextStartIdx).rtrim();

      size_t SpaceIndex = Offs.find(' ');
      if (SpaceIndex != StringRef::npos && SpaceIndex < TextStartIdx) {
        size_t Delta = Offs.size() - SpaceIndex;
        MatchStart = MatchStart.substr(TextStartIdx - Delta);
        TextStartIdx = Delta;
        Offs = Offs.slice(0, SpaceIndex);
      } else {
        MatchStart = MatchStart.substr(TextStartIdx);
        TextStartIdx = 0;
      }

      if (Offs.getAsInteger(10, LineOffset)) {
        Errors.push_back({ MatchStart.data(),
          "expected line offset before '{{'" });
        continue;
      }
    }

    if (TextStartIdx > 0) {
      StringRef CountStr = MatchStart.substr(0, TextStartIdx).trim();
      if (CountStr == "+") {
        // Rely on wraparound to bring this to UINT_MAX after the first match,
        // thus continuing to search for diagnostics until we've matched all
        // of them.
        MatchCount = 0;
        AtLeastOne = true;
      } else if (CountStr.getAsInteger(10, MatchCount)) {
        Errors.push_back(std::make_pair(MatchStart.data(),
                                        "expected match count before '{{'"));
        continue;
      } else if (MatchCount == 0) {
        Errors.push_back({ MatchStart.data(),
          "expected positive match count before '{{'" });
        continue;
      }

      // Resync up to the '{{'.
      MatchStart = MatchStart.substr(TextStartIdx);
    }

    size_t End = MatchStart.find("}}");
    if (End == StringRef::npos) {
      Errors.push_back(std::make_pair(MatchStart.data(),
                                      "didn't find '}}' to match '{{' in expected-warning/note/error line"));
      continue;
    }

    ExpectedDiagnosticInfo Expected;
    Expected.Classification = ExpectedClassification;
    Expected.Str = MatchStart.slice(2, End);
    if (PrevExpectedContinuationLine)
      Expected.LineNo = PrevExpectedContinuationLine;
    else
      Expected.LineNo = SM.getLineAndColumn(
          BufferStartLoc.getAdvancedLoc(MatchStart.data() - InputFile.data()),
          BufferID).first;
    Expected.LineNo += LineOffset;

    // Check if the next expected diagnostic should be in the same line.
    StringRef AfterEnd = MatchStart.substr(End + strlen("}}"));
    AfterEnd = AfterEnd.substr(AfterEnd.find_first_not_of(" \t"));
    if (AfterEnd.startswith("\\"))
      PrevExpectedContinuationLine = Expected.LineNo;
    else
      PrevExpectedContinuationLine = 0;

    // Check to see if we had this expected diagnostic.
    do {
      auto FoundDiagnosticIter = findDiagnostic(Expected, BufferName);
      if (FoundDiagnosticIter == CapturedDiagnostics.end()) {
        if (!AtLeastOne || MatchCount == 0) {
          std::string message =
            "expected "+getDiagKindString(Expected.Classification) +
            " not produced";
          Errors.push_back(std::make_pair(ExpectedStringStart, message));
        }
        break;
      }
      auto &FoundDiagnostic = *FoundDiagnosticIter;

      // Scan for fix-its: {{10-14=replacement text}}
      StringRef ExtraChecks = MatchStart.substr(End+2).ltrim(" \t");
      while (ExtraChecks.startswith("{{")) {
        // First make sure we have a closing "}}".
        size_t EndLoc = ExtraChecks.find("}}");
        if (EndLoc == StringRef::npos) {
          Errors.push_back(std::make_pair(ExtraChecks.data(),
                                          "didn't find '}}' to match '{{' in "
                                          "fix-it verification"));
          break;
        }

        // Allow for close braces to appear in the replacement text.
        while (EndLoc+2 < ExtraChecks.size() && ExtraChecks[EndLoc+2] == '}')
          ++EndLoc;

        StringRef FixItStr = ExtraChecks.slice(2, EndLoc);
        // Check for matching a later "}}" on a different line.
        if (FixItStr.find_first_of("\r\n") != StringRef::npos) {
          Errors.push_back(std::make_pair(ExtraChecks.data(),
                                          "didn't find '}}' to match '{{' in "
                                          "fix-it verification"));
          break;
        }

        // Prepare for the next round of checks.
        ExtraChecks = ExtraChecks.substr(EndLoc+2).ltrim();

        // Parse the pieces of the fix-it.
        size_t MinusLoc = FixItStr.find('-');
        if (MinusLoc == StringRef::npos) {
          Errors.push_back(std::make_pair(FixItStr.data(),
                                          "expected '-' in fix-it verification"));
          continue;
        }
        StringRef StartColStr = FixItStr.slice(0, MinusLoc);
        StringRef AfterMinus = FixItStr.substr(MinusLoc+1);

        size_t EqualLoc = AfterMinus.find('=');
        if (EqualLoc == StringRef::npos) {
          Errors.push_back(std::make_pair(AfterMinus.data(),
                                          "expected '=' after '-' in fix-it "
                                          "verification"));
          continue;
        }
        StringRef EndColStr = AfterMinus.slice(0, EqualLoc);
        StringRef AfterEqual = AfterMinus.substr(EqualLoc+1);

        ExpectedFixIt FixIt;
        if (StartColStr.getAsInteger(10, FixIt.StartCol)) {
          Errors.push_back(std::make_pair(StartColStr.data(),
                                          "invalid column number in fix-it "
                                          "verification"));
          continue;
        }
        if (EndColStr.getAsInteger(10, FixIt.EndCol)) {
          Errors.push_back(std::make_pair(EndColStr.data(),
                                          "invalid column number in fix-it "
                                          "verification"));
          continue;
        }

        // Translate literal "\\n" into '\n', inefficiently.
        StringRef fixItText = AfterEqual.slice(0, EndLoc);
        for (const char *current = fixItText.begin(), *end = fixItText.end();
             current != end; /* in loop */) {
          if (*current == '\\' && current + 1 < end && *(current + 1) == 'n') {
            FixIt.Text += '\n';
            current += 2;
          } else {
            FixIt.Text += *current++;
          }
        }

        // Finally, make sure the fix-it is present in the diagnostic.
        if (!checkForFixIt(FixIt, FoundDiagnostic, InputFile)) {
          std::string Message;
          {
            llvm::raw_string_ostream OS(Message);
            OS << "expected fix-it not seen";

            if (!FoundDiagnostic.getFixIts().empty()) {
              OS << "; actual fix-its:";

              for (auto &ActualFixIt : FoundDiagnostic.getFixIts()) {
                llvm::SMRange Range = ActualFixIt.getRange();

                OS << " {{"
                << getColumnNumber(InputFile, Range.Start) << '-'
                << getColumnNumber(InputFile, Range.End) << '='
                << ActualFixIt.getText()
                << "}}";
              }
            }
          }

          Errors.push_back(std::make_pair(StartColStr.data()-2,
                                          std::move(Message)));
        }
      }

      // Actually remove the diagnostic from the list, so we don't match it
      // again. We do have to do this after checking fix-its, though, because
      // the diagnostic owns its fix-its.
      CapturedDiagnostics.erase(FoundDiagnosticIter);
    } while (--MatchCount);
  }

  // Verify that there are no diagnostics (in MemoryBuffer) left in the list.
  for (unsigned i = 0, e = CapturedDiagnostics.size(); i != e; ++i) {
    if (CapturedDiagnostics[i].getFilename() != BufferName)
      continue;

    std::string Message =
      "unexpected "+getDiagKindString(CapturedDiagnostics[i].getKind())+
      " produced: "+CapturedDiagnostics[i].getMessage().str();
    Errors.push_back(std::make_pair(
                                    CapturedDiagnostics[i].getLoc().getPointer(),
                                    Message));
  }

  // Sort the diagnostics by their address in the memory buffer as the primary
  // key.  This ensures that an "unexpected diagnostic" and
  // "expected diagnostic" in the same place are emitted next to each other.
  std::sort(Errors.begin(), Errors.end());

  // Emit all of the queue'd up errors.
  for (auto Err : Errors)
    SM.getLLVMSourceMgr().PrintMessage(llvm::SMLoc::getFromPointer(Err.first),
                                       llvm::SourceMgr::DK_Error, Err.second);
  
  return !Errors.empty();
}



//===----------------------------------------------------------------------===//
// Main entrypoints
//===----------------------------------------------------------------------===//

/// VerifyModeDiagnosticHook - Every time a diagnostic is generated in -verify
/// mode, this function is called with the diagnostic.  We just buffer them up
/// until the end of the file.
static void VerifyModeDiagnosticHook(const llvm::SMDiagnostic &Diag,
                                     void *Context) {
  ((DiagnosticVerifier*)Context)->addDiagnostic(Diag);
}


/// enableDiagnosticVerifier - Set up the specified source manager so that
/// diagnostics are captured instead of being printed.
void swift::enableDiagnosticVerifier(SourceManager &SM) {
  SM.getLLVMSourceMgr().setDiagHandler(VerifyModeDiagnosticHook,
                                       new DiagnosticVerifier(SM));
}

/// verifyDiagnostics - Verify that captured diagnostics meet with the
/// expectations of the source files corresponding to the specified BufferIDs
/// and tear down our support for capturing and verifying diagnostics.
bool swift::verifyDiagnostics(SourceManager &SM, ArrayRef<unsigned> BufferIDs) {
  auto *Verifier = (DiagnosticVerifier*)SM.getLLVMSourceMgr().getDiagContext();
  SM.getLLVMSourceMgr().setDiagHandler(nullptr, nullptr);

  bool HadError = false;

  for (auto &BufferID : BufferIDs)
    HadError |= Verifier->verifyFile(BufferID);

  delete Verifier;

  return HadError;
}

