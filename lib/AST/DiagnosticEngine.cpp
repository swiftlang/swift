//===- DiagnosticEngine.h - Diagnostic Display Engine -----------*- C++ -*-===//
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
//  This file defines the DiagnosticEngine class, which manages any diagnostics
//  emitted by Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticEngine.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

struct StoredDiagnosticInfo {
  /// \brief The kind of diagnostic we're dealing with.
  DiagnosticKind Kind;
  
  // FIXME: Category
  
  /// \brief Text associated with the diagnostic
  const char *Text;
};

static StoredDiagnosticInfo StoredDiagnosticInfos[] = {
#define ERROR(ID,Category,Options,Text,Signature) \
  { DiagnosticKind::Error, Text },
#define WARNING(ID,Category,Options,Text,Signature) \
  { DiagnosticKind::Warning, Text },
#define NOTE(ID,Category,Options,Text,Signature) \
  { DiagnosticKind::Note, Text },
#include "swift/AST/Diagnostics.def"
  { DiagnosticKind::Error, "<not a diagnostic>" }
};

void InFlightDiagnostic::flush() {
  if (!IsActive)
    return;
  
  IsActive = false;
  if (Engine)
    Engine->flushActiveDiagnostic();
}

/// \brief Skip forward to one of the given delimiters.
///
/// \param Text The text to search through, which will be updated to point
/// just after the delimiter.
///
/// \param Delim1 The first character delimiter to search for.
///
/// \param Delim2 The second character delimiter to search for.
///
/// \returns The string leading up to the delimiter, or the empty string
/// if no delimiter is found.
static StringRef 
skipToDelimiter(StringRef &Text, char Delim1, char Delim2 = 0) {
  unsigned Depth = 0;

  unsigned I = 0;
  for (unsigned N = Text.size(); I != N; ++I) {
    if (Depth == 0 && Text[I] == '{') {
      ++Depth;
      continue;
    }
    if (Depth > 0 && Text[I] == '}') {
      --Depth;
      continue;
    }
    
    if (Text[I] == Delim1 || Text[I] == Delim2)
      break;
  }

  assert(Depth == 0 && "Unbalanced {} set in diagnostic text");
  StringRef Result = Text.substr(0, I);
  Text = Text.substr(I + 1);
  return Result;
}

/// \brief Format a selection argument and write it to the given stream.
static void formatSelectionArgument(StringRef ModifierArguments,
                                    ArrayRef<DiagnosticArgument> Args,
                                    unsigned SelectedIndex,
                                    llvm::raw_ostream &Out) {
  do {
    StringRef Text = skipToDelimiter(ModifierArguments, '|');
    if (SelectedIndex == 0) {
      Out << Text;
      break;
    }
    --SelectedIndex;
  } while (true);
  
}

/// \brief Format a single diagnostic argument and write it to the given
/// stream.
static void formatDiagnosticArgument(StringRef Modifier, 
                                     StringRef ModifierArguments,
                                     ArrayRef<DiagnosticArgument> Args,
                                     unsigned ArgIndex,
                                     llvm::raw_ostream &Out) {
  const DiagnosticArgument &Arg = Args[ArgIndex];
  switch (Arg.getKind()) {
  case DiagnosticArgumentKind::Integer:
    if (Modifier == "select") {
      assert(Arg.getAsInteger() >= 0 && "Negative selection index");
      formatSelectionArgument(ModifierArguments, Args, Arg.getAsInteger(), 
                              Out);
    } else {
      assert(Modifier.empty() && "Improper modifier for integer argument");
      Out << Arg.getAsInteger();
    }
    break;

  case DiagnosticArgumentKind::Unsigned:
    if (Modifier == "select") {
      formatSelectionArgument(ModifierArguments, Args, Arg.getAsUnsigned(), 
                              Out);
    } else {
      assert(Modifier.empty() && "Improper modifier for unsigned argument");
      Out << Arg.getAsUnsigned();
    }
    break;

  case DiagnosticArgumentKind::String:
    assert(Modifier.empty() && "Improper modifier for string argument");
    Out << Arg.getAsString();
    break;

  case DiagnosticArgumentKind::Identifier:
    assert(Modifier.empty() && "Improper modifier for identifier argument");
    Out << '\'' << Arg.getAsIdentifier() << '\'';
    break;
  case DiagnosticArgumentKind::Type:
    assert(Modifier.empty() && "Improper modifier for Type argument");
    Out << '\'' << Arg.getAsType().getString() << '\'';
    break;
  }
}

/// \brief Format the given diagnostic text and place the result in the given
/// buffer.
static void formatDiagnosticText(StringRef InText, 
                                 ArrayRef<DiagnosticArgument> Args,
                                 llvm::SmallVectorImpl<char> &OutText) {
  llvm::raw_svector_ostream Out(OutText);
  while (!InText.empty()) {
    size_t Percent = InText.find('%');
    if (Percent == StringRef::npos) {
      // Write the rest of the string; we're done.
      Out.write(InText.data(), InText.size());
      break;
    }
    
    // Write the string up to (but not including) the %, then drop that text
    // (including the %).
    Out.write(InText.data(), Percent);
    InText = InText.substr(Percent + 1);
    
    // '%%' -> '%'.
    if (InText[0] == '%') {
      Out.write('%');
      InText = InText.substr(1);
      continue;
    }

    // Parse an optional modifier.
    StringRef Modifier;
    {
      unsigned Length = 0;
      while (isalpha(InText[Length]))
        ++Length;
      Modifier = InText.substr(0, Length);
      InText = InText.substr(Length);
    }
    
    // Parse the optional argument list for a modifier, which is brace-enclosed.
    StringRef ModifierArguments;
    if (InText[0] == '{') {
      InText = InText.substr(1);
      ModifierArguments = skipToDelimiter(InText, '}');
    }
    
    // Find the digit sequence.
    unsigned Length = 0;
    for (size_t N = InText.size(); Length != N; ++Length) {
      if (!isdigit(InText[Length]))
        break;
    }
      
    // Parse the digit sequence into an argument index.
    unsigned ArgIndex;      
    bool Result = InText.substr(0, Length).getAsInteger(10, ArgIndex);
    assert(!Result && "Unparseable argument index value?");
    (void)Result;
    assert(ArgIndex < Args.size() && "Out-of-range argument index");
    InText = InText.substr(Length);
    
    // Convert the argument to a string.
    formatDiagnosticArgument(Modifier, ModifierArguments, Args, ArgIndex, Out);
  }
}
                             
void DiagnosticEngine::flushActiveDiagnostic() {
  assert(ActiveDiagnostic && "No active diagnostic to flush");
  const StoredDiagnosticInfo &StoredInfo
    = StoredDiagnosticInfos[(unsigned)ActiveDiagnostic->getID()];

  // Check whether this is an error.
  switch (StoredInfo.Kind) {
  case DiagnosticKind::Error:
    HadAnyError = true;
    break;
    
  case DiagnosticKind::Note:
  case DiagnosticKind::Warning:
    break;
  }
  
  // Actually substitute the diagnostic arguments into the diagnostic text.
  llvm::SmallString<256> Text;
  formatDiagnosticText(StoredInfo.Text, ActiveDiagnostic->getArgs(), Text);
  
  // Pass the diagnostic off to the consumer.
  DiagnosticInfo Info;
  Info.Ranges = ActiveDiagnostic->getRanges();
  Consumer.handleDiagnostic(SourceMgr, ActiveDiagnosticLoc, StoredInfo.Kind,
                            Text, Info);
  
  // Reset the active diagnostic.
  ActiveDiagnostic.reset();
}
