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
#include "swift/Basic/DiagnosticEngine.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;



struct DiagnosticInfo {
  /// \brief The kind of diagnostic we're dealing with.
  DiagnosticKind Kind;
  
  // FIXME: Category
  
  /// \brief Text associated with the diagnostic
  const char *Text;
};

static DiagnosticInfo DiagnosticInfos[] = {
#define ERROR(ID,Category,Options,Text,Signature) \
  { DiagnosticKind::Error, Text },
#define WARNING(ID,Category,Options,Text,Signature) \
  { DiagnosticKind::Warning, Text },
#define NOTE(ID,Category,Options,Text,Signature) \
  { DiagnosticKind::Note, Text },
#include "swift/Basic/Diagnostics.def"
  { DiagnosticKind::Error, "<not a diagnostic>" }
};

/// \brief Format a single diagnostic argumentand write it to the given stream.
static void formatDiagnosticArgument(StringRef Modifier, 
                                     StringRef ModifierArguments,
                                     ArrayRef<DiagnosticArgument> Args,
                                     unsigned ArgIndex,
                                     llvm::raw_ostream &Out) {
  const DiagnosticArgument &Arg = Args[ArgIndex];
  switch (Arg.getKind()) {
  case DiagnosticArgumentKind::Integer:
    Out << Arg.getAsInteger();
    break;

  case DiagnosticArgumentKind::Unsigned:
    Out << Arg.getAsUnsigned();
    break;

  case DiagnosticArgumentKind::String:
    Out << Arg.getAsString();
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
      // FIXME: Actually do this properly when we need it for something.
      unsigned Length = 1;
      while (InText[Length] != '}')
        ++Length;
      ModifierArguments = InText.substr(1, Length - 1);
      InText = InText.substr(Length + 1);
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
                             
void DiagnosticEngine::diagnose(SMLoc Loc, DiagID ID, 
                                ArrayRef<DiagnosticArgument> Args) {
  const DiagnosticInfo &Info = DiagnosticInfos[(unsigned)ID];
  
  // Determine what kind of diagnostic we're emitting.
  const char *Kind;
  switch (Info.Kind) {
  case DiagnosticKind::Error: Kind = "error"; break;
  case DiagnosticKind::Warning: Kind = "warning"; break;
  case DiagnosticKind::Note: Kind = "note"; break;
  }
  
  // Actually substitute the diagnostic arguments into the diagnostic text.
  llvm::SmallString<256> Text;
  formatDiagnosticText(Info.Text, Args, Text);
  
  // Display the diagnostic.
  SourceMgr.PrintMessage(Loc, StringRef(Text), Kind);
}
