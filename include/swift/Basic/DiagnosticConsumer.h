//===- DiagnosticConsumer.h - Diagnostic Consumer Interface -----*- C++ -*-===//
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
//  This file declares the DiagnosticConsumer class, which receives callbacks
//  whenever the front end emits a diagnostic and is responsible for presenting
//  or storing that diagnostic (whatever is appropriate).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIAGNOSTIC_CONSUMER_H
#define SWIFT_BASIC_DIAGNOSTIC_CONSUMER_H

#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace llvm {
  class SourceMgr;
}

namespace swift {
  
/// \brief Describes the kind of diagnostic.
///
enum class DiagnosticKind {
  Error,
  Warning,
  Note
};

/// \brief Extra information carried along with a diagnostic, which may or
/// may not be of interest to a given diagnostic consumer.
struct DiagnosticInfo {
  /// Represents a source range that can either be a half-open character range
  /// (like llvm::SMRange) or a closed token range (like SourceRange).
  ///
  /// Diagnostics need to be able to handle both kinds of ranges.
  class Range {
  public:
    SourceLoc Start, End;
    bool IsTokenRange;

    Range(SourceLoc S, SourceLoc E, bool TokenRange = false)
      : Start(S), End(E), IsTokenRange(TokenRange) {}

    /// Force callers to choose whether they want a zero-length range at \p S,
    /// a one-character range at \p S, or a range consisting of the token at
    /// \p S.
    /*implicit*/ Range(SourceLoc S) = delete;

    /*implicit*/ Range(SourceRange SR)
      : Start(SR.Start), End(SR.End), IsTokenRange(true) {}
  };

  /// Represents a fix-it, a replacement of one range of text with another.
  class FixIt {
    DiagnosticInfo::Range Range;
    std::string Text;

  public:
    FixIt(DiagnosticInfo::Range R, llvm::StringRef Str)
      : Range(R), Text(Str) {}

    DiagnosticInfo::Range getRange() const { return Range; }
    llvm::StringRef getText() const { return Text; }
  };

  /// \brief Extra source ranges that are attached to the diagnostic.
  llvm::ArrayRef<Range> Ranges;

  /// \brief Extra source ranges that are attached to the diagnostic.
  llvm::ArrayRef<FixIt> FixIts;
};
  
/// \brief Abstract interface for classes that present diagnostics to the user.
class DiagnosticConsumer {
public:
  virtual ~DiagnosticConsumer();
  
  /// \brief Invoked whenever the frontend emits a diagnostic.
  ///
  /// \param SM The source manager associated with the source locations in
  /// this diagnostic.
  ///
  /// \param Loc The source location associated with this diagnostic. This
  /// location may be invalid, if the diagnostic is not directly related to
  /// the source (e.g., if it comes from command-line parsing).
  ///
  /// \param Kind The severity of the diagnostic (error, warning, note).
  ///
  /// \param Text The diagnostic text.
  ///
  /// \param Info Extra information associated with the diagnostic.
  virtual void handleDiagnostic(llvm::SourceMgr &SM, SourceLoc Loc,
                                DiagnosticKind Kind, llvm::StringRef Text,
                                const DiagnosticInfo &Info) = 0;
};
  
/// \brief DiagnosticConsumer that discards all diagnostics.
class NullDiagnosticConsumer : public DiagnosticConsumer {
public:
  void handleDiagnostic(llvm::SourceMgr &SM, SourceLoc Loc,
                        DiagnosticKind Kind, llvm::StringRef Text,
                        const DiagnosticInfo &Info) override;
};
  
}

#endif
