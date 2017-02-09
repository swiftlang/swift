//===--- DiagnosticConsumer.h - Diagnostic Consumer Interface ---*- C++ -*-===//
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
//
//  This file declares the DiagnosticConsumer class, which receives callbacks
//  whenever the front end emits a diagnostic and is responsible for presenting
//  or storing that diagnostic (whatever is appropriate).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIAGNOSTICCONSUMER_H
#define SWIFT_BASIC_DIAGNOSTICCONSUMER_H

#include "swift/Basic/SourceLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace swift {
  class SourceManager;
  enum class DiagID : uint32_t;

/// \brief Describes the kind of diagnostic.
///
enum class DiagnosticKind : uint8_t {
  Error,
  Warning,
  Note
};

/// \brief Extra information carried along with a diagnostic, which may or
/// may not be of interest to a given diagnostic consumer.
struct DiagnosticInfo {
  DiagID ID = DiagID(0);

  /// Represents a fix-it, a replacement of one range of text with another.
  class FixIt {
    CharSourceRange Range;
    std::string Text;

  public:
    FixIt(CharSourceRange R, StringRef Str)
        : Range(R), Text(Str) {}

    CharSourceRange getRange() const { return Range; }
    StringRef getText() const { return Text; }
  };

  /// \brief Extra source ranges that are attached to the diagnostic.
  ArrayRef<CharSourceRange> Ranges;

  /// \brief Extra source ranges that are attached to the diagnostic.
  ArrayRef<FixIt> FixIts;
};
  
/// \brief Abstract interface for classes that present diagnostics to the user.
class DiagnosticConsumer {
protected:
  static llvm::SMLoc getRawLoc(SourceLoc Loc);

  static llvm::SMRange getRawRange(SourceManager &SM, CharSourceRange R) {
    return llvm::SMRange(getRawLoc(R.getStart()), getRawLoc(R.getEnd()));
  }

  static llvm::SMFixIt getRawFixIt(SourceManager &SM, DiagnosticInfo::FixIt F) {
    // FIXME: It's unfortunate that we have to copy the replacement text.
    return llvm::SMFixIt(getRawRange(SM, F.getRange()), F.getText());
  }

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
  virtual void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                                DiagnosticKind Kind, StringRef Text,
                                const DiagnosticInfo &Info) = 0;
};
  
/// \brief DiagnosticConsumer that discards all diagnostics.
class NullDiagnosticConsumer : public DiagnosticConsumer {
public:
  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind, StringRef Text,
                        const DiagnosticInfo &Info) override;
};
  
} // end namespace swift

#endif // SWIFT_BASIC_DIAGNOSTICCONSUMER_H
