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

namespace llvm {
  class SourceMgr;
  class StringRef;
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
  /// \brief Extra source ranges that are attached to the diagnostic.
  llvm::ArrayRef<SourceRange> Ranges;
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
