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

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace swift {
  class DiagnosticArgument;
  class SourceManager;
  enum class DiagID : uint32_t;

/// \brief Describes the kind of diagnostic.
///
enum class DiagnosticKind : uint8_t {
  Error,
  Warning,
  Remark,
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
  /// \param FormatArgs The diagnostic format string arguments.
  ///
  /// \param Info Extra information associated with the diagnostic.
  virtual void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                                DiagnosticKind Kind,
                                StringRef FormatString,
                                ArrayRef<DiagnosticArgument> FormatArgs,
                                const DiagnosticInfo &Info) = 0;

  /// \returns true if an error occurred while finishing-up.
  virtual bool finishProcessing() { return false; }
};
  
/// \brief DiagnosticConsumer that discards all diagnostics.
class NullDiagnosticConsumer : public DiagnosticConsumer {
public:
  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override;
};

/// \brief DiagnosticConsumer that funnels diagnostics in certain files to
/// particular sub-consumers.
///
/// The intended use case for such a consumer is "batch mode" compilations,
/// where we want to record diagnostics for each file as if they were compiled
/// separately. This is important for incremental builds, so that if a file has
/// warnings but doesn't get recompiled in the next build, the warnings persist.
///
/// Diagnostics that are not in one of the special files are emitted into every
/// sub-consumer. This is necessary to deal with, for example, diagnostics in a
/// bridging header imported from Objective-C, which isn't really about the
/// current file.
class FileSpecificDiagnosticConsumer : public DiagnosticConsumer {
public:
  /// A diagnostic consumer, along with the name of the buffer that it should
  /// be associated with. An empty string means that a consumer is not
  /// associated with any particular buffer, and should only receive diagnostics
  /// that are not in any of the other consumers' files.
  using ConsumerPair =
      std::pair<std::string, std::unique_ptr<DiagnosticConsumer>>;

private:
  /// All consumers owned by this FileSpecificDiagnosticConsumer.
  const SmallVector<ConsumerPair, 4> SubConsumers;

  using ConsumersOrderedByRangeEntry =
    std::pair<CharSourceRange, DiagnosticConsumer *>;

  /// The consumers owned by this FileSpecificDiagnosticConsumer, sorted by
  /// the end locations of each file so that a lookup by position can be done
  /// using binary search.
  ///
  /// Generated and cached when the first diagnostic with a location is emitted.
  /// This allows diagnostics to be emitted before files are actually opened,
  /// as long as they don't have source locations.
  ///
  /// \see #consumerForLocation
  SmallVector<ConsumersOrderedByRangeEntry, 4> ConsumersOrderedByRange;

  /// Indicates which consumer to send Note diagnostics too.
  ///
  /// Notes are always considered attached to the error, warning, or remark
  /// that was most recently emitted.
  ///
  /// If null, Note diagnostics are sent to every consumer.
  DiagnosticConsumer *ConsumerForSubsequentNotes = nullptr;

public:
  /// Takes ownership of the DiagnosticConsumers specified in \p consumers.
  ///
  /// There must not be two consumers for the same file (i.e., having the same
  /// buffer name).
  explicit FileSpecificDiagnosticConsumer(
      SmallVectorImpl<ConsumerPair> &consumers);

  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override;

   bool finishProcessing() override;

private:
  void computeConsumersOrderedByRange(SourceManager &SM);
  DiagnosticConsumer *consumerForLocation(SourceManager &SM,
                                          SourceLoc loc) const;
};
  
} // end namespace swift

#endif // SWIFT_BASIC_DIAGNOSTICCONSUMER_H
