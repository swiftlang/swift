//===--- Parser.h - ReST parser -------------------------------------------===//
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
///
/// \file
/// This is a ReST parser.
///
/// Specification:
/// http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
///
/// Level of feature support:
///
///   [ ] Unsupported
///   [E] Unsupported now, but expected to be supported
///   [P] Partial support
///   [X] Fully supported
///
/// * Whitespace [X]
///   - Blank Lines [X]
///   - Indentation [X]
/// * Escaping Mechanism [E]
/// * Reference Names [ ]
/// * Document Structure
///   - Document [P] (missing metadata)
///   - Sections [ ]
///   - Transitions [ ]
/// * Body Elements
///   - Paragraphs [X]
///   - Bullet Lists [X]
///   - Enumerated Lists [P] (missing checks that items have consecutive
///     numbers)
///   - Definition Lists [X]
///   - Field Lists [X]
///     + Bibliographic Fields [ ]
///     + RCS Keywords [ ]
///   - Option Lists [ ]
///   - Literal Blocks [E] (will be implemented when inline markup is
///     implemented, because recognizing literal blocks involves
///     scanning paragraph text for "::" in a specific way, just like
///     inline markup)
///     + Indented Literal Blocks [E]
///     + Quoted Literal Blocks [E]
///   - Line Blocks [ ]
///   - Block Quotes [X]
///   - Doctest Blocks [ ]
///   - Tables [ ]
///     + Grid Tables [ ]
///     + Simple Tables [ ]
///   - Explicit Markup Blocks [ ]
///     + Footnotes [ ]
///       - Auto-Numbered Footnotes [ ]
///       - Auto-Symbol Footnotes [ ]
///       - Mixed Manual and Auto-Numbered Footnotes [ ]
///     + Citations [ ]
///     + Hyperlink Targets [ ]
///     + Anonymous Hyperlinks [ ]
///     + Directives [ ]
///     + Substitution Definitions [ ]
///     + Comments [ ]
///     + Implicit Hyperlink Targets [ ]
/// * Inline Markup [E]
///   - Inline markup recognition rules [E]
///   - Recognition order [E]
///   - Character-Level Inline Markup [E]
///   - Emphasis [E]
///   - Strong Emphasis [E]
///   - Interpreted Text [E]
///   - Inline Literals [E]
///   - Hyperlink References [E]
///     + Embedded URIs and Aliases [E]
///   - Inline Internal Targets [ ]
///   - Footnote References [ ]
///   - Citation References [ ]
///   - Substitution References [ ]
///   - Standalone Hyperlinks [ ]
/// * Units [ ]
///   - Length Units [ ]
///   - Percentage Units [ ]
//===----------------------------------------------------------------------===//

#ifndef LLVM_REST_PARSER_H
#define LLVM_REST_PARSER_H

#include "swift/ReST/LineList.h"
#include "swift/ReST/AST.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
namespace rest {
namespace detail {

LineClassification classifyLine(const Line &L);

} // namespace detail

void extractBrief(LineListRef LL, llvm::SmallVectorImpl<char> &Str);

struct LangOptions final {
  /// If set to true, then if all input lines are uniformly indented by the
  /// same amount, ignore that amount of indentation everywhere.
  ///
  /// If set to false, then the following example, where the vertical bar
  /// represents the column zero, is parsed as a paragraph inside a block
  /// quote.  This is the default because this behavior is strictly conforming
  /// to the ReST spec.
  ///
  /// \code
  ///   |  aaa
  /// \code
  bool IgnoreUniformIndentation = false;
  bool TemporaryHacks = false;
};

class ReSTContext final {
public:
  LangOptions LangOpts;
  llvm::BumpPtrAllocator Allocator;

  void *allocate(unsigned long Bytes, unsigned Alignment) {
    return Allocator.Allocate(Bytes, Alignment);
  }

  template <typename T, typename It>
  T *allocateCopy(It Begin, It End) {
    T *Res =
        static_cast<T *>(allocate(sizeof(T) * (End - Begin), alignof(T)));
    for (unsigned i = 0; Begin != End; ++Begin, ++i)
      new (Res + i) T(*Begin);
    return Res;
  }

  template <typename T>
  MutableArrayRef<T> allocateCopy(ArrayRef<T> Array) {
    return MutableArrayRef<T>(allocateCopy<T>(Array.begin(), Array.end()),
                              Array.size());
  }

  StringRef allocateCopy(StringRef Str) {
    ArrayRef<char> Result =
        allocateCopy(llvm::makeArrayRef(Str.data(), Str.size()));
    return StringRef(Result.data(), Result.size());
  }
};

Document *parseDocument(ReSTContext &C, LineListRef LL);

void convertToDocutilsXML(const Document *D, raw_ostream &OS);

Optional<std::pair<LinePart, LinePart>> extractWord(LinePart LP);
Optional<LinePart> extractWord(TextAndInline *TAI);

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_PARSER_H

