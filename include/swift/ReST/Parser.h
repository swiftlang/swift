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

struct LangOptions {
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
};

class ReSTContext {
public:
  LangOptions LangOpts;
  llvm::BumpPtrAllocator Allocator;

  void *allocate(unsigned long Bytes, unsigned Alignment) {
    return Allocator.Allocate(Bytes, Alignment);
  }

  template <typename T, typename It>
  T *allocateCopy(It Begin, It End) const {
    T *Res =
        static_cast<T *>(allocate(sizeof(T) * (End - Begin), alignof(T)));
    for (unsigned i = 0; Begin != End; ++Begin, ++i)
      new (Res + i) T(*Begin);
    return Res;
  }

  template <typename T>
  MutableArrayRef<T> allocateCopy(ArrayRef<T> Array) const {
    return MutableArrayRef<T>(allocateCopy<T>(Array.begin(), Array.end()),
                              Array.size());
  }
};

Document *parseDocument(ReSTContext &C, LineListRef LL);

void convertToDocutilsXML(const Document *D, raw_ostream &OS);

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_PARSER_H

