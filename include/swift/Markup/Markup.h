//===--- Markup.h - Markup ------------------------------------------------===//
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

#ifndef LLVM_MARKUP_MARKUP_H
#define LLVM_MARKUP_MARKUP_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Markup/AST.h"
#include "swift/Markup/LineList.h"


namespace swift {
  struct RawComment;
}

namespace llvm {
namespace markup {

class LineList;

class MarkupContext final {
  llvm::BumpPtrAllocator Allocator;

public:
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

  LineList getLineList(swift::RawComment RC);
};

Document *parseDocument(MarkupContext &MC, LineList &LL);

} // namespace markup
} // namespace llvm

#endif // LLVM_MARKUP_MARKUP_H
