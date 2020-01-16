//===--- Located.cpp - Source Location and Associated Value ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Located.h"

using namespace swift;

template<typename T>
void Located<T>::dump() const {
  dump(llvm::errs());
}

template<typename T>
void Located<T>::dump(raw_ostream &os) const {
  // FIXME: The following does not compile on newer clangs because operator<<
  // does not exist for SourceLoc. More so, the operator does not exist because
  // one needs a SourceManager reference and buffer ID to convert any given
  // SourceLoc into line and column information.
  //os << Loc << " " << Item;
}
