//===--- SILPrintContext.h - Context for SIL print functions ----*- C++ -*-===//
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

#ifndef SWIFT_SIL_PRINTCONTEXT_H
#define SWIFT_SIL_PRINTCONTEXT_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class SILDebugScope;
class SILInstruction;
class SILFunction;

/// Used as context for the SIL print functions.
class SILPrintContext {
protected:
  llvm::raw_ostream &OutStream;

  llvm::DenseMap<const SILDebugScope *, unsigned> ScopeToIDMap;

  /// Dump more information in the SIL output.
  bool Verbose;
  
  /// Sort all kind of tables to ease diffing.
  bool SortedSIL;

public:
  SILPrintContext(llvm::raw_ostream &OS, bool Verbose = false,
                  bool SortedSIL = false) :
        OutStream(OS), Verbose(Verbose), SortedSIL(SortedSIL) { }

  virtual ~SILPrintContext();

  /// Returns the output stream for printing.
  llvm::raw_ostream &OS() const { return OutStream; }

  /// Returns true if the SIL output should be sorted.
  bool sortSIL() const { return SortedSIL; }
  
  /// Returns true if verbose SIL should be printed.
  bool printVerbose() const { return Verbose; }

  /// Returns true if the \p Scope has and ID assigned.
  bool hasScopeID(const SILDebugScope *Scope) const {
    return ScopeToIDMap.count(Scope) != 0;
  }

  /// Returns the ID of \p Scope.
  unsigned getScopeID(const SILDebugScope *Scope) const {
    return ScopeToIDMap.lookup(Scope);
  }

  /// Assigns the next available ID to \p Scope.
  unsigned assignScopeID(const SILDebugScope *Scope) {
    assert(!hasScopeID(Scope));
    unsigned ID = ScopeToIDMap.size() + 1;
    ScopeToIDMap.insert({Scope, ID});
    return ID;
  }

  /// Callback which is invoked by the SILPrinter before the instruction \p I
  /// is written.
  virtual void printInstructionCallBack(const SILInstruction *I);
};

} // end namespace swift

#endif /* SWIFT_SIL_PRINTCONTEXT_H */
