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

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class SILDebugScope;
class SILInstruction;
class SILFunction;
class SILBasicBlock;

/// Used as context for the SIL print functions.
class SILPrintContext {
public:
  struct ID {
    enum ID_Kind { SILBasicBlock, SILUndef, SSAValue } Kind;
    unsigned Number;

    // A stable ordering of ID objects.
    bool operator<(ID Other) const {
      if (unsigned(Kind) < unsigned(Other.Kind))
        return true;
      if (Number < Other.Number)
        return true;
      return false;
    }

    void print(raw_ostream &OS);
  };

protected:
  // Cache block and value identifiers for this function. This is useful in
  // general for identifying entities, not just emitting textual SIL.
  //
  // TODO: It would be more discplined for the caller to provide a function
  // context. That way it would be impossible for IDs to change meaning within
  // the caller's scope.
  struct SILPrintFunctionContext {
    const SILFunction *F = nullptr;
    llvm::DenseMap<const SILBasicBlock *, unsigned> BlocksToIDMap;
    llvm::DenseMap<const ValueBase *, unsigned> ValueToIDMap;
  };

  SILPrintFunctionContext FuncCtx;

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

  SILPrintFunctionContext &getFuncContext(const SILFunction *F);

  // Initialized block IDs from the order provided in `blocks`.
  void initBlockIDs(ArrayRef<const SILBasicBlock *> Blocks);

  /// Returns the output stream for printing.
  llvm::raw_ostream &OS() const { return OutStream; }

  /// Returns true if the SIL output should be sorted.
  bool sortSIL() const { return SortedSIL; }
  
  /// Returns true if verbose SIL should be printed.
  bool printVerbose() const { return Verbose; }

  SILPrintContext::ID getID(const SILBasicBlock *Block);

  SILPrintContext::ID getID(SILValue V);

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

raw_ostream &operator<<(raw_ostream &OS, SILPrintContext::ID i);

} // end namespace swift

#endif /* SWIFT_SIL_PRINTCONTEXT_H */
