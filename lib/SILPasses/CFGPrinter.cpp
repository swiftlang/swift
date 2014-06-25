//===-- CFGPrinter.cpp - CFG printer pass ---------------------------------===//
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
// This file defines external functions that can be called to explicitly
// instantiate the CFG printer.
//
//===----------------------------------------------------------------------===//

#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILFunction.h"
#include "llvm/Support/GraphWriter.h"

using namespace swift;

template<typename InstTy, typename CaseValueTy>
inline CaseValueTy getCaseValueForBB(const InstTy *Inst,
                                     const SILBasicBlock *BB) {
  for (unsigned i = 0, e = Inst->getNumCases(); i != e; ++i) {
    auto P = Inst->getCase(i);
    if (P.second != BB)
      continue;
    return P.first;
  }
  llvm_unreachable("Error! should never pass in BB that is not a successor");
}

namespace llvm {
template<>
struct DOTGraphTraits<SILFunction *> : public DefaultDOTGraphTraits {

  DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getGraphName(const SILFunction *F) {
    return "CFG for '" + F->getName().str() + "' function";
  }

  static std::string getSimpleNodeLabel(const SILBasicBlock *Node,
                                        const SILFunction *F) {
    std::string OutStr;
    raw_string_ostream OSS(OutStr);
    const_cast<SILBasicBlock *>(Node)->printAsOperand(OSS, false);
    return OSS.str();
  }

  static std::string getCompleteNodeLabel(const SILBasicBlock *Node,
                                          const SILFunction *F) {
    enum { MaxColumns = 80 };
    std::string Str;
    raw_string_ostream OS(Str);

    OS << *Node;
    std::string OutStr = OS.str();
    if (OutStr[0] == '\n') OutStr.erase(OutStr.begin());

    // Process string output to make it nicer...
    unsigned ColNum = 0;
    unsigned LastSpace = 0;
    for (unsigned i = 0; i != OutStr.length(); ++i) {
      if (OutStr[i] == '\n') {                            // Left justify
        OutStr[i] = '\\';
        OutStr.insert(OutStr.begin()+i+1, 'l');
        ColNum = 0;
        LastSpace = 0;
      } else if (OutStr[i] == ';') {                      // Delete comments!
        unsigned Idx = OutStr.find('\n', i+1);            // Find end of line
        OutStr.erase(OutStr.begin()+i, OutStr.begin()+Idx);
        --i;
      } else if (ColNum == MaxColumns) {                  // Wrap lines.
        if (LastSpace) {
          OutStr.insert(LastSpace, "\\l...");
          ColNum = i - LastSpace;
          LastSpace = 0;
          i += 3; // The loop will advance 'i' again.
        }
        // Else keep trying to find a space.
      }
      else
        ++ColNum;
      if (OutStr[i] == ' ')
        LastSpace = i;
    }
    return OutStr;
  }

  std::string getNodeLabel(const SILBasicBlock *Node,
                           const SILFunction *Graph) {
    if (isSimple())
      return getSimpleNodeLabel(Node, Graph);
    else
      return getCompleteNodeLabel(Node, Graph);
  }

  static std::string getEdgeSourceLabel(const SILBasicBlock *Node,
                                        SILBasicBlock::const_succ_iterator I) {
    SILBasicBlock *Succ = I->getBB();
    const TermInst *Term = Node->getTerminator();

    // Label source of conditional branches with "T" or "F"
    if (auto *CBI = dyn_cast<CondBranchInst>(Term))
        return (Succ == CBI->getTrueBB()) ? "T" : "F";

    // Label source of switch edges with the associated value.
    if (auto *SI = dyn_cast<SwitchIntInst>(Term)) {
      if (SI->hasDefault() && SI->getDefaultBB() == Succ)
        return "def";

      std::string Str;
      raw_string_ostream OS(Str);

      APInt I = getCaseValueForBB<SwitchIntInst, APInt>(SI, Succ);
      OS << I;
      return OS.str();
    }

    if (auto *SEIB = dyn_cast<SwitchEnumInst>(Term)) {
      std::string Str;
      raw_string_ostream OS(Str);

      EnumElementDecl *E =
        getCaseValueForBB<SwitchEnumInst, EnumElementDecl *>(SEIB, Succ);
      OS << E->getFullName();
      return OS.str();
    }

    if (auto *SEIB = dyn_cast<SwitchEnumAddrInst>(Term)) {
      std::string Str;
      raw_string_ostream OS(Str);

      EnumElementDecl *E =
        getCaseValueForBB<SwitchEnumAddrInst, EnumElementDecl *>(SEIB, Succ);
      OS << E->getFullName();
      return OS.str();
    }

    if (auto *DMBI = dyn_cast<DynamicMethodBranchInst>(Term))
        return (Succ == DMBI->getHasMethodBB()) ? "T" : "F";

    if (auto *CCBI = dyn_cast<CheckedCastBranchInst>(Term))
        return (Succ == CCBI->getSuccessBB()) ? "T" : "F";

    if (auto *CCBI = dyn_cast<CheckedCastAddrBranchInst>(Term))
        return (Succ == CCBI->getSuccessBB()) ? "T" : "F";

    return "";
  }
};
} // end llvm namespace

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {
class SILCFGPrinter : public SILFunctionTransform {
  StringRef getName() override { return "SIL CFG Printer"; }

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();
    ViewGraph(F, "cfg" + F->getName().str());
  }
};
} // end anonymous namespace


SILTransform *swift::createSILCFGPrinter() {
  return new SILCFGPrinter();
}
