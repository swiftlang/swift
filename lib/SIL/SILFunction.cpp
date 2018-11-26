//===--- SILFunction.cpp - Defines the SILFunction data structure ---------===//
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

#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILProfiler.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/Basic/OptimizationMode.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/GraphWriter.h"

using namespace swift;
using namespace Lowering;

ArrayRef<Requirement> SILSpecializeAttr::getRequirements() const {
  return {const_cast<SILSpecializeAttr *>(this)->getRequirementsData(),
          numRequirements};
}

SILSpecializeAttr::SILSpecializeAttr(ArrayRef<Requirement> requirements,
                                     bool exported, SpecializationKind kind)
    : numRequirements(requirements.size()), kind(kind), exported(exported) {
  std::copy(requirements.begin(), requirements.end(), getRequirementsData());
}

SILSpecializeAttr *SILSpecializeAttr::create(SILModule &M,
                                             ArrayRef<Requirement> requirements,
                                             bool exported,
                                             SpecializationKind kind) {
  unsigned size =
      sizeof(SILSpecializeAttr) + sizeof(Requirement) * requirements.size();
  void *buf = M.allocate(size, alignof(SILSpecializeAttr));
  return ::new (buf) SILSpecializeAttr(requirements, exported, kind);
}

void SILFunction::addSpecializeAttr(SILSpecializeAttr *Attr) {
  if (getLoweredFunctionType()->getGenericSignature()) {
    Attr->F = this;
    SpecializeAttrSet.push_back(Attr);
  }
}

/// SWIFT_ENABLE_TENSORFLOW
SILDifferentiableAttr::
SILDifferentiableAttr(const SILAutoDiffIndices &indices,
                      StringRef primalName,
                      StringRef adjointName,
                      bool adjointIsPrimitive,
                      StringRef jvpName,
                      StringRef vjpName)
  : indices(indices), PrimalName(primalName), AdjointName(adjointName),
    AdjointIsPrimitive(adjointIsPrimitive), JVPName(jvpName), VJPName(vjpName)
    {}

SILDifferentiableAttr *
SILDifferentiableAttr::create(SILModule &M,
                              const SILAutoDiffIndices &indices,
                              StringRef primalName,
                              StringRef adjointName,
                              bool adjointIsPrimitive,
                              StringRef jvpName,
                              StringRef vjpName) {
  void *mem = M.allocate(sizeof(SILDifferentiableAttr),
                         alignof(SILDifferentiableAttr));
  return ::new (mem)
      SILDifferentiableAttr(indices, primalName, adjointName,
                            adjointIsPrimitive, jvpName, vjpName);
}

SILFunction *SILFunction::create(
    SILModule &M, SILLinkage linkage, StringRef name,
    CanSILFunctionType loweredType, GenericEnvironment *genericEnv,
    Optional<SILLocation> loc, IsBare_t isBareSILFunction,
    IsTransparent_t isTrans, IsSerialized_t isSerialized,
    ProfileCounter entryCount, IsThunk_t isThunk,
    SubclassScope classSubclassScope, Inline_t inlineStrategy, EffectsKind E,
    SILFunction *insertBefore, const SILDebugScope *debugScope) {
  // Get a StringMapEntry for the function.  As a sop to error cases,
  // allow the name to have an empty string.
  llvm::StringMapEntry<SILFunction*> *entry = nullptr;
  if (!name.empty()) {
    entry = &*M.FunctionTable.insert(std::make_pair(name, nullptr)).first;
    PrettyStackTraceSILFunction trace("creating", entry->getValue());
    assert(!entry->getValue() && "function already exists");
    name = entry->getKey();
  }

  auto fn = new (M) SILFunction(M, linkage, name, loweredType, genericEnv, loc,
                                isBareSILFunction, isTrans, isSerialized,
                                entryCount, isThunk, classSubclassScope,
                                inlineStrategy, E, insertBefore, debugScope);

  if (entry) entry->setValue(fn);
  return fn;
}

SILFunction::SILFunction(SILModule &Module, SILLinkage Linkage, StringRef Name,
                         CanSILFunctionType LoweredType,
                         GenericEnvironment *genericEnv,
                         Optional<SILLocation> Loc, IsBare_t isBareSILFunction,
                         IsTransparent_t isTrans, IsSerialized_t isSerialized,
                         ProfileCounter entryCount, IsThunk_t isThunk,
                         SubclassScope classSubclassScope,
                         Inline_t inlineStrategy, EffectsKind E,
                         SILFunction *InsertBefore,
                         const SILDebugScope *DebugScope)
    : Module(Module), Name(Name), LoweredType(LoweredType),
      GenericEnv(genericEnv), SpecializationInfo(nullptr),
      DebugScope(DebugScope), Bare(isBareSILFunction), Transparent(isTrans),
      Serialized(isSerialized), Thunk(isThunk),
      ClassSubclassScope(unsigned(classSubclassScope)), GlobalInitFlag(false),
      InlineStrategy(inlineStrategy), Linkage(unsigned(Linkage)),
      HasCReferences(false), IsWeakLinked(false),
      OptMode(OptimizationMode::NotSet), EffectsKindAttr(E),
      EntryCount(entryCount) {
  validateSubclassScope(classSubclassScope, isThunk, nullptr);

  if (InsertBefore)
    Module.functions.insert(SILModule::iterator(InsertBefore), this);
  else
    Module.functions.push_back(this);

  Module.removeFromZombieList(Name);

  // Set our BB list to have this function as its parent. This enables us to
  // splice efficiently basic blocks in between functions.
  BlockList.Parent = this;
}

SILFunction::~SILFunction() {
  // If the function is recursive, a function_ref inst inside of the function
  // will give the function a non-zero ref count triggering the assertion. Thus
  // we drop all instruction references before we erase.
  // We also need to drop all references if instructions are allocated using
  // an allocator that may recycle freed memory.
  dropAllReferences();

  auto &M = getModule();
  for (auto &BB : *this) {
    for (auto I = BB.begin(), E = BB.end(); I != E;) {
      auto Inst = &*I;
      ++I;
      SILInstruction::destroy(Inst);
      // TODO: It is only safe to directly deallocate an
      // instruction if this BB is being removed in scope
      // of destructing a SILFunction.
      M.deallocateInst(Inst);
    }
    BB.InstList.clearAndLeakNodesUnsafely();
  }

  assert(RefCount == 0 &&
         "Function cannot be deleted while function_ref's still exist");
}

void SILFunction::createProfiler(ASTNode Root, ForDefinition_t forDefinition) {
  assert(!Profiler && "Function already has a profiler");
  Profiler = SILProfiler::create(Module, forDefinition, Root);
}

bool SILFunction::hasForeignBody() const {
  if (!hasClangNode()) return false;
  return SILDeclRef::isClangGenerated(getClangNode());
}

void SILFunction::numberValues(llvm::DenseMap<const SILNode*, unsigned> &
                                 ValueToNumberMap) const {
  unsigned idx = 0;
  for (auto &BB : *this) {
    for (auto I = BB.args_begin(), E = BB.args_end(); I != E; ++I)
      ValueToNumberMap[*I] = idx++;
    
    for (auto &I : BB) {
      auto results = I.getResults();
      if (results.empty()) {
        ValueToNumberMap[&I] = idx++;
      } else {
        // Assign the instruction node the first result ID.
        ValueToNumberMap[&I] = idx;
        for (auto result : results) {
          ValueToNumberMap[result] = idx++;
        }
      }
    }
  }
}


ASTContext &SILFunction::getASTContext() const {
  return getModule().getASTContext();
}

OptimizationMode SILFunction::getEffectiveOptimizationMode() const {
  if (OptMode != OptimizationMode::NotSet)
    return OptMode;

  return getModule().getOptions().OptMode;
}

bool SILFunction::shouldOptimize() const {
  return getEffectiveOptimizationMode() != OptimizationMode::NoOptimization;
}

Type SILFunction::mapTypeIntoContext(Type type) const {
  return GenericEnvironment::mapTypeIntoContext(
      getGenericEnvironment(), type);
}

SILType SILFunction::mapTypeIntoContext(SILType type) const {
  if (auto *genericEnv = getGenericEnvironment())
    return genericEnv->mapTypeIntoContext(getModule(), type);
  return type;
}

SILType GenericEnvironment::mapTypeIntoContext(SILModule &M,
                                               SILType type) const {
  assert(!type.hasArchetype());

  auto genericSig = getGenericSignature()->getCanonicalSignature();
  return type.subst(M,
                    QueryInterfaceTypeSubstitutions(this),
                    LookUpConformanceInSignature(*genericSig),
                    genericSig);
}

bool SILFunction::isNoReturnFunction() const {
  return SILType::getPrimitiveObjectType(getLoweredFunctionType())
      .isNoReturnFunction();
}

SILBasicBlock *SILFunction::createBasicBlock() {
  return new (getModule()) SILBasicBlock(this, nullptr, false);
}

SILBasicBlock *SILFunction::createBasicBlockAfter(SILBasicBlock *afterBB) {
  assert(afterBB);
  return new (getModule()) SILBasicBlock(this, afterBB, /*after*/ true);
}

SILBasicBlock *SILFunction::createBasicBlockBefore(SILBasicBlock *beforeBB) {
  assert(beforeBB);
  return new (getModule()) SILBasicBlock(this, beforeBB, /*after*/ false);
}

//===----------------------------------------------------------------------===//
//                          View CFG Implementation
//===----------------------------------------------------------------------===//

#ifndef NDEBUG

static llvm::cl::opt<unsigned>
MaxColumns("view-cfg-max-columns", llvm::cl::init(80),
           llvm::cl::desc("Maximum width of a printed node"));

namespace {
enum class LongLineBehavior { None, Truncate, Wrap };
} // end anonymous namespace
static llvm::cl::opt<LongLineBehavior>
LLBehavior("view-cfg-long-line-behavior",
           llvm::cl::init(LongLineBehavior::Truncate),
           llvm::cl::desc("Behavior when line width is greater than the "
                          "value provided my -view-cfg-max-columns "
                          "option"),
           llvm::cl::values(
               clEnumValN(LongLineBehavior::None, "none", "Print everything"),
               clEnumValN(LongLineBehavior::Truncate, "truncate",
                          "Truncate long lines"),
               clEnumValN(LongLineBehavior::Wrap, "wrap", "Wrap long lines")));

static llvm::cl::opt<bool>
RemoveUseListComments("view-cfg-remove-use-list-comments",
                      llvm::cl::init(false),
                      llvm::cl::desc("Should use list comments be removed"));

template <typename InstTy, typename CaseValueTy>
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
template <>
struct DOTGraphTraits<SILFunction *> : public DefaultDOTGraphTraits {

  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getGraphName(SILFunction *F) {
    return "CFG for '" + F->getName().str() + "' function";
  }

  static std::string getSimpleNodeLabel(SILBasicBlock *Node, SILFunction *F) {
    std::string OutStr;
    raw_string_ostream OSS(OutStr);
    const_cast<SILBasicBlock *>(Node)->printAsOperand(OSS, false);
    return OSS.str();
  }

  static std::string getCompleteNodeLabel(SILBasicBlock *Node, SILFunction *F) {
    std::string Str;
    raw_string_ostream OS(Str);

    OS << *Node;
    std::string OutStr = OS.str();
    if (OutStr[0] == '\n')
      OutStr.erase(OutStr.begin());

    // Process string output to make it nicer...
    unsigned ColNum = 0;
    unsigned LastSpace = 0;
    for (unsigned i = 0; i != OutStr.length(); ++i) {
      if (OutStr[i] == '\n') { // Left justify
        OutStr[i] = '\\';
        OutStr.insert(OutStr.begin() + i + 1, 'l');
        ColNum = 0;
        LastSpace = 0;
      } else if (RemoveUseListComments && OutStr[i] == '/' &&
                 i != (OutStr.size() - 1) && OutStr[i + 1] == '/') {
        unsigned Idx = OutStr.find('\n', i + 1); // Find end of line
        OutStr.erase(OutStr.begin() + i, OutStr.begin() + Idx);
        --i;

      } else if (ColNum == MaxColumns) { // Handle long lines.

        if (LLBehavior == LongLineBehavior::Wrap) {
          if (!LastSpace)
            LastSpace = i;
          OutStr.insert(LastSpace, "\\l...");
          ColNum = i - LastSpace;
          LastSpace = 0;
          i += 3; // The loop will advance 'i' again.
        } else if (LLBehavior == LongLineBehavior::Truncate) {
          unsigned Idx = OutStr.find('\n', i + 1); // Find end of line
          OutStr.erase(OutStr.begin() + i, OutStr.begin() + Idx);
          --i;
        }

        // Else keep trying to find a space.
      } else
        ++ColNum;
      if (OutStr[i] == ' ')
        LastSpace = i;
    }
    return OutStr;
  }

  std::string getNodeLabel(SILBasicBlock *Node, SILFunction *Graph) {
    if (isSimple())
      return getSimpleNodeLabel(Node, Graph);
    else
      return getCompleteNodeLabel(Node, Graph);
  }

  static std::string getEdgeSourceLabel(SILBasicBlock *Node,
                                        SILBasicBlock::succblock_iterator I) {
    const SILBasicBlock *Succ = *I;
    const TermInst *Term = Node->getTerminator();

    // Label source of conditional branches with "T" or "F"
    if (auto *CBI = dyn_cast<CondBranchInst>(Term))
      return (Succ == CBI->getTrueBB()) ? "T" : "F";

    // Label source of switch edges with the associated value.
    if (auto *SI = dyn_cast<SwitchValueInst>(Term)) {
      if (SI->hasDefault() && SI->getDefaultBB() == Succ)
        return "def";

      std::string Str;
      raw_string_ostream OS(Str);

      SILValue I = getCaseValueForBB<SwitchValueInst, SILValue>(SI, Succ);
      OS << I; // TODO: or should we output the literal value of I?
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

    if (auto *CCBI = dyn_cast<CheckedCastValueBranchInst>(Term))
      return (Succ == CCBI->getSuccessBB()) ? "T" : "F";

    if (auto *CCBI = dyn_cast<CheckedCastAddrBranchInst>(Term))
      return (Succ == CCBI->getSuccessBB()) ? "T" : "F";

    return "";
  }
};
} // namespace llvm
#endif

#ifndef NDEBUG
static llvm::cl::opt<std::string>
TargetFunction("view-cfg-only-for-function", llvm::cl::init(""),
               llvm::cl::desc("Only print out the cfg for this function"));
#endif

static void viewCFGHelper(const SILFunction* f, bool skipBBContents) {
/// When asserts are disabled, this should be a NoOp.
#ifndef NDEBUG
    // If we have a target function, only print that function out.
    if (!TargetFunction.empty() && !(f->getName().str() == TargetFunction))
      return;

    ViewGraph(const_cast<SILFunction *>(f), "cfg" + f->getName().str(),
              /*shortNames=*/skipBBContents);
#endif
}

void SILFunction::viewCFG() const {
  viewCFGHelper(this, /*skipBBContents=*/false);
}

void SILFunction::viewCFGOnly() const {
  viewCFGHelper(this, /*skipBBContents=*/true);
}


bool SILFunction::hasSelfMetadataParam() const {
  auto paramTypes = getConventions().getParameterSILTypes();
  if (paramTypes.empty())
    return false;

  auto silTy = *std::prev(paramTypes.end());
  if (!silTy.isObject())
    return false;

  auto selfTy = silTy.getASTType();

  if (auto metaTy = dyn_cast<MetatypeType>(selfTy)) {
    selfTy = metaTy.getInstanceType();
    if (auto dynamicSelfTy = dyn_cast<DynamicSelfType>(selfTy))
      selfTy = dynamicSelfTy.getSelfType();
  }

  return !!selfTy.getClassOrBoundGenericClass();
}

bool SILFunction::hasName(const char *Name) const {
  return getName() == Name;
}

/// Returns true if this function can be referenced from a fragile function
/// body.
bool SILFunction::hasValidLinkageForFragileRef() const {
  // Fragile functions can reference 'static inline' functions imported
  // from C.
  if (hasForeignBody())
    return true;

  // If we can inline it, we can reference it.
  if (hasValidLinkageForFragileInline())
    return true;

  // If the containing module has been serialized
  if (getModule().isSerialized()) {
    return true;
  }

  // Otherwise, only public functions can be referenced.
  return hasPublicVisibility(getLinkage());
}

bool
SILFunction::isPossiblyUsedExternally() const {
  auto linkage = getLinkage();

  // Hidden functions may be referenced by other C code in the linkage unit.
  if (linkage == SILLinkage::Hidden && hasCReferences())
    return true;

  return swift::isPossiblyUsedExternally(linkage, getModule().isWholeModule());
}

bool SILFunction::isExternallyUsedSymbol() const {
  return swift::isPossiblyUsedExternally(getEffectiveSymbolLinkage(),
                                         getModule().isWholeModule());
}

void SILFunction::convertToDeclaration() {
  assert(isDefinition() && "Can only convert definitions to declarations");
  dropAllReferences();
  getBlocks().clear();
}

SubstitutionMap SILFunction::getForwardingSubstitutionMap() {
  if (ForwardingSubMap)
    return ForwardingSubMap;

  if (auto *env = getGenericEnvironment())
    ForwardingSubMap = env->getForwardingSubstitutionMap();

  return ForwardingSubMap;
}

bool SILFunction::shouldVerifyOwnership() const {
  return !hasSemanticsAttr("verify.ownership.sil.never");
}

unsigned SILFunction::codeSize() const {
  unsigned size = 0;
  for (auto &BB : *this)
    size += BB.codeSize();
  return size;
}

// See swift/Basic/Statistic.h for declaration: this enables tracing
// SILFunctions, is defined here to avoid too much layering violation / circular
// linkage dependency.

struct SILFunctionTraceFormatter : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const {
    if (!Entity)
      return;
    const SILFunction *F = static_cast<const SILFunction *>(Entity);
    F->printName(OS);
  }

  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const {
    if (!Entity)
      return;
    const SILFunction *F = static_cast<const SILFunction *>(Entity);
    if (!F->hasLocation())
      return;
    F->getLocation().getSourceRange().print(OS, *SM, false);
  }
};

static SILFunctionTraceFormatter TF;

template<>
const UnifiedStatsReporter::TraceFormatter*
FrontendStatsTracer::getTraceFormatter<const SILFunction *>() {
  return &TF;
}
