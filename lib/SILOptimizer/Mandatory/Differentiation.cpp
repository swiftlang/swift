//===--- Differentiation.cpp - SIL Automatic Differentiation --*- C++ -*---===//
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
// SWIFT_ENABLE_TENSORFLOW
//
// This file implements reverse-mode automatic differentiation.
//
// NOTE: Although the AD feature is developed as part of the Swift for
// TensorFlow project, it is completely independent from TensorFlow support.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "Differentiation.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/DenseSet.h"

using namespace swift;
using llvm::DenseMap;
using llvm::SmallDenseMap;
using llvm::SmallDenseSet;
using llvm::SmallMapVector;
using llvm::SmallSet;

/// This flag is used to disable `autodiff_function_extract` instruction folding
/// for SIL testing purposes.
static llvm::cl::opt<bool> SkipFoldingAutoDiffFunctionExtraction(
    "differentiation-skip-folding-autodiff-function-extraction",
    llvm::cl::init(false));

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
static raw_ostream &getADDebugStream() { return llvm::dbgs() << "[AD] "; }

/// Given a dumpable value, dumps it to `llvm::dbgs()`.
template <typename T> static inline void debugDump(T &v) {
  LLVM_DEBUG(llvm::dbgs() << "\n==== BEGIN DEBUG DUMP ====\n"
                          << v << "\n==== END DEBUG DUMP ====\n");
}

/// Creates arguments in the entry block based on the function type.
static void createEntryArguments(SILFunction *f) {
  auto *entry = f->getEntryBlock();
  auto conv = f->getConventions();
  auto &ctx = f->getASTContext();
  auto moduleDecl = f->getModule().getSwiftModule();
  assert((entry->getNumArguments() == 0 || conv.getNumSILArguments() == 0) &&
         "Entry already has arguments?!");
  auto createFunctionArgument = [&](SILType type) {
    // Create a dummy parameter declaration.
    // Necessary to prevent crash during argument explosion optimization.
    auto loc = f->getLocation().getSourceLoc();
    auto *decl = new (ctx)
        ParamDecl(VarDecl::Specifier::Default, loc, loc, Identifier(), loc,
                  Identifier(), moduleDecl);
    decl->setType(type.getASTType());
    entry->createFunctionArgument(type, decl);
  };
  for (auto indResTy : conv.getIndirectSILResultTypes())
    createFunctionArgument(f->mapTypeIntoContext(indResTy).getAddressType());
  for (auto paramTy : conv.getParameterSILTypes())
    createFunctionArgument(f->mapTypeIntoContext(paramTy));
}

static bool isWithoutDerivative(SILValue v) {
  if (auto *fnRef = dyn_cast<FunctionRefInst>(v))
    return fnRef->getReferencedFunctionOrNull()->hasSemanticsAttr(
        "autodiff.nonvarying");
  return false;
}

static ApplyInst *getAllocateUninitializedArrayIntrinsic(SILValue v) {
  if (auto *applyInst = dyn_cast<ApplyInst>(v))
    if (applyInst->hasSemantics("array.uninitialized_intrinsic"))
      return applyInst;
  return nullptr;
}

/// Given a function, gather all of its formal results (both direct and
/// indirect) in an order defined by its result type. Note that "formal results"
/// refer to result values in the body of the function, not at call sites.
static void
collectAllFormalResultsInTypeOrder(SILFunction &function,
                                   SmallVectorImpl<SILValue> &results) {
  SILFunctionConventions convs(function.getLoweredFunctionType(),
                               function.getModule());
  auto indResults = function.getIndirectResults();
  auto *retInst = cast<ReturnInst>(function.findReturnBB()->getTerminator());
  auto retVal = retInst->getOperand();
  SmallVector<SILValue, 8> dirResults;
  if (auto *tupleInst =
          dyn_cast_or_null<TupleInst>(retVal->getDefiningInstruction()))
    dirResults.append(tupleInst->getElements().begin(),
                      tupleInst->getElements().end());
  else
    dirResults.push_back(retVal);
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : convs.getResults())
    results.push_back(resInfo.isFormalDirect() ? dirResults[dirResIdx++]
                                               : indResults[indResIdx++]);
}

/// Given a function call site, gather all of its actual results (both direct
/// and indirect) in an order defined by its result type.
template <typename IndResRange>
static void collectAllActualResultsInTypeOrder(
    ApplyInst *ai, ArrayRef<SILValue> extractedDirectResults,
    IndResRange &&indirectResults, SmallVectorImpl<SILValue> &results) {
  auto callee = ai->getCallee();
  SILFunctionConventions calleeConvs(
      callee->getType().castTo<SILFunctionType>(), ai->getModule());
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : calleeConvs.getResults()) {
    results.push_back(resInfo.isFormalDirect()
                          ? extractedDirectResults[dirResIdx++]
                          : indirectResults[indResIdx++]);
  }
}

/// Given a range of types, joins these into a single type. If there's exactly
/// one element type, returns that element type. Otherwise, creates a tuple type
/// of all element types.
template <typename TypeRange>
static CanType joinElementTypes(TypeRange &&range, const ASTContext &ctx) {
  if (range.size() == 1)
    return range.front();
  auto typeElts =
      map<SmallVector<TupleTypeElt, 8>>(range, [&](Type type) { return type; });
  return TupleType::get(typeElts, ctx);
}

/// Given a range of SIL values, retrieves the canonical types of these values,
/// and joins these types into a single type.
template <typename SILValueRange>
static CanType joinElementTypesFromValues(SILValueRange &&range,
                                          const ASTContext &ctx) {
  if (range.size() == 1)
    return range.front()->getType().getASTType();
  SmallVector<TupleTypeElt, 8> elts;
  transform(range, elts.begin(),
            [&](SILValue val) { return val->getType().getASTType(); });
  return TupleType::get(elts, ctx)->getCanonicalType();
}

/// Given an operator name, such as '+', and a protocol, returns the '+'
/// operator. If the operator does not exist in the protocol, returns null.
static FuncDecl *findOperatorDeclInProtocol(DeclName operatorName,
                                            ProtocolDecl *protocol) {
  assert(operatorName.isOperator());
  // Find the operator requirement in the `VectorProtocol` protocol
  // declaration and cache it.
  auto opLookup = protocol->lookupDirect(operatorName);
  // Find the `+` with type siguature `(Self, Self) -> Self`.
  for (auto *decl : opLookup) {
    auto *fd = dyn_cast<FuncDecl>(decl);
    if (!fd || !fd->isStatic() || !fd->isOperator())
      continue;
    return fd;
  }
  // Not found.
  return nullptr;
}

/// Assuming the buffer is for indirect passing, returns the store ownership
/// qualifier for creating a `store` instruction into the buffer.
static StoreOwnershipQualifier getBufferSOQ(Type type, SILFunction &fn) {
  if (fn.hasOwnership())
    return fn.getModule().Types.getTypeLowering(
        type, ResilienceExpansion::Minimal).isTrivial()
            ? StoreOwnershipQualifier::Trivial
            : StoreOwnershipQualifier::Init;
  return StoreOwnershipQualifier::Unqualified;
}

/// Assuming the buffer is for indirect passing, returns the load ownership
/// qualified for creating a `load` instruction from the buffer.
static LoadOwnershipQualifier getBufferLOQ(Type type, SILFunction &fn) {
  if (fn.hasOwnership())
    return fn.getModule().Types.getTypeLowering(
        type, ResilienceExpansion::Minimal).isTrivial()
            ? LoadOwnershipQualifier::Trivial
            : LoadOwnershipQualifier::Take;
  return LoadOwnershipQualifier::Unqualified;
}

// Return the expected generic signature for autodiff associated functions given
// a SILDifferentiableAttr. The expected generic signature is built from the
// original generic signature and the attribute's requirements.
static CanGenericSignature
getAssociatedFunctionGenericSignature(SILDifferentiableAttr *attr,
                                      SILFunction *original) {
  auto originalGenSig =
      original->getLoweredFunctionType()->getGenericSignature();
  if (!originalGenSig)
    return nullptr;
  GenericSignatureBuilder builder(original->getASTContext());
  // Add original generic signature.
  builder.addGenericSignature(originalGenSig);
  // Add where clause requirements.
  auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  for (auto &req : attr->getRequirements())
    builder.addRequirement(req, source, original->getModule().getSwiftModule());
  return std::move(builder)
      .computeGenericSignature(SourceLoc(), /*allowConcreteGenericParams=*/true)
      ->getCanonicalSignature();
}

// Clone the generic parameters of the given generic signature and return a new
// `GenericParamList`.
static GenericParamList *cloneGenericParameters(ASTContext &ctx,
                                                DeclContext *dc,
                                                CanGenericSignature sig) {
  SmallVector<GenericTypeParamDecl *, 2> clonedParams;
  for (auto paramType : sig->getGenericParams()) {
    auto clonedParam = new (ctx) GenericTypeParamDecl(
        dc, paramType->getName(), SourceLoc(), paramType->getDepth(),
        paramType->getIndex());
    clonedParam->setDeclContext(dc);
    clonedParam->setImplicit(true);
    clonedParams.push_back(clonedParam);
  }
  return GenericParamList::create(ctx, SourceLoc(), clonedParams, SourceLoc());
}

/// Given an `autodiff_function` instruction, find the corresponding
/// differential operator used in the AST. If no differential operator is found,
/// return nullptr.
static AutoDiffFunctionExpr *
findDifferentialOperator(AutoDiffFunctionInst *inst) {
  return inst->getLoc().getAsASTNode<AutoDiffFunctionExpr>();
}

/// Returns the underlying instruction for the given SILValue, if it exists,
/// peering through function conversion instructions.
template<class Inst>
static Inst *peerThroughFunctionConversions(SILValue value) {
  if (auto *inst = dyn_cast<Inst>(value))
    return inst;
  if (auto *thinToThick = dyn_cast<ThinToThickFunctionInst>(value))
    return peerThroughFunctionConversions<Inst>(thinToThick->getOperand());
  if (auto *convertFn = dyn_cast<ConvertFunctionInst>(value))
    return peerThroughFunctionConversions<Inst>(convertFn->getOperand());
  if (auto *convertFn = dyn_cast<ConvertEscapeToNoEscapeInst>(value))
    return peerThroughFunctionConversions<Inst>(convertFn->getOperand());
  if (auto *partialApply = dyn_cast<PartialApplyInst>(value))
    return peerThroughFunctionConversions<Inst>(partialApply->getCallee());
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Auxiliary data structures
//===----------------------------------------------------------------------===//

namespace {
class ADContext;

/// The invoker of a differentiation task. It can be some user syntax, e.g.
/// an `autodiff_function` instruction lowered from an `AutoDiffFunctionExpr`
/// expression, the differentiation pass, or nothing at all. This will be used
/// to emit informative diagnostics.
struct DifferentiationInvoker {
public:
  /// The kind of the invoker of a differentiation task.
  enum class Kind {
    // Invoked by an `autodiff_function` instruction, which may or may not be
    // linked to a Swift AST node (e.g. an `AutoDiffFunctionExpr` expression).
    AutoDiffFunctionInst,

    // Invoked by the indirect application of differentiation. This case has an
    // associated original `apply` instruction and `[differentiable]` attribute.
    IndirectDifferentiation,

    // Invoker by a `[differentiable]` attribute in SIL **without** being linked
    // to a Swift AST attribute. This case has an associated `[differentiable]`
    // attribute.
    SILDifferentiableAttribute
  };

private:
  Kind kind;
  union Value {
    /// The instruction associated with the `AutoDiffFunctionInst` case.
    AutoDiffFunctionInst *adFuncInst;
    Value(AutoDiffFunctionInst *inst) : adFuncInst(inst) {}

    /// The parent `apply` instruction and `[differentiable]` attribute
    /// associated with the `IndirectDifferentiation` case.
    std::pair<ApplyInst *, SILDifferentiableAttr *>
        indirectDifferentiation;
    Value(ApplyInst *applyInst, SILDifferentiableAttr *attr)
        : indirectDifferentiation({applyInst, attr}) {}

    /// The `[differentiable]` attribute associated with the
    /// `SILDifferentiableAttribute` case.
    SILDifferentiableAttr *silDifferentiableAttribute;
    Value(SILDifferentiableAttr *attr) : silDifferentiableAttribute(attr) {}
  } value;

  /*implicit*/
  DifferentiationInvoker(Kind kind, Value value) : kind(kind), value(value) {}

public:
  DifferentiationInvoker(AutoDiffFunctionInst *inst)
      : kind(Kind::AutoDiffFunctionInst), value(inst) {}
  DifferentiationInvoker(ApplyInst *applyInst, SILDifferentiableAttr *attr)
      : kind(Kind::IndirectDifferentiation),
        value({applyInst, attr}) {}
  DifferentiationInvoker(SILDifferentiableAttr *attr)
      : kind(Kind::SILDifferentiableAttribute), value(attr) {}

  Kind getKind() const { return kind; }

  AutoDiffFunctionInst *getAutoDiffFunctionInst() const {
    assert(kind == Kind::AutoDiffFunctionInst);
    return value.adFuncInst;
  }

  std::pair<ApplyInst *, SILDifferentiableAttr *>
  getIndirectDifferentiation() const {
    assert(kind == Kind::IndirectDifferentiation);
    return value.indirectDifferentiation;
  }


  SILDifferentiableAttr *getSILDifferentiableAttribute() const {
    assert(kind == Kind::SILDifferentiableAttribute);
    return value.silDifferentiableAttribute;
  }

  SourceLoc getLocation() const {
    switch (kind) {
    case Kind::AutoDiffFunctionInst:
      return getAutoDiffFunctionInst()->getLoc().getSourceLoc();
    case Kind::IndirectDifferentiation:
      return getIndirectDifferentiation().first->getLoc().getSourceLoc();
    case Kind::SILDifferentiableAttribute:
      return getSILDifferentiableAttribute()->getOriginal()
          ->getLocation().getSourceLoc();
    }
  }

  void print(llvm::raw_ostream &os) const;
};

/// Information about the VJP function produced during VJP generation, e.g.
/// mappings from original values to corresponding values in the pullback
/// struct.
///
/// A pullback struct is an aggregate value containing pullbacks checkpointed
/// during the VJP computation. Pullback structs are generated for every
/// original function during VJP generation. Pullback struct values are
/// constructed by VJP functions and consumed by pullback functions.
class PullbackInfo {
private:
  /// The original function.
  SILFunction *const original;

  /// Mapping from original basic blocks to pullback structs.
  DenseMap<SILBasicBlock *, StructDecl *> pullbackStructs;

  /// Mapping from original basic blocks to predecessor enums.
  DenseMap<SILBasicBlock *, EnumDecl *> predecessorEnums;

  /// Mapping from `apply` and `struct_extract` instructions in the original
  /// function to the corresponding pullback declaration in the pullback struct.
  DenseMap<SILInstruction *, VarDecl *> pullbackValueMap;

  /// Mapping from predecessor+succcessor basic block pairs in original function
  /// to the corresponding predecessor enum case.
  DenseMap<std::pair<SILBasicBlock *, SILBasicBlock *>, EnumElementDecl *>
      predecessorEnumCases;

  /// Mapping from pullback structs to their predecessor enum fields.
  DenseMap<StructDecl *, VarDecl *> pullbackStructPredecessorFields;

  /// A type converter, used to compute struct/enum SIL types.
  Lowering::TypeConverter &typeConverter;

private:
  VarDecl *addVarDecl(NominalTypeDecl *nominal, StringRef name, Type type) {
    auto &astCtx = nominal->getASTContext();
    auto id = astCtx.getIdentifier(name);
    auto *varDecl = new (astCtx) VarDecl(
        /*IsStatic*/ false, VarDecl::Specifier::Var,
        /*IsCaptureList*/ false, SourceLoc(), id, nominal);
    varDecl->setAccess(nominal->getEffectiveAccess());
    if (type->hasArchetype())
      varDecl->setInterfaceType(type->mapTypeOutOfContext());
    else
      varDecl->setInterfaceType(type);
    nominal->addMember(varDecl);
    return varDecl;
  }

  /// Retrieves the file unit that contains implicit declarations in the
  /// current Swift module. If it does not exist, create one.
  ///
  // FIXME: Currently it defaults to the file containing `origFn`, if it can be
  // determined. Otherwise, it defaults to any file unit in the module. To
  // handle this more properly, we should make a DerivedFileUnit class to
  // contain all synthesized implicit type declarations.
  SourceFile &getDeclarationFileUnit() {
    if (original->hasLocation())
      if (auto *declContext = original->getLocation().getAsDeclContext())
        if (auto *parentSourceFile = declContext->getParentSourceFile())
          return *parentSourceFile;
    for (auto *file : original->getModule().getSwiftModule()->getFiles())
      if (auto *src = dyn_cast<SourceFile>(file))
        return *src;
    llvm_unreachable("No files?");
  }

  /// Compute and set the access level for the given pullback data structure,
  /// given the original function linkage.
  void computeAccessLevel(
      NominalTypeDecl *nominal, SILLinkage originalLinkage) {
    auto &astCtx = nominal->getASTContext();
    switch (originalLinkage) {
    case swift::SILLinkage::Public:
    case swift::SILLinkage::PublicNonABI:
      nominal->setAccess(AccessLevel::Internal);
      nominal->getAttrs().add(
          new (astCtx) UsableFromInlineAttr(/*Implicit*/ true));
      break;
    case swift::SILLinkage::Hidden:
    case swift::SILLinkage::Shared:
      nominal->setAccess(AccessLevel::Internal);
      break;
    case swift::SILLinkage::Private:
      nominal->setAccess(AccessLevel::FilePrivate);
      break;
    default:
      // When the original function has external linkage, we create an internal
      // struct for use by our own module. This is necessary for cross-cell
      // differentiation in Jupyter.
      // TODO: Add a test in the compiler that exercises a similar situation as
      // cross-cell differentiation in Jupyter.
      nominal->setAccess(AccessLevel::Internal);
    }
  }

  /// Creates an enum declaration with the given VJP generic signature, whose
  /// cases represent the predecessors of the given original block.
  EnumDecl *
  createBasicBlockPredecessorEnum(SILBasicBlock *originalBB,
                                  SILAutoDiffIndices indices,
                                  CanGenericSignature vjpGenericSig) {
    assert(originalBB->getParent() == original);
    auto *moduleDecl = original->getModule().getSwiftModule();
    auto &astCtx = original->getASTContext();
    auto &file = getDeclarationFileUnit();
    // Create a `_AD__<fn_name>_bb<bb_id>__Pred__` predecessor enum.
    std::string predEnumName =
        "_AD__" + original->getName().str() +
        "_bb" + std::to_string(originalBB->getDebugID()) +
         "__Pred__" + indices.mangle();
    auto enumId = astCtx.getIdentifier(predEnumName);
    auto loc = original->getLocation().getSourceLoc();
    auto *predecessorEnum = new (astCtx) EnumDecl(
        /*EnumLoc*/ loc, /*Name*/ enumId, /*NameLoc*/ loc, /*Inherited*/ {},
        /*GenericParams*/ /*set later*/ nullptr, /*DC*/ &file);
    if (vjpGenericSig) {
      auto *genericParams =
          cloneGenericParameters(astCtx, predecessorEnum, vjpGenericSig);
      predecessorEnum->setGenericParams(genericParams);
      predecessorEnum->setGenericEnvironment(
          vjpGenericSig->createGenericEnvironment());
    }
    predecessorEnum->setBraces(loc);
    computeAccessLevel(predecessorEnum, original->getEffectiveSymbolLinkage());
    predecessorEnum->computeType();
    assert(predecessorEnum->hasInterfaceType());
    file.addVisibleDecl(predecessorEnum);
    // Add predecessor block enum cases.
    for (auto *predBB : originalBB->getPredecessorBlocks()) {
      auto bbId = "bb" + std::to_string(predBB->getDebugID());
      auto *predPBStruct = getPullbackStruct(predBB);
      assert(predPBStruct);
      auto predPBStructTy =
          predPBStruct->getDeclaredInterfaceType()->getCanonicalType();
      // Create dummy declaration representing enum case parameter.
      auto *decl = new (astCtx)
          ParamDecl(VarDecl::Specifier::Default, loc, loc, Identifier(), loc,
                    Identifier(), moduleDecl);
      if (predPBStructTy->hasArchetype())
        decl->setInterfaceType(predPBStructTy->mapTypeOutOfContext());
      else
        decl->setInterfaceType(predPBStructTy);

      // Create enum element and enum case declarations.
      auto *paramList = ParameterList::create(astCtx, {decl});
      auto *enumEltDecl = new (astCtx) EnumElementDecl(
          /*IdentifierLoc*/ loc, DeclName(astCtx.getIdentifier(bbId)),
          paramList, loc, /*RawValueExpr*/ nullptr, predecessorEnum);
      enumEltDecl->setImplicit();
      enumEltDecl->computeType();
      auto *enumCaseDecl = EnumCaseDecl::create(
          /*CaseLoc*/ loc, {enumEltDecl}, predecessorEnum);
      enumCaseDecl->setImplicit();
      predecessorEnum->addMember(enumEltDecl);
      predecessorEnum->addMember(enumCaseDecl);
      // Cache predecessor/successor enum element declarations.
      predecessorEnumCases.insert({{predBB, originalBB}, enumEltDecl});
    }
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "Predecessor enum created for function @" << original->getName()
        << " bb" << originalBB->getDebugID() << '\n';
      predecessorEnum->print(s);
      s << '\n';
    });
    return predecessorEnum;
  }

  /// Creates a struct declaration with the given VJP generic signature, for
  /// storing the pullback values and predecessor of the given original block.
  StructDecl *
  createPullbackStruct(SILBasicBlock *originalBB, SILAutoDiffIndices indices,
                       CanGenericSignature vjpGenericSig) {
    auto *original = originalBB->getParent();
    auto &astCtx = original->getASTContext();
    auto &file = getDeclarationFileUnit();
    // Create a `_AD__<fn_name>_bb<bb_id>__PB__` struct.
    std::string pbStructName =
        "_AD__" + original->getName().str() +
        "_bb" + std::to_string(originalBB->getDebugID()) +
         "__PB__" + indices.mangle();
    auto structId = astCtx.getIdentifier(pbStructName);
    SourceLoc loc = original->getLocation().getSourceLoc();
    auto *pullbackStruct = new (astCtx) StructDecl(
        /*StructLoc*/ loc, /*Name*/ structId, /*NameLoc*/ loc, /*Inherited*/ {},
        /*GenericParams*/ /*set later*/ nullptr, /*DC*/ &file);
    if (vjpGenericSig) {
      auto *genericParams =
          cloneGenericParameters(astCtx, pullbackStruct, vjpGenericSig);
      pullbackStruct->setGenericParams(genericParams);
      pullbackStruct->setGenericEnvironment(
          vjpGenericSig->createGenericEnvironment());
    }
    pullbackStruct->setBraces(loc);
    computeAccessLevel(
        pullbackStruct, original->getEffectiveSymbolLinkage());
    pullbackStruct->computeType();
    assert(pullbackStruct->hasInterfaceType());
    file.addVisibleDecl(pullbackStruct);
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "Pullback struct created for function @" << original->getName()
        << " bb" << originalBB->getDebugID() << '\n';
      pullbackStruct->print(s);
      s << '\n';
    });
    return pullbackStruct;
  }

public:
  PullbackInfo(const PullbackInfo &) = delete;
  PullbackInfo &operator=(const PullbackInfo &) = delete;

  explicit PullbackInfo(ADContext &context, SILFunction *original,
                        SILFunction *vjp, const SILAutoDiffIndices &indices);

  /// Returns the pullback struct associated with the given original block.
  StructDecl *getPullbackStruct(SILBasicBlock *origBB) const {
    return pullbackStructs.lookup(origBB);
  }

  /// Returns the lowered SIL type of the pullback struct associated with the
  /// given original block.
  SILType getPullbackStructLoweredType(SILBasicBlock *origBB) const {
    auto *pbStruct = getPullbackStruct(origBB);
    auto pbStructType =
        pbStruct->getDeclaredInterfaceType()->getCanonicalType();
    return typeConverter.getLoweredType(pbStructType,
                                        ResilienceExpansion::Minimal);
  }

  /// Returns the predecessor enum associated with the given original block.
  EnumDecl *getPredecessorEnum(SILBasicBlock *origBB) const {
    return predecessorEnums.lookup(origBB);
  }

  /// Returns the lowered SIL type of the predecessor enum associated with the
  /// given original block.
  SILType getPredecessorEnumLoweredType(SILBasicBlock *origBB) const {
    auto *predEnum = getPredecessorEnum(origBB);
    auto predEnumType =
        predEnum->getDeclaredInterfaceType()->getCanonicalType();
    return typeConverter.getLoweredType(predEnumType,
                                        ResilienceExpansion::Minimal);
  }

  /// Returns the enum element in the given successor block's predecessor enum
  /// corresponding to the given predecessor block.
  EnumElementDecl *
  lookUpPredecessorEnumElement(SILBasicBlock *origPredBB,
                               SILBasicBlock *origSuccBB) const {
    assert(origPredBB->getParent() == original);
    return predecessorEnumCases.lookup({origPredBB, origSuccBB});
  }

  /// Returns the mapping from pullback structs to their predecessor enum
  /// fields.
  DenseMap<StructDecl *, VarDecl *> &getPullbackStructPredecessorFields() {
    return pullbackStructPredecessorFields;
  }

  /// Returns the predecessor enum field for the pullback struct of the given
  /// original block.
  VarDecl *lookUpPullbackStructPredecessorField(SILBasicBlock *origBB) {
    auto *pullbackStruct = getPullbackStruct(origBB);
    return pullbackStructPredecessorFields.lookup(pullbackStruct);
  }

  /// Add a pullback to the pullback struct.
  VarDecl *addPullbackDecl(SILInstruction *inst, SILType pullbackType) {
    // IRGen requires decls to have AST types (not `SILFunctionType`), so we
    // convert the `SILFunctionType` of the pullback to a `FunctionType` with
    // the same parameters and results.
    auto silFnTy = pullbackType.castTo<SILFunctionType>();
    SmallVector<AnyFunctionType::Param, 8> params;
    for (auto &param : silFnTy->getParameters())
      params.push_back(AnyFunctionType::Param(param.getType()));
    AnyFunctionType *astFnTy;
    if (auto genSig = silFnTy->getGenericSignature())
      astFnTy = GenericFunctionType::get(
          genSig, params, silFnTy->getAllResultsType().getASTType());
    else
      astFnTy = FunctionType::get(
          params, silFnTy->getAllResultsType().getASTType());

    auto *origBB = inst->getParent();
    auto *pbStruct = getPullbackStruct(origBB);
    auto pullbackName = "pullback_" + llvm::itostr(pullbackValueMap.size());
    auto *pullbackDecl = addVarDecl(pbStruct, pullbackName, astFnTy);
    pullbackValueMap.insert({inst, pullbackDecl});
    return pullbackDecl;
  }

  /// Finds the pullback declaration in the pullback struct for an `apply` or
  /// `struct_extract` in the original function.
  VarDecl *lookUpPullbackDecl(SILInstruction *inst) {
    auto lookup = pullbackValueMap.find(inst);
    return lookup == pullbackValueMap.end() ? nullptr
                                            : lookup->getSecond();
  }
};

/// Stores `apply` instruction information calculated by VJP generation.
struct NestedApplyInfo {
  /// The differentiation indices that are used to differentiate this `apply`
  /// instruction.
  SILAutoDiffIndices indices;
  /// The original pullback type before reabstraction. `None` if the pullback
  /// type is not reabstracted.
  Optional<CanSILFunctionType> originalPullbackType;
};

static inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                            DifferentiationInvoker invoker) {
  invoker.print(os);
  return os;
}

void DifferentiationInvoker::print(llvm::raw_ostream &os) const {
  os << "(differentiation_invoker ";
  switch (kind) {
  case Kind::AutoDiffFunctionInst:
    os << "autodiff_function_inst=(" << *getAutoDiffFunctionInst() << ")";
    break;
  case Kind::IndirectDifferentiation: {
    auto indDiff = getIndirectDifferentiation();
    os << "indirect_differentiation=(" << *std::get<0>(indDiff) << ')';
    // TODO: Enable printing parent invokers.
    // May require storing a `DifferentiableInvoker *` in the
    // `IndirectDifferentiation` case.
    /*
    SILInstruction *inst;
    SILDifferentiableAttr *attr;
    std::tie(inst, attr) = getIndirectDifferentiation();
    auto invokerLookup = invokers.find(attr); // No access to ADContext?
    assert(invokerLookup != invokers.end() && "Expected parent invoker");
    */
    break;
  }
  case Kind::SILDifferentiableAttribute: {
    auto diffAttr = getSILDifferentiableAttribute();
    os << "sil_differentiable_attribute=(attr=(";
    diffAttr->print(os);
    os << ") function=" << diffAttr->getOriginal()->getName();
    break;
  }
  }
  os << ')';
}

// Check whether the given requirements are satisfied, with the given
// substitution map and in the given module.
static bool checkRequirementsSatisfied(
    ArrayRef<Requirement> requirements, SubstitutionMap substMap,
    SILFunction *original, ModuleDecl *swiftModule) {
  if (requirements.empty())
    return true;
  // Iterate through all requirements and check whether they are satisfied.
  SmallVector<Requirement, 2> unsatisfiedRequirements;
  for (auto req : requirements) {
    auto firstType = req.getFirstType();
    auto secondType = req.getSecondType();
    // Substitute first and second types using the given substitution map,
    // looking up conformances in the current module, if possible.
    if (auto substFirstType =
            firstType.subst(QuerySubstitutionMap{substMap},
                            LookUpConformanceInModule(swiftModule))) {
      firstType = substFirstType;
    }
    if (auto substSecondType =
            secondType.subst(QuerySubstitutionMap{substMap},
                             LookUpConformanceInModule(swiftModule))) {
      secondType = substSecondType;
    }
    switch (req.getKind()) {
    // Check same type requirements.
    case RequirementKind::SameType:
      // If the first type does not equal the second type, then record the
      // unsatisfied requirement.
      if (!firstType->isEqual(secondType))
        unsatisfiedRequirements.push_back(req);
      continue;
    // Check conformance requirements.
    case RequirementKind::Conformance: {
      auto protocolType = req.getSecondType()->castTo<ProtocolType>();
      auto protocol = protocolType->getDecl();
      assert(protocol && "Expected protocol in generic signature requirement");
      // If the first type does not conform to the second type in the current
      // module, then record the unsatisfied requirement.
      if (!swiftModule->lookupConformance(firstType, protocol))
        unsatisfiedRequirements.push_back(req);
      continue;
    }
    // Ignore other requirements (superclass and layout).
    // Layout requirements are rejected during type-checking.
    default:
      continue;
    }
  }
  // Diagnose unsatisfied requirements.
  for (auto req : unsatisfiedRequirements) {
    LLVM_DEBUG(auto &s = getADDebugStream() << "Unsatisfied requirement:\n";
               req.print(s, PrintOptions());
               s << '\n');
  }
  return unsatisfiedRequirements.empty();
}

//===----------------------------------------------------------------------===//
// ADContext - Per-module contextual information for the Differentiation pass.
//===----------------------------------------------------------------------===//

class ADContext {
private:
  /// Reference to the main transform.
  SILModuleTransform &transform;

  /// The module where Differentiation is performed on.
  SILModule &module;

  /// AST context.
  ASTContext &astCtx = module.getASTContext();

  /// Shared pass manager.
  SILPassManager &passManager;

  /// The worklist (stack) of `autodiff_function` instructions to be processed.
  SmallVector<AutoDiffFunctionInst *, 32> autoDiffFunctionInsts;

  /// The set of `autodiff_function` instructions that have been processed.
  /// Used to avoid reprocessing invalidated instructions.
  SmallPtrSet<AutoDiffFunctionInst *, 32> processedAutoDiffFunctionInsts;

  /// Mapping from `[differentiable]` attributes to invokers.
  /// `SmallMapVector` is used for deterministic insertion order iteration.
  SmallMapVector<SILDifferentiableAttr *, DifferentiationInvoker, 32>
      invokers;

  /// Mapping from `autodiff_function` instructions to result indices.
  DenseMap<AutoDiffFunctionInst *, unsigned> resultIndices;

  /// Mapping from original `apply` instructions to their corresponding
  /// `NestedApplyInfo`s.
  DenseMap<ApplyInst *, NestedApplyInfo> nestedApplyInfo;

  /// List of generated functions (JVPs, VJPs, pullbacks, and thunks).
  /// Saved for deletion during cleanup.
  SmallVector<SILFunction *, 32> generatedFunctions;

  /// List of associated function references, generated via
  /// `emitAssociatedFunctionReference`.
  /// Saved for deletion during cleanup.
  SmallVector<SILValue, 32> generatedAssociatedFunctionReferences;

  /// The AdditiveArithmetic protocol in the standard library.
  ProtocolDecl *additiveArithmeticProtocol =
      astCtx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  /// The VectorProtocol protocol in the standard library.
  ProtocolDecl *vectorProtocolProtocol =
      astCtx.getProtocol(KnownProtocolKind::VectorProtocol);

  /// `AdditiveArithmetic.+` declaration.
  mutable FuncDecl *cachedPlusFn = nullptr;
  /// `AdditiveArithmetic.+=` declaration.
  mutable FuncDecl *cachedPlusEqualFn = nullptr;

public:
  /// Construct an ADContext for the given module.
  explicit ADContext(SILModuleTransform &transform);

  //--------------------------------------------------------------------------//
  // General utilities
  //--------------------------------------------------------------------------//

  SILModuleTransform &getTransform() const { return transform; }
  SILModule &getModule() const { return module; }
  ASTContext &getASTContext() const { return module.getASTContext(); }
  SILPassManager &getPassManager() const { return passManager; }
  Lowering::TypeConverter &getTypeConverter() { return module.Types; }

  SmallVectorImpl<AutoDiffFunctionInst *> &getAutoDiffFunctionInsts() {
    return autoDiffFunctionInsts;
  }

  SmallPtrSetImpl<AutoDiffFunctionInst *> &getProcessedAutoDiffFunctionInsts() {
    return processedAutoDiffFunctionInsts;
  }

  llvm::SmallMapVector<SILDifferentiableAttr *, DifferentiationInvoker, 32> &
  getInvokers() {
    return invokers;
  }

  DenseMap<AutoDiffFunctionInst *, unsigned> &getResultIndices() {
    return resultIndices;
  }

  DenseMap<ApplyInst *, NestedApplyInfo> &getNestedApplyInfo() {
    return nestedApplyInfo;
  }

  SmallVector<SILFunction *, 32> &getGeneratedFunctions() {
    return generatedFunctions;
  }

  SmallVector<SILValue, 32> &getGeneratedAssociatedFunctionReferences() {
    return generatedAssociatedFunctionReferences;
  }

  ProtocolDecl *getAdditiveArithmeticProtocol() const {
    return additiveArithmeticProtocol;
  }

  ProtocolDecl *getVectorProtocolProtocol() const {
    return vectorProtocolProtocol;
  }

  FuncDecl *getPlusDecl() const {
    if (!cachedPlusFn) {
      cachedPlusFn = findOperatorDeclInProtocol(
          astCtx.getIdentifier("+"), additiveArithmeticProtocol);
      assert(cachedPlusFn && "AdditiveArithmetic.+ not found");
    }
    return cachedPlusFn;
  }

  FuncDecl *getPlusEqualDecl() const {
    if (!cachedPlusEqualFn) {
      cachedPlusEqualFn = findOperatorDeclInProtocol(
          astCtx.getIdentifier("+="), additiveArithmeticProtocol);
      assert(cachedPlusEqualFn && "AdditiveArithmetic.+= not found");
    }
    return cachedPlusEqualFn;
  }

  void cleanUp() {
    for (auto invokerPair : invokers) {
      auto *attr = std::get<0>(invokerPair);
      auto *original = attr->getOriginal();
      LLVM_DEBUG(getADDebugStream()
                 << "Removing [differentiable] attribute for "
                 << original->getName() << '\n');
      original->removeDifferentiableAttr(attr);
    }
    // Delete all references to generated functions.
    for (auto assocFn : generatedAssociatedFunctionReferences) {
      if (auto *fnRef =
              peerThroughFunctionConversions<FunctionRefInst>(assocFn)) {
        LLVM_DEBUG(getADDebugStream()
                   << "Deleting generated associated function reference:\n"
                   << *fnRef);
        fnRef->replaceAllUsesWithUndef();
        fnRef->eraseFromParent();
      }
    }
    // Delete all generated functions.
    for (auto *generatedFunction : generatedFunctions) {
      LLVM_DEBUG(getADDebugStream()
                 << "Deleting generated function "
                 << generatedFunction->getName() << '\n');
      generatedFunction->dropAllReferences();
      transform.notifyWillDeleteFunction(generatedFunction);
      module.eraseFunction(generatedFunction);
    }
  }

  //--------------------------------------------------------------------------//
  // `[differentiable]` attribute lookup and registration
  //--------------------------------------------------------------------------//

  /// Finds the `[differentiable]` attribute on the specified original function
  /// corresponding to the specified parameter indices. Returns nullptr if it
  /// does not exist.
  SILDifferentiableAttr *lookUpDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices) const {
    for (auto *attr : original->getDifferentiableAttrs())
      if (attr->getIndices() == indices)
        return attr;
    return nullptr;
  }

  /// Finds the `[differentiable]` attribute on the specified original function
  /// corresponding to the specified parameter indices. Returns nullptr if it
  /// does not exist.
  SILDifferentiableAttr *lookUpMinimalDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices) const {
    auto *superset = AutoDiffIndexSubset::getDefault(
        getASTContext(),
        original->getLoweredFunctionType()->getNumParameters(), false);
    auto *indexSet = indices.parameters;
    if (auto *exactAttr = lookUpDifferentiableAttr(original, indices))
      return exactAttr;
    SILDifferentiableAttr *minimalAttr = nullptr;
    for (auto *rda : original->getDifferentiableAttrs()) {
      if (rda->getIndices().source != indices.source)
        continue;
      auto *rdaIndexSet = rda->getIndices().parameters;
      // If all indices in `indexSet` are in `rdaIndexSet`, and it has fewer
      // indices than our current candidate and a primitive VJP, then `rda` is
      // our new candidate.
      //
      // NOTE: `rda` may come from a un-partial-applied function and have larger
      // capacity than the desired indices. We expect this logic to go away when
      // we support `@differentiable` partial apply.
      if (rdaIndexSet->isSupersetOf(
              indexSet->extendingCapacity(getASTContext(),
                                          rdaIndexSet->getCapacity())) &&
          // fewer parameters than before
          (superset->isEmpty() ||
           rdaIndexSet->getNumIndices() < superset->getNumIndices())) {
        superset = rda->getIndices().parameters;
        minimalAttr = rda;
      }
    }
    return minimalAttr;
  }

  /// Creates a `[differentiable]` attribute on the specified original function
  /// with the specified parameter indices.
  SILDifferentiableAttr *createDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices,
      ArrayRef<Requirement> contextualRequirements) const {
    assert(!lookUpDifferentiableAttr(original, indices));
    auto *attr = SILDifferentiableAttr::create(getModule(), indices,
                                               contextualRequirements);
    original->addDifferentiableAttr(attr);
    return attr;
  }

  /// Finds or creates a `[differentiable]` attribute on the specified
  /// original function corresponding to the specified parameter indices.
  SILDifferentiableAttr *getOrCreateDifferentiableAttr(
      SILFunction *original, const SILAutoDiffIndices &indices,
      ArrayRef<Requirement> contextualRequirements) {
    if (auto *attr = lookUpDifferentiableAttr(original, indices))
      return attr;
    assert(original->isDefinition());
    return createDifferentiableAttr(original, indices, contextualRequirements);
  }

  /// Creates an `autodiff_function` instruction using the given builder and
  /// arguments. Erase the newly created instruction from the processed set, if
  /// it exists - it may exist in the processed set if it has the same pointer
  /// value as a previously processed and deleted instruction.
  AutoDiffFunctionInst *createAutoDiffFunction(
      SILBuilder &builder, SILLocation loc,
      AutoDiffIndexSubset *parameterIndices, unsigned differentiationOrder,
      SILValue original, ArrayRef<SILValue> associatedFunctions = {}) {
    auto *adfi = builder.createAutoDiffFunction(
        loc, parameterIndices, differentiationOrder, original,
        associatedFunctions);
    processedAutoDiffFunctionInsts.erase(adfi);
    return adfi;
  }

private:
  /// Promotes the given `autodiff_function` instruction to a valid
  /// `@differentiable` function-typed value.
  SILValue promoteToDifferentiableFunction(
      AutoDiffFunctionInst *inst, SILBuilder &builder, SILLocation loc,
      DifferentiationInvoker invoker);

public:
  /// Process the given `[differentiable]` attribute, filling in JVP/VJPs if
  /// missing.
  bool processDifferentiableAttribute(
      SILFunction *original, SILDifferentiableAttr *attr,
      DifferentiationInvoker invoker);

  /// Process the given `autodiff_function` instruction, filling in missing
  /// associated functions if necessary.
  bool processAutoDiffFunctionInst(AutoDiffFunctionInst *adfi);

  /// Fold `autodiff_function_extract` users of the given `autodiff_function`
  /// instruction, directly replacing them with `autodiff_function` instruction
  /// operands. If the `autodiff_function` instruction has no remaining uses,
  /// delete the instruction itself after folding.
  ///
  /// Folding can be disabled by the `SkipFoldingAutoDiffFunctionExtraction`
  /// flag for SIL testing purposes.
  void foldAutoDiffFunctionExtraction(AutoDiffFunctionInst *source);

  /// Get or create an associated function index subset thunk from
  /// `actualIndices` to `desiredIndices` for the given associated function
  /// value and original function operand.
  /// Calls `getOrCreateSubsetParametersThunkForLinearMap` to thunk the linear
  /// map returned by the associated function.
  std::pair<SILFunction *, SubstitutionMap>
  getOrCreateSubsetParametersThunkForAssociatedFunction(
      SILValue origFnOperand, SILValue assocFn,
      AutoDiffAssociatedFunctionKind kind, SILAutoDiffIndices desiredIndices,
      SILAutoDiffIndices actualIndices);

  /// Get or create an associated function index subset thunk from
  /// `actualIndices` to `desiredIndices` for the given associated function
  /// value and original function operand.
  SILFunction *getOrCreateSubsetParametersThunkForLinearMap(
      SILFunction *assocFn, CanSILFunctionType linearMapType,
      CanSILFunctionType targetType, AutoDiffAssociatedFunctionKind kind,
      SILAutoDiffIndices desiredIndices, SILAutoDiffIndices actualIndices);

public:
  /// Declare an external reference to an associated function of `original`,
  /// given a `[differentiable]` attribute of `original` and the associated
  /// function kind.
  SILFunction *
  declareExternalAssociatedFunction(SILFunction *original,
                                    SILDifferentiableAttr *attr, StringRef name,
                                    AutoDiffAssociatedFunctionKind kind);

  template <typename ...T, typename ...U>
  InFlightDiagnostic diagnose(SourceLoc loc, Diag<T...> diag,
                              U &&...args) const {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  /// Given an instruction and a differentiation task associated with the
  /// parent function, emits a "not differentiable" error based on the task. If
  /// the task is indirect, emits notes all the way up to the outermost task,
  /// and emits an error at the outer task. Otherwise, emits an error directly.
  template<typename ...T, typename ...U>
  InFlightDiagnostic emitNondifferentiabilityError(
      SILInstruction *inst, DifferentiationInvoker invoker,
      Diag<T...> diag, U &&...args);

  /// Given a value and a differentiation task associated with the parent
  /// function, emits a "not differentiable" error based on the task. If the
  /// task is indirect, emits notes all the way up to the outermost task, and
  /// emits an error at the outer task. Otherwise, emits an error directly.
  template<typename ...T, typename ...U>
  InFlightDiagnostic emitNondifferentiabilityError(
      SILValue value, DifferentiationInvoker invoker,
      Diag<T...> diag, U &&...args);

  /// Emit a "not differentiable" error based on the given differentiation task
  /// and diagnostic.
  template<typename ...T, typename ...U>
  InFlightDiagnostic emitNondifferentiabilityError(
      SourceLoc loc, DifferentiationInvoker invoker,
      Diag<T...> diag, U &&...args);
};
} // end anonymous namespace

ADContext::ADContext(SILModuleTransform &transform)
    : transform(transform), module(*transform.getModule()),
      passManager(*transform.getPassManager()) {}

template<typename ...T, typename ...U>
InFlightDiagnostic
ADContext::emitNondifferentiabilityError(SILValue value,
                                         DifferentiationInvoker invoker,
                                         Diag<T...> diag, U &&...args) {
  LLVM_DEBUG({
    getADDebugStream() << "Diagnosing non-differentiability.\n";
    getADDebugStream() << "For value:\n" << value;
    getADDebugStream() << "With invoker:\n" << invoker << '\n';
  });
  auto valueLoc = value.getLoc().getSourceLoc();
  return emitNondifferentiabilityError(valueLoc, invoker, diag,
                                       std::forward<U>(args)...);
}

template<typename ...T, typename ...U>
InFlightDiagnostic
ADContext::emitNondifferentiabilityError(SILInstruction *inst,
                                         DifferentiationInvoker invoker,
                                         Diag<T...> diag, U &&...args) {
  LLVM_DEBUG({
    getADDebugStream() << "Diagnosing non-differentiability.\n";
    getADDebugStream() << "For instruction:\n" << *inst;
    getADDebugStream() << "With invoker:\n" << invoker << '\n';
  });
  auto instLoc = inst->getLoc().getSourceLoc();
  return emitNondifferentiabilityError(instLoc, invoker, diag,
                                       std::forward<U>(args)...);
}

template<typename ...T, typename ...U>
InFlightDiagnostic
ADContext::emitNondifferentiabilityError(SourceLoc loc,
                                         DifferentiationInvoker invoker,
                                         Diag<T...> diag, U &&...args) {
  switch (invoker.getKind()) {
  // For `autodiff_function` instructions: if the `autodiff_function`
  // instruction comes from a differential operator, emit an error on the
  // expression and a note on the non-differentiable operation. Otherwise, emit
  // both an error and note on the non-differentiation operation.
  case DifferentiationInvoker::Kind::AutoDiffFunctionInst: {
    auto *inst = invoker.getAutoDiffFunctionInst();
    if (auto *expr = findDifferentialOperator(inst)) {
      diagnose(expr->getLoc(), diag::autodiff_function_not_differentiable_error)
          .highlight(expr->getSubExpr()->getSourceRange());
      return diagnose(loc, diag, std::forward<U>(args)...);
    }
    diagnose(loc, diag::autodiff_expression_not_differentiable_error);
    return diagnose(loc, diag, std::forward<U>(args)...);
  }

  // For `[differentiable]` attributes, try to find an AST function declaration
  // and `@differentiable` attribute. If they are found, emit an error on the
  // `@differentiable` attribute; otherwise, emit an error on the SIL function.
  // Emit a note at the non-differentiable operation.
  case DifferentiationInvoker::Kind::SILDifferentiableAttribute: {
    auto *attr = invoker.getSILDifferentiableAttribute();
    auto *original = attr->getOriginal();
    bool foundAttr = false;
    if (auto *declContext = original->getDeclContext()) {
      if (auto *fnDecl = declContext->getInnermostDeclarationDeclContext()) {
        if (auto *diffAttr =
                fnDecl->getAttrs().getAttribute<DifferentiableAttr>()) {
          diagnose(diffAttr->getLocation(),
                   diag::autodiff_function_not_differentiable_error)
              .highlight(diffAttr->getRangeWithAt());
          diagnose(original->getLocation().getSourceLoc(),
                   diag::autodiff_when_differentiating_function_definition);
          foundAttr = true;
        }
      }
    }
    // Fallback if we cannot find the expected attribute.
    if (!foundAttr)
      diagnose(original->getLocation().getSourceLoc(),
               diag::autodiff_function_not_differentiable_error);
    return diagnose(loc, diag, std::forward<U>(args)...);
  }

  // For indirect differentiation, emit a "not differentiable" note on the
  // expression first. Then emit an error at the source invoker of
  // differentiation, and a "when differentiating this" note at each indirect
  // invoker.
  case DifferentiationInvoker::Kind::IndirectDifferentiation: {
    SILInstruction *inst;
    SILDifferentiableAttr *attr;
    std::tie(inst, attr) = invoker.getIndirectDifferentiation();
    auto invokerLookup = invokers.find(attr);
    assert(invokerLookup != invokers.end() && "Expected parent invoker");
    emitNondifferentiabilityError(inst, invokerLookup->second,
        diag::autodiff_expression_not_differentiable_note);
    return diagnose(loc, diag::autodiff_when_differentiating_function_call);
  }
  }
}

PullbackInfo::PullbackInfo(ADContext &context, SILFunction *original,
                           SILFunction *vjp, const SILAutoDiffIndices &indices)
    : original(original), typeConverter(context.getTypeConverter()) {
  auto &astCtx = original->getASTContext();
  auto *loopAnalysis = context.getPassManager().getAnalysis<SILLoopAnalysis>();
  auto *loopInfo = loopAnalysis->get(original);
  // Get VJP generic signature.
  CanGenericSignature vjpGenSig = nullptr;
  if (auto *vjpGenEnv = vjp->getGenericEnvironment())
    vjpGenSig = vjpGenEnv->getGenericSignature()->getCanonicalSignature();
  // Create predecessor enum and pullback struct for each original block.
  for (auto &origBB : *original) {
    auto *pbStruct = createPullbackStruct(&origBB, indices, vjpGenSig);
    pullbackStructs.insert({&origBB, pbStruct});
  }
  for (auto &origBB : *original) {
    auto *pbStruct = getPullbackStruct(&origBB);
    auto *predEnum =
        createBasicBlockPredecessorEnum(&origBB, indices, vjpGenSig);
    // If original block is in a loop, mark predecessor enum as indirect.
    if (loopInfo->getLoopFor(&origBB))
      predEnum->getAttrs().add(new (astCtx) IndirectAttr(/*Implicit*/ true));
    predecessorEnums.insert({&origBB, predEnum});
    if (origBB.isEntry())
      continue;
    auto *predEnumField =
        addVarDecl(pbStruct, astCtx.getIdentifier("predecessor").str(),
                   predEnum->getDeclaredInterfaceType());
    pullbackStructPredecessorFields.insert({pbStruct, predEnumField});
  }
}

//===----------------------------------------------------------------------===//
// Activity Analysis
//===----------------------------------------------------------------------===//

namespace {
class DifferentiableActivityCollection;

/// In many real situations, the end-users of AD need only the derivatives of
/// some selected outputs of `P` with respect to some selected inputs of `P`.
/// Whatever the differentiation mode (tangent, reverse,...), these restrictions
/// allow the AD tool to produce a much more efficient differentiated program.
/// Essentially, fixing some inputs and neglecting some outputs allows AD to
/// just forget about several intermediate differentiated variables.
///
/// Activity analysis is the specific analysis that detects these situations,
/// therefore allowing for a better differentiated code. Activity analysis is
/// present in all transformation-based AD tools.
///
/// To begin with, the end-user specifies that only some output variables (the
/// “dependent”) must be differentiated with respect to only some input
/// variables (the “independent”). We say that variable `y` depends on `x` when
/// the derivative of `y` with respect to `x` is not trivially null. We say that
/// a variable is “varied” if it depends on at least one independent. Conversely
/// we say that a variable is “useful” if at least one dependent depends on it.
/// Finally, we say that a variable is “active” if it is at the same time varied
/// and useful. In the special case of the tangent mode, it is easy to check
/// that when variable `v` is not varied at some place in the program, then its
/// derivative `v̇` at this place is certainly null. Conversely when variable `v`
/// is not useful, then whatever the value of `v̇`, this value does not matter
/// for the final result. Symmetric reasoning applies for the reverse mode of
/// AD: observing that differentiated variables go upstream, we see that a
/// useless variable has a null derivative, in other words the partial
/// derivative of the output with respect to this variable is null. Conversely
/// when variable `v` is not varied, then whatever the value of `v`, this value
/// does not matter for the final result.
///
/// Reference:
/// Laurent Hascoët. Automatic Differentiation by Program Transformation. 2007.
class DifferentiableActivityAnalysis
    : public FunctionAnalysisBase<DifferentiableActivityCollection> {
private:
  DominanceAnalysis *dominanceAnalysis = nullptr;
  PostDominanceAnalysis *postDominanceAnalysis = nullptr;

public:
  explicit DifferentiableActivityAnalysis()
      : FunctionAnalysisBase(SILAnalysisKind::DifferentiableActivity) {}

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == SILAnalysisKind::DifferentiableActivity;
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind k) override {
    return k & InvalidationKind::Everything;
  }

  virtual std::unique_ptr<DifferentiableActivityCollection>
  newFunctionAnalysis(SILFunction *f) override;

  virtual void initialize(SILPassManager *pm) override;
};
} // end anonymous namespace

namespace {
/// Represents the differentiation activity associated with a SIL value.
enum class ActivityFlags : unsigned {
  /// The value depends on a function parameter.
  Varied = 1 << 1,
  /// The value contributes to a result.
  Useful = 1 << 2,
  /// The value is both varied and useful.
  Active = Varied | Useful,
};

using Activity = OptionSet<ActivityFlags>;

/// Result of activity analysis on a function. Accepts queries for whether a
/// value is "varied", "useful" or "active" against certain differentiation
/// indices.
class DifferentiableActivityInfo {
private:
  DifferentiableActivityCollection &parent;
  GenericSignature *assocGenSig = nullptr;

  /// Input values, i.e. parameters (both direct and indirect).
  SmallVector<SILValue, 4> inputValues;
  /// Output values, i.e. individual values (not the final tuple) being returned
  /// by the `return` instruction.
  SmallVector<SILValue, 4> outputValues;

  /// The set of useful variables, indexed by the corresponding dependent value
  /// (output) index.
  SmallVector<SmallDenseSet<SILValue>, 4> usefulValueSets;
  /// The set of useful variables, indexed by the corresponding independent
  /// value (input) index.
  SmallVector<SmallDenseSet<SILValue>, 4> variedValueSets;

  /// The original function.
  SILFunction &getFunction();

  /// Perform analysis and populate sets.
  void analyze(DominanceInfo *di, PostDominanceInfo *pdi);

  void setVaried(SILValue value, unsigned independentVariableIndex);
  void setVariedAcrossArrayInitialization(SILValue value,
                                          unsigned independentVariableIndex);
  void setUseful(SILValue value, unsigned dependentVariableIndex);
  void setUsefulAcrossArrayInitialization(SILValue value,
                                          unsigned dependentVariableIndex);
  void recursivelySetVaried(SILValue value, unsigned independentVariableIndex);
  void propagateUsefulThroughBuffer(SILValue value,
                                    unsigned dependentVariableIndex);

public:
  explicit DifferentiableActivityInfo(
      DifferentiableActivityCollection &parent, GenericSignature *assocGenSig);

  bool isVaried(SILValue value, unsigned independentVariableIndex) const;
  bool isUseful(SILValue value, unsigned dependentVariableIndex) const;
  bool isVaried(SILValue value, AutoDiffIndexSubset *parameterIndices) const;
  bool isActive(SILValue value, const SILAutoDiffIndices &indices) const;

  Activity getActivity(SILValue value,
                       const SILAutoDiffIndices &indices) const;
  Activity getActivity(SILInstruction *inst,
                       const SILAutoDiffIndices &indices) const;
};

class DifferentiableActivityCollection {
public:
  SmallDenseMap<GenericSignature *, DifferentiableActivityInfo> activityInfoMap;
  SILFunction &function;
  DominanceInfo *domInfo;
  PostDominanceInfo *postDomInfo;

  DifferentiableActivityInfo &getActivityInfo(GenericSignature *assocGenSig) {
    auto activityInfoLookup = activityInfoMap.find(assocGenSig);
    if (activityInfoLookup != activityInfoMap.end())
      return activityInfoLookup->getSecond();
    auto insertion = activityInfoMap.insert(
        {assocGenSig, DifferentiableActivityInfo(*this, assocGenSig)});
    return insertion.first->getSecond();
  }

  explicit DifferentiableActivityCollection(SILFunction &f,
                                            DominanceInfo *di,
                                            PostDominanceInfo *pdi);
};

} // end anonymous namespace

std::unique_ptr<DifferentiableActivityCollection>
DifferentiableActivityAnalysis::newFunctionAnalysis(SILFunction *f) {
  assert(dominanceAnalysis && "Expect a valid dominance anaysis");
  assert(postDominanceAnalysis && "Expect a valid post-dominance anaysis");
  return llvm::make_unique<DifferentiableActivityCollection>(
      *f, dominanceAnalysis->get(f), postDominanceAnalysis->get(f));
}

void DifferentiableActivityAnalysis::initialize(SILPassManager *pm) {
  dominanceAnalysis = pm->getAnalysis<DominanceAnalysis>();
  postDominanceAnalysis = pm->getAnalysis<PostDominanceAnalysis>();
}

SILAnalysis *swift::createDifferentiableActivityAnalysis(SILModule *m) {
  return new DifferentiableActivityAnalysis();
}

DifferentiableActivityCollection::DifferentiableActivityCollection(
    SILFunction &f, DominanceInfo *di, PostDominanceInfo *pdi)
    : function(f), domInfo(di), postDomInfo(pdi) {}

DifferentiableActivityInfo::DifferentiableActivityInfo(
    DifferentiableActivityCollection &parent, GenericSignature *assocGenSig)
    : parent(parent), assocGenSig(assocGenSig) {
  analyze(parent.domInfo, parent.postDomInfo);
}

void DifferentiableActivityInfo::analyze(DominanceInfo *di,
                                         PostDominanceInfo *pdi) {
  auto &function = parent.function;
  LLVM_DEBUG(getADDebugStream()
             << "Running activity analysis on @" << function.getName() << '\n');
  // Inputs are just function's arguments, count `n`.
  auto paramArgs = function.getArgumentsWithoutIndirectResults();
  for (auto value : paramArgs)
    inputValues.push_back(value);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Inputs in @" << function.getName() << ":\n";
    for (auto val : inputValues)
      s << val << '\n';
  });
  // Outputs are indirect result buffers and return values, count `m`.
  collectAllFormalResultsInTypeOrder(function, outputValues);
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Outputs in @" << function.getName() << ":\n";
    for (auto val : outputValues)
      s << val << '\n';
  });

  // Mark inputs as varied.
  assert(variedValueSets.empty());
  for (auto input : inputValues)
    variedValueSets.push_back({input});
  // Propagate varied-ness through the function in dominance order.
  DominanceOrder domOrder(function.getEntryBlock(), di);
  while (auto *bb = domOrder.getNext()) {
    for (auto &inst : *bb) {
      for (auto i : indices(inputValues)) {
        // Handle `apply`.
        if (auto *ai = dyn_cast<ApplyInst>(&inst)) {
          if (isWithoutDerivative(ai->getCallee()))
            continue;
          for (auto arg : ai->getArgumentsWithoutIndirectResults()) {
            if (isVaried(arg, i)) {
              for (auto indRes : ai->getIndirectSILResults())
                setVaried(indRes, i);
              for (auto dirRes : ai->getResults())
                setVaried(dirRes, i);
            }
          }
        }
        // Handle `store`.
        else if (auto *si = dyn_cast<StoreInst>(&inst)) {
          if (isVaried(si->getSrc(), i))
            recursivelySetVaried(si->getDest(), i);
        }
        // Handle `copy_addr`.
        else if (auto *cai = dyn_cast<CopyAddrInst>(&inst)) {
          if (isVaried(cai->getSrc(), i))
            recursivelySetVaried(cai->getDest(), i);
        }
        // Handle `tuple_element_addr`.
        else if (auto *teai = dyn_cast<TupleElementAddrInst>(&inst)) {
          if (isVaried(teai->getOperand(), i)) {
            auto projType = teai->getType().getASTType();
            if (assocGenSig && projType->hasArchetype())
              projType = assocGenSig->getCanonicalTypeInContext(
                  projType->mapTypeOutOfContext());
            if (projType->getAutoDiffAssociatedTangentSpace(
                LookUpConformanceInSignature(*assocGenSig)))
              setVaried(teai, i);
          }
        }

// Handle `struct_extract` and `struct_element_addr` instructions.
// - If the field is marked `@noDerivative`, do not set the result as varied
//   because it is not in the set of differentiable variables.
// - Otherwise, propagate variedness from operand to result as usual.
#define PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION(INST) \
  else if (auto *sei = dyn_cast<INST##Inst>(&inst)) { \
    if (isVaried(sei->getOperand(), i)) { \
      auto hasNoDeriv = sei->getField()->getAttrs() \
          .hasAttribute<NoDerivativeAttr>(); \
      if (!hasNoDeriv) \
        setVaried(sei, i); \
    } \
  }
  PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION(StructExtract)
  PROPAGATE_VARIED_FOR_STRUCT_EXTRACTION(StructElementAddr)
#undef VISIT_STRUCT_ELEMENT_INNS

        // Handle `br`.
        else if (auto *bi = dyn_cast<BranchInst>(&inst)) {
          for (auto &op : bi->getAllOperands())
            if (isVaried(op.get(), i))
              setVaried(bi->getArgForOperand(&op), i);
        }
        // Handle `cond_br`.
        else if (auto *cbi = dyn_cast<CondBranchInst>(&inst)) {
          for (unsigned opIdx : indices(cbi->getTrueOperands())) {
            auto &op = cbi->getTrueOperands()[opIdx];
            if (isVaried(op.get(), i))
              setVaried(cbi->getTrueBB()->getArgument(opIdx), i);
          }
          for (unsigned opIdx : indices(cbi->getFalseOperands())) {
            auto &op = cbi->getFalseOperands()[opIdx];
            if (isVaried(op.get(), i))
              setVaried(cbi->getFalseBB()->getArgument(opIdx), i);
          }
        }
        // Handle `switch_enum`.
        else if (auto *sei = dyn_cast<SwitchEnumInst>(&inst)) {
          if (isVaried(sei->getOperand(), i)) {
            for (auto *succBB : sei->getSuccessorBlocks())
              for (auto *arg : succBB->getArguments())
                setVaried(arg, i);
            // Default block cannot have arguments.
          }
        }
        // Handle everything else.
        else {
          for (auto &op : inst.getAllOperands())
            if (isVaried(op.get(), i))
              for (auto result : inst.getResults())
                setVaried(result, i);
        }
      }
    }
    domOrder.pushChildren(bb);
  }

  // Mark differentiable outputs as useful.
  assert(usefulValueSets.empty());
  for (auto output : outputValues) {
    usefulValueSets.push_back({});
    // If the output has an address type, propagate usefulness recursively.
    if (output->getType().isAddress())
      propagateUsefulThroughBuffer(output, usefulValueSets.size() - 1);
    // Otherwise, just mark the output as useful.
    else
      setUseful(output, usefulValueSets.size() - 1);
  }
  // Propagate usefulness through the function in post-dominance order.
  PostDominanceOrder postDomOrder(&*function.findReturnBB(), pdi);
  while (auto *bb = postDomOrder.getNext()) {
    for (auto &inst : reversed(*bb)) {
      for (auto i : indices(outputValues)) {
        // Handle indirect results in `apply`.
        if (auto *ai = dyn_cast<ApplyInst>(&inst)) {
          if (isWithoutDerivative(ai->getCallee()))
            continue;
          auto checkAndSetUseful = [&](SILValue res) {
            if (isUseful(res, i))
              for (auto arg : ai->getArgumentsWithoutIndirectResults())
                setUseful(arg, i);
          };
          for (auto dirRes : ai->getResults())
            checkAndSetUseful(dirRes);
          for (auto indRes : ai->getIndirectSILResults())
            checkAndSetUseful(indRes);
          auto paramInfos = ai->getSubstCalleeConv().getParameters();
          for (auto i : indices(paramInfos))
            if (paramInfos[i].isIndirectInOut())
              checkAndSetUseful(ai->getArgumentsWithoutIndirectResults()[i]);
        }
        // Handle `store`.
        else if (auto *si = dyn_cast<StoreInst>(&inst)) {
          if (isUseful(si->getDest(), i))
            setUseful(si->getSrc(), i);
        }
        // Handle `copy_addr`.
        else if (auto *cai = dyn_cast<CopyAddrInst>(&inst)) {
          if (isUseful(cai->getDest(), i))
            propagateUsefulThroughBuffer(cai->getSrc(), i);
        }
        // Handle reads.
        else if (inst.mayReadFromMemory()) {
          if (llvm::any_of(inst.getResults(),
                           [&](SILValue res) { return isUseful(res, i); }))
            for (auto &op : inst.getAllOperands())
              if (op.get()->getType().isAddress())
                propagateUsefulThroughBuffer(op.get(), i);
        }
        // Handle everything else.
        else {
          for (auto result : inst.getResults())
            if (isUseful(result, i))
              for (auto &op : inst.getAllOperands())
                setUseful(op.get(), i);
        }
      }
    }
    // Propagate usefulness from basic block arguments to incoming phi values.
    for (auto i : indices(outputValues)) {
      for (auto *arg : bb->getArguments()) {
        if (isUseful(arg, i)) {
          SmallVector<SILValue, 4> incomingValues;
          arg->getSingleTerminatorOperands(incomingValues);
          for (auto incomingValue : incomingValues)
            setUseful(incomingValue, i);
        }
      }
    }
    postDomOrder.pushChildren(bb);
  }
}

void DifferentiableActivityInfo::setVariedAcrossArrayInitialization(
    SILValue value, unsigned independentVariableIndex) {
  auto uai = getAllocateUninitializedArrayIntrinsic(value);
  if (!uai) return;
  for (auto use : value->getUses())
    if (auto tei = dyn_cast<TupleExtractInst>(use->getUser()->getResult(0)))
      // The first tuple field of the intrinsic's return value is the array.
      if (tei->getFieldNo() == 0)
        setVaried(tei->getResult(0), independentVariableIndex);
}

void DifferentiableActivityInfo::setUsefulAcrossArrayInitialization(
    SILValue value, unsigned dependentVariableIndex) {
  // Array initializer syntax is lowered to an intrinsic and one or more
  // stores to a `RawPointer` returned by the intrinsic.
  auto uai = getAllocateUninitializedArrayIntrinsic(value);
  if (!uai) return;
  for (auto use : value->getUses()) {
    auto tei = dyn_cast<TupleExtractInst>(use->getUser()->getResult(0));
    if (!tei || tei->getFieldNo() != 1) continue;
    // The second tuple field of the return value is the `RawPointer`.
    for (auto use : tei->getUses()) {
      // The `RawPointer` passes through a `pointer_to_address`. That
      // instruction's first use is a `store` whose src is useful; its
      // subsequent uses are `index_addr`s whose only use is a useful `store`.
      for (auto use : use->getUser()->getResult(0)->getUses()) {
        auto inst = use->getUser();
        if (auto si = dyn_cast<StoreInst>(inst)) {
          setUseful(si->getSrc(), dependentVariableIndex);
        } else if (auto iai = dyn_cast<IndexAddrInst>(inst)) {
          for (auto use : iai->getUses())
            if (auto si = dyn_cast<StoreInst>(use->getUser()))
              setUseful(si->getSrc(), dependentVariableIndex);
        }
      }
    }
  }
}

void DifferentiableActivityInfo::setVaried(SILValue value,
                                           unsigned independentVariableIndex) {
  variedValueSets[independentVariableIndex].insert(value);
  setVariedAcrossArrayInitialization(value, independentVariableIndex);
}

void DifferentiableActivityInfo::setUseful(SILValue value,
                                           unsigned dependentVariableIndex) {
  usefulValueSets[dependentVariableIndex].insert(value);
  setUsefulAcrossArrayInitialization(value, dependentVariableIndex);
}

void DifferentiableActivityInfo::recursivelySetVaried(
    SILValue value, unsigned independentVariableIndex) {
  setVaried(value, independentVariableIndex);
  if (auto *inst = value->getDefiningInstruction()) {
    if (auto *ai = dyn_cast<ApplyInst>(inst))
      return;
    for (auto &op : inst->getAllOperands())
      recursivelySetVaried(op.get(), independentVariableIndex);
  }
}

void DifferentiableActivityInfo::propagateUsefulThroughBuffer(
    SILValue value, unsigned dependentVariableIndex) {
  assert(value->getType().isAddress());
  // Check whether value is already useful to prevent infinite recursion.
  if (isUseful(value, dependentVariableIndex))
    return;
  setUseful(value, dependentVariableIndex);
  if (auto *inst = value->getDefiningInstruction())
    for (auto &operand : inst->getAllOperands())
      if (operand.get()->getType().isAddress())
        propagateUsefulThroughBuffer(operand.get(), dependentVariableIndex);
  // Recursively propagate usefulness through users that are projections or
  // `begin_access` instructions.
  for (auto use : value->getUses())
    for (auto res : use->getUser()->getResults())
      if (Projection::isAddressProjection(res) || isa<BeginAccessInst>(res))
        propagateUsefulThroughBuffer(res, dependentVariableIndex);
}

bool DifferentiableActivityInfo::isVaried(
    SILValue value, unsigned independentVariableIndex) const {
  auto &set = variedValueSets[independentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::isVaried(
    SILValue value, AutoDiffIndexSubset *parameterIndices) const {
  for (auto paramIdx : parameterIndices->getIndices())
    if (isVaried(value, paramIdx))
      return true;
  return false;
}

bool DifferentiableActivityInfo::isUseful(
    SILValue value, unsigned dependentVariableIndex) const {
  auto &set = usefulValueSets[dependentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::isActive(
    SILValue value, const SILAutoDiffIndices &indices) const {
  return isVaried(value, indices.parameters) && isUseful(value, indices.source);
}

Activity DifferentiableActivityInfo::getActivity(
    SILValue value, const SILAutoDiffIndices &indices) const {
  Activity activity;
  if (isVaried(value, indices.parameters))
    activity |= ActivityFlags::Varied;
  if (isUseful(value, indices.source))
    activity |= ActivityFlags::Useful;
  return activity;
}

Activity DifferentiableActivityInfo::getActivity(
    SILInstruction *inst, const SILAutoDiffIndices &indices) const {
  Activity activity;
  for (auto result : inst->getResults())
    activity |= getActivity(result, indices);
  return activity;
}

static void dumpActivityInfo(SILValue value,
                             const SILAutoDiffIndices &indices,
                             const DifferentiableActivityInfo &activityInfo,
                             llvm::raw_ostream &s = llvm::dbgs()) {
  s << '[';
  auto activity = activityInfo.getActivity(value, indices);
  switch (activity.toRaw()) {
  case 0: s << "NONE"; break;
  case (unsigned)ActivityFlags::Varied: s << "VARIED"; break;
  case (unsigned)ActivityFlags::Useful: s << "USEFUL"; break;
  case (unsigned)ActivityFlags::Active: s << "ACTIVE"; break;
  }
  s << "] " << value;
}

static void dumpActivityInfo(SILFunction &fn,
                             const SILAutoDiffIndices &indices,
                             DifferentiableActivityInfo &activityInfo,
                             llvm::raw_ostream &s = llvm::dbgs()) {
  s << "Activity info for " << fn.getName() << " at " << indices << '\n';
  for (auto &bb : fn) {
    s << "bb" << bb.getDebugID() << ":\n";
    for (auto *arg : bb.getArguments())
      dumpActivityInfo(arg, indices, activityInfo, s);
    for (auto &inst : bb)
      for (auto res : inst.getResults())
        dumpActivityInfo(res, indices, activityInfo, s);
    s << '\n';
  }
}

/// If the original function doesn't have a return, it cannot be differentiated.
/// Returns true if error is emitted.
static bool diagnoseNoReturn(ADContext &context, SILFunction *original,
                             DifferentiationInvoker invoker) {
  if (original->findReturnBB() != original->end())
    return false;
  context.emitNondifferentiabilityError(
      original->getLocation().getEndSourceLoc(), invoker,
      diag::autodiff_missing_return);
  return true;
}

/// If the original function contains unsupported control flow, emit a "control
/// flow unsupported" error at appropriate source locations. Returns true if
/// error is emitted.
///
/// Update as control flow support is added. Currently, branching terminators
/// other than `br`, `cond_br`, `switch_enum` are not supported.
static bool diagnoseUnsupportedControlFlow(ADContext &context,
                                           SILFunction *original,
                                           DifferentiationInvoker invoker) {
  if (original->getBlocks().size() <= 1)
    return false;
  // Diagnose unsupported branching terminators.
  for (auto &bb : *original) {
    auto *term = bb.getTerminator();
    // Supported terminators are: `br`, `cond_br`, `switch_enum`.
    if (isa<BranchInst>(term) || isa<CondBranchInst>(term) ||
        isa<SwitchEnumInst>(term))
      continue;
    // If terminator is an unsupported branching terminator, emit an error.
    if (term->isBranch()) {
      context.emitNondifferentiabilityError(
          term, invoker, diag::autodiff_control_flow_not_supported);
      return true;
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
// Code emission utilities
//===----------------------------------------------------------------------===//

/// Given a value, collect all `tuple_extract` users in `result` if value is a
/// tuple. Otherwise, add the value directly to `result`.
static void collectAllExtractedElements(SILValue val,
                                        SmallVectorImpl<SILValue> &result) {
  if (auto tupleType = val->getType().getAs<TupleType>()) {
    result.resize(tupleType->getNumElements(), SILValue());
    for (auto *use : val->getUses())
      if (auto *tupleExtract = dyn_cast<TupleExtractInst>(use->getUser()))
        result[tupleExtract->getFieldNo()] = tupleExtract;
  }
  else
    result.push_back(val);
}

/// Given a value, extracts all elements to `result` from this value if it's a
/// tuple. Otherwise, add this value directly to `result`.
static void extractAllElements(SILValue val, SILBuilder &builder,
                               SmallVectorImpl<SILValue> &result) {
  if (auto tupleType = val->getType().getAs<TupleType>())
    for (auto i : range(tupleType->getNumElements()))
      result.push_back(builder.createTupleExtract(val.getLoc(), val, i));
  else
    result.push_back(val);
}

/// Given a range of elements, joins these into a single value. If there's
/// exactly one element, returns that element. Otherwise, creates a tuple using
/// a `tuple` instruction.
static SILValue joinElements(ArrayRef<SILValue> elements, SILBuilder &builder,
                             SILLocation loc) {
  if (elements.size() == 1)
    return elements.front();
  return builder.createTuple(loc, elements);
}

// Emits a release based on the value's type category (address or object).
static void emitCleanup(SILBuilder &builder, SILLocation loc, SILValue v) {
  if (v->getType().isAddress())
    builder.createDestroyAddr(loc, v);
  else
    builder.createReleaseValue(loc, v, builder.getDefaultAtomicity());
}

/// When a function value is used in an instruction (usually `apply`), there's
/// some conversion instruction in between, e.g. `thin_to_thick_function`. Given
/// a new function value and an old function value, this helper function
/// recursively converts the new function just like how the old function is
/// converted. If the new function's generic signature is specified, it is used
/// to create substitution maps for reapplied `partial_apply` instructions.
static SILValue
reapplyFunctionConversion(SILValue newFunc, SILValue oldFunc,
                          SILValue oldConvertedFunc, SILBuilder &builder,
                          SILLocation loc,
                          GenericSignature *newFuncGenSig = nullptr) {
  // If the old func is the new func, then there's no conversion.
  if (oldFunc == oldConvertedFunc)
    return newFunc;
  // Handle a few instruction cases.
  // thin_to_thick_function
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(oldConvertedFunc)) {
    auto innerNewFunc = reapplyFunctionConversion(
        newFunc, oldFunc, tttfi->getOperand(), builder, loc, newFuncGenSig);
    auto operandFnTy = innerNewFunc->getType().castTo<SILFunctionType>();
    auto thickTy = operandFnTy->getWithRepresentation(
        SILFunctionTypeRepresentation::Thick);
    auto silTy = SILType::getPrimitiveObjectType(thickTy);

    return builder.createThinToThickFunction(loc, innerNewFunc, silTy);
  }
  // partial_apply
  if (auto *pai = dyn_cast<PartialApplyInst>(oldConvertedFunc)) {
    SmallVector<SILValue, 8> newArgs;
    newArgs.reserve(pai->getNumArguments());
    for (auto arg : pai->getArguments()) {
      // Retain the argument since it's to be owned by the newly created
      // closure.
      if (arg->getType().isObject())
        builder.createRetainValue(loc, arg, builder.getDefaultAtomicity());
      else if (arg->getType().isLoadable(builder.getFunction()))
        builder.createRetainValueAddr(loc, arg, builder.getDefaultAtomicity());
      newArgs.push_back(arg);
    }
    auto innerNewFunc = reapplyFunctionConversion(
        newFunc, oldFunc, pai->getCallee(), builder, loc, newFuncGenSig);
    // If new function's generic signature is specified, use it to create
    // substitution map for reapplied `partial_apply` instruction.
    auto substMap = !newFuncGenSig
        ? pai->getSubstitutionMap()
        : SubstitutionMap::get(
              newFuncGenSig, QuerySubstitutionMap{pai->getSubstitutionMap()},
              LookUpConformanceInModule(builder.getModule().getSwiftModule()));
    return builder.createPartialApply(loc, innerNewFunc, substMap, newArgs,
                                      ParameterConvention::Direct_Guaranteed);
  }
  // convert_escape_to_noescape
  if (auto *cetn = dyn_cast<ConvertEscapeToNoEscapeInst>(oldConvertedFunc)) {
    auto innerNewFunc = reapplyFunctionConversion(newFunc, oldFunc,
                                                  cetn->getOperand(), builder,
                                                  loc, newFuncGenSig);
    auto operandFnTy = innerNewFunc->getType().castTo<SILFunctionType>();
    auto noEscapeType = operandFnTy->getWithExtInfo(
        operandFnTy->getExtInfo().withNoEscape());
    auto silTy = SILType::getPrimitiveObjectType(noEscapeType);
    return builder.createConvertEscapeToNoEscape(
        loc, innerNewFunc, silTy,
        cetn->isLifetimeGuaranteed());
  }
  // convert_function
  if (auto *cfi = dyn_cast<ConvertFunctionInst>(oldConvertedFunc)) {
    // `convert_function` does not have a fixed typing rule because it can
    // convert between function types as long as they are ABI-compatible. Here
    // we match specific patterns.
    auto origTargetFnTy = cfi->getType().castTo<SILFunctionType>();
    auto origSourceFnTy =
        cfi->getOperand()->getType().castTo<SILFunctionType>();
    auto innerNewFunc = reapplyFunctionConversion(newFunc, oldFunc,
                                                  cfi->getOperand(), builder,
                                                  loc, newFuncGenSig);
    // Match a conversion from escaping to `@noescape`
    CanSILFunctionType targetType;
    if (!origSourceFnTy->isNoEscape() && origTargetFnTy->isNoEscape() &&
        origSourceFnTy == origTargetFnTy->getWithExtInfo(
            origTargetFnTy->getExtInfo().withNoEscape(false))) {
      auto operandFnTy = innerNewFunc->getType().castTo<SILFunctionType>();
      targetType = operandFnTy->getWithExtInfo(
          operandFnTy->getExtInfo().withNoEscape(true));
    }
    assert(targetType && "Unhandled convert_function pattern");
    auto silTy = SILType::getPrimitiveObjectType(targetType);
    return builder.createConvertFunction(loc, innerNewFunc, silTy,
                                         cfi->withoutActuallyEscaping());
  }
  llvm_unreachable("Unhandled function convertion instruction");
}

static SubstitutionMap getSubstitutionMap(
    SILValue value, SubstitutionMap substMap = SubstitutionMap()) {
  if (auto *thinToThick = dyn_cast<ThinToThickFunctionInst>(value))
    return getSubstitutionMap(thinToThick->getOperand(), substMap);
  if (auto *convertFn = dyn_cast<ConvertFunctionInst>(value))
    return getSubstitutionMap(convertFn->getOperand(), substMap);
  if (auto *convertFn = dyn_cast<ConvertEscapeToNoEscapeInst>(value))
    return getSubstitutionMap(convertFn->getOperand(), substMap);
  if (auto *partialApply = dyn_cast<PartialApplyInst>(value)) {
    auto appliedSubstMap = partialApply->getSubstitutionMap();
    // TODO: Combine argument `substMap` with `appliedSubstMap`.
    return getSubstitutionMap(partialApply->getCallee(), appliedSubstMap);
  }
  if (auto *apply = dyn_cast<ApplyInst>(value)) {
    auto appliedSubstMap = apply->getSubstitutionMap();
    // TODO: Combine argument `substMap` with `appliedSubstMap`.
    return getSubstitutionMap(apply->getCallee(), appliedSubstMap);
  }
  return substMap;
}

/// Emits a reference to an associated function of `original`, differentiated
/// with respect to a superset of `desiredIndices`. Returns the `SILValue` for
/// the associated function and the actual indices that the associated function
/// is with respect to.
///
/// Returns `None` on failure, signifying that a diagnostic has been emitted.
///
/// Creates new differentiation tasks, if necessary, using `invoker` as the
/// invoker. Calls `taskCallback` for all newly-created tasks (but may also call
/// `taskCallback` for already-existing tasks), so that the caller can make sure
/// that the task actually gets executed.
///
/// FIXME: This is too complicated and needs to be rewritten.
static Optional<std::pair<SILValue, SILAutoDiffIndices>>
emitAssociatedFunctionReference(
    ADContext &context, SILBuilder &builder, SILAutoDiffIndices desiredIndices,
    AutoDiffAssociatedFunctionKind kind, SILValue original,
    DifferentiationInvoker invoker) {

  SILValue functionSource = original;

  // If `original` is itself an `AutoDiffFunctionExtractInst` whose kind matches
  // the given kind and desired differentiation parameter indices, simply
  // extract the associated function of its function operand, retain the
  // associated function, and return it.
  if (auto *inst = original->getDefiningInstruction())
    if (auto *adfei = dyn_cast<AutoDiffFunctionExtractInst>(inst))
      if (adfei->getExtractee() == AutoDiffFunctionExtractee::Original)
        functionSource = adfei->getFunctionOperand();

  // If `functionSource` is a `@differentiable` function, just extract the
  // associated function.
  if (auto diffableFnType = original->getType().castTo<SILFunctionType>()) {
    if (diffableFnType->isDifferentiable()) {
      auto paramIndices = diffableFnType->getDifferentiationParameterIndices();
      for (auto i : desiredIndices.parameters->getIndices()) {
        if (!paramIndices->contains(i)) {
          context.emitNondifferentiabilityError(original, invoker,
              diag::autodiff_function_nondiff_parameter_not_differentiable);
          return None;
        }
      }
      SILValue assocFn = builder.createAutoDiffFunctionExtract(
          original.getLoc(), kind, /*differentiationOrder*/ 1, functionSource);
      SILAutoDiffIndices indices(0, desiredIndices.parameters);
      return std::make_pair(assocFn, indices);
    }
  }

  // Find local function reference.
  if (auto *originalFRI =
          peerThroughFunctionConversions<FunctionRefInst>(original)) {
    auto loc = originalFRI->getLoc();
    auto *originalFn = originalFRI->getReferencedFunctionOrNull();
    auto substMap = getSubstitutionMap(original);
    // Attempt to look up a `[differentiable]` attribute that minimally
    // satisfies the specified indices.
    // TODO(TF-482): Change `lookupMinimalDifferentiableAttr` to additionally
    // check whether `[differentiable]` attribute generic requirements are
    // satisfied.
    auto *minimalAttr =
        context.lookUpMinimalDifferentiableAttr(originalFn, desiredIndices);
    if (!minimalAttr) {
      // If the function is intentionally marked as being opaque to
      // differentiation, then we should not create a task for it.
      if (originalFn->hasSemanticsAttr("autodiff.opaque")) {
        context.emitNondifferentiabilityError(original, invoker,
            diag::autodiff_opaque_function_not_differentiable);
        return None;
      }
      // Check and diagnose non-differentiable arguments.
      auto originalFnTy = originalFn->getLoweredFunctionType();
      for (unsigned paramIndex : range(originalFnTy->getNumParameters())) {
        if (desiredIndices.isWrtParameter(paramIndex) &&
            !originalFnTy->getParameters()[paramIndex]
                 .getSILStorageType()
                 .isDifferentiable(context.getModule())) {
          auto diag = context.emitNondifferentiabilityError(
              original, invoker, diag::autodiff_nondifferentiable_argument);
          return None;
        }
      }
      // Check and diagnose non-differentiable results.
      if (!originalFnTy->getResults()[desiredIndices.source]
               .getSILStorageType()
               .isDifferentiable(context.getModule())) {
        context.emitNondifferentiabilityError(
            original, invoker, diag::autodiff_nondifferentiable_result);
        return None;
      }
      // Check and diagnose external declarations.
      if (originalFn->isExternalDeclaration()) {
        context.emitNondifferentiabilityError(
            original, invoker,
            diag::autodiff_external_nondifferentiable_function);
        return None;
      }
      // Sanity check passed. Create a new `[differentiable]` attribute and
      // process it it.
      ArrayRef<Requirement> contextualRequirements;
      if (invoker.getKind() ==
          DifferentiationInvoker::Kind::IndirectDifferentiation)
        contextualRequirements =
            invoker.getIndirectDifferentiation().second->getRequirements();
      auto *newAttr = context.getOrCreateDifferentiableAttr(
          originalFn, desiredIndices, contextualRequirements);
      if (context.processDifferentiableAttribute(originalFn, newAttr, invoker))
        return None;
      minimalAttr = newAttr;
    }
    assert(minimalAttr);
    // TODO(TF-482): Move generic requirement checking logic to
    // `lookupMinimalDifferentiableAttr`.
    if (!checkRequirementsSatisfied(
            minimalAttr->getRequirements(),
            substMap, originalFn, context.getModule().getSwiftModule())) {
      context.emitNondifferentiabilityError(original, invoker,
          diag::autodiff_function_assoc_func_requirements_unmet);
      return None;
    }
    if (context.processDifferentiableAttribute(
            originalFn, minimalAttr, invoker))
      return None;
    SILFunction *assocFn = nullptr;
    switch (kind) {
    case AutoDiffAssociatedFunctionKind::JVP:
      assert(!minimalAttr->getJVPName().empty() && "Expected JVP name");
      assocFn = context.getModule().lookUpFunction(minimalAttr->getJVPName());
      break;
    case AutoDiffAssociatedFunctionKind::VJP:
      assert(!minimalAttr->getVJPName().empty() && "Expected VJP name");
      assocFn = context.getModule().lookUpFunction(minimalAttr->getVJPName());
      break;
    }
    auto *assocFnRef = builder.createFunctionRef(loc, assocFn);
    // FIXME(TF-201): Handle direct differentiation of reabstraction thunks.
    // Tentative solution: clone a new reabstraction thunk where function
    // argument has a `@differentiable` function type.
    if (originalFn->isThunk() == IsReabstractionThunk) {
      // Handle here.
    }
    auto convertedRef = reapplyFunctionConversion(
        assocFnRef, originalFRI, original, builder, loc,
        assocFn->getLoweredFunctionType()->getGenericSignature());
    return std::make_pair(convertedRef, minimalAttr->getIndices());
  }

  // Find witness method retrieval.
  if (auto *witnessMethod =
          peerThroughFunctionConversions<WitnessMethodInst>(original)) {
    auto loc = witnessMethod->getLoc();
    auto requirement = witnessMethod->getMember();
    auto *requirementDecl = requirement.getDecl();
    auto *diffAttr =
        requirementDecl->getAttrs().getAttribute<DifferentiableAttr>();
    if (!diffAttr) {
      context.emitNondifferentiabilityError(original, invoker,
          diag::autodiff_protocol_member_not_differentiable);
      return None;
    }

    // Check that the requirement indices are the same as the desired indices.
    auto *requirementParameterIndices = diffAttr->getParameterIndices();
    auto loweredRequirementIndices = requirementParameterIndices->getLowered(
        context.getASTContext(),
        requirementDecl->getInterfaceType()->castTo<AnyFunctionType>());
    SILAutoDiffIndices requirementIndices(/*source*/ 0,
                                          loweredRequirementIndices);

    // NOTE: We need to extend the capacity of desired parameter indices to
    // requirement parameter indices, because there's a argument count mismatch.
    // When `@differentiable` partial apply is supported, this problem will go
    // away.
    if (desiredIndices.source != requirementIndices.source ||
        !desiredIndices.parameters->extendingCapacity(
            context.getASTContext(),
            requirementIndices.parameters->getCapacity())
                ->isSubsetOf(requirementIndices.parameters)) {
      context.emitNondifferentiabilityError(original, invoker,
          diag::autodiff_protocol_member_subset_indices_not_differentiable);
      return None;
    }

    auto originalType = witnessMethod->getType().castTo<SILFunctionType>();
    auto assocType = originalType->getAutoDiffAssociatedFunctionType(
        requirementIndices.parameters, requirementIndices.source,
        /*differentiationOrder*/ 1, kind, builder.getModule(),
        LookUpConformanceInModule(builder.getModule().getSwiftModule()));

    // Emit a witness_method instruction pointing at the associated function.
    auto *autoDiffFuncId = AutoDiffAssociatedFunctionIdentifier::get(
        kind, /*differentiationOrder*/ 1, requirementParameterIndices,
        context.getASTContext());
    auto *ref = builder.createWitnessMethod(
        loc, witnessMethod->getLookupType(), witnessMethod->getConformance(),
        requirement.asAutoDiffAssociatedFunction(autoDiffFuncId),
        SILType::getPrimitiveObjectType(assocType));
    auto convertedRef =
        reapplyFunctionConversion(ref, witnessMethod, original, builder, loc);
    return std::make_pair(convertedRef, requirementIndices);
  }

  // Reject class methods.
  if (auto *classMethod =
          peerThroughFunctionConversions<ClassMethodInst>(original)) {
    context.emitNondifferentiabilityError(original, invoker,
        diag::autodiff_class_member_not_supported);
    return None;
  }

  // Emit the general opaque function error.
  context.emitNondifferentiabilityError(original, invoker,
      diag::autodiff_opaque_function_not_differentiable);
  return None;
}

/// Emit a zero value into the given buffer access by calling
/// `AdditiveArithmetic.zero`. The given type must conform to
/// `AdditiveArithmetic`.
static void emitZeroIntoBuffer(
    SILBuilder &builder, CanType type, SILValue bufferAccess,
    SILLocation loc) {
  auto &astCtx = builder.getASTContext();
  auto *swiftMod = builder.getModule().getSwiftModule();
  auto &typeConverter = builder.getModule().Types;
  // Look up conformance to `AdditiveArithmetic`.
  auto *additiveArithmeticProto =
      astCtx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto confRef = swiftMod->lookupConformance(type, additiveArithmeticProto);
  assert(confRef.hasValue() && "Missing conformance to `AdditiveArithmetic`");
  // Look up `AdditiveArithmetic.zero.getter`.
  auto zeroDeclLookup = additiveArithmeticProto->lookupDirect(astCtx.Id_zero);
  auto *zeroDecl = cast<VarDecl>(zeroDeclLookup.front());
  assert(zeroDecl->isProtocolRequirement());
  auto *accessorDecl = zeroDecl->getAccessor(AccessorKind::Get);
  SILDeclRef accessorDeclRef(accessorDecl, SILDeclRef::Kind::Func);
  auto silFnType = typeConverter.getConstantType(accessorDeclRef);
  // %wm = witness_method ...
  auto *getter = builder.createWitnessMethod(
      loc, type, *confRef, accessorDeclRef, silFnType);
  // %metatype = metatype $T
  auto metatypeType = CanMetatypeType::get(
      type, MetatypeRepresentation::Thick);
  auto metatype = builder.createMetatype(
      loc, SILType::getPrimitiveObjectType(metatypeType));
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      additiveArithmeticProto, type, *confRef);
  builder.createApply(loc, getter, subMap, {bufferAccess, metatype},
                      /*isNonThrowing*/ false);
}

//===----------------------------------------------------------------------===//
// Thunk helpers
//===----------------------------------------------------------------------===//
// These helpers are copied/adapted from SILGen. They should be refactored and
// moved to a shared location.
//===----------------------------------------------------------------------===//

static CanGenericSignature
buildThunkSignature(SILFunction *fn,
                    bool inheritGenericSig,
                    OpenedArchetypeType *openedExistential,
                    GenericEnvironment *&genericEnv,
                    SubstitutionMap &contextSubs,
                    SubstitutionMap &interfaceSubs,
                    ArchetypeType *&newArchetype) {
  // If there's no opened existential, we just inherit the generic environment
  // from the parent function.
  if (openedExistential == nullptr) {
    auto genericSig = fn->getLoweredFunctionType()->getGenericSignature();
    genericEnv = fn->getGenericEnvironment();
    interfaceSubs = fn->getForwardingSubstitutionMap();
    contextSubs = interfaceSubs;
    return genericSig;
  }

  auto &ctx = fn->getASTContext();
  GenericSignatureBuilder builder(ctx);

  // Add the existing generic signature.
  int depth = 0;
  if (inheritGenericSig) {
    if (auto genericSig =
            fn->getLoweredFunctionType()->getGenericSignature()) {
      builder.addGenericSignature(genericSig);
      depth = genericSig->getGenericParams().back()->getDepth() + 1;
    }
  }

  // Add a new generic parameter to replace the opened existential.
  auto *newGenericParam = GenericTypeParamType::get(depth, 0, ctx);

  builder.addGenericParameter(newGenericParam);
  Requirement newRequirement(RequirementKind::Conformance, newGenericParam,
                             openedExistential->getOpenedExistentialType());
  auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  builder.addRequirement(newRequirement, source, nullptr);

  auto *genericSig = std::move(builder).computeGenericSignature(
      SourceLoc(), /*allowConcreteGenericParams=*/true);
  genericEnv = genericSig->createGenericEnvironment();

  newArchetype = genericEnv->mapTypeIntoContext(newGenericParam)
      ->castTo<ArchetypeType>();

  // Calculate substitutions to map the caller's archetypes to the thunk's
  // archetypes.
  if (auto calleeGenericSig =
          fn->getLoweredFunctionType()->getGenericSignature()) {
    contextSubs = SubstitutionMap::get(
        calleeGenericSig,
        [&](SubstitutableType *type) -> Type {
          return genericEnv->mapTypeIntoContext(type);
        },
        MakeAbstractConformanceForGenericType());
  }

  // Calculate substitutions to map interface types to the caller's archetypes.
  interfaceSubs = SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *type) -> Type {
        if (type->isEqual(newGenericParam))
          return openedExistential;
        return fn->mapTypeIntoContext(type);
      },
      MakeAbstractConformanceForGenericType());

  return genericSig->getCanonicalSignature();

}

/// The thunk kinds used in the differentiation transform.
enum class DifferentiationThunkKind {
  /// A reabstraction thunk.
  ///
  /// Reabstraction thunks transform a function-typed value to another one with
  /// different parameter/result abstraction patterns. This is identical to the
  /// thunks generated by SILGen.
  Reabstraction,

  /// An index subset thunk.
  ///
  /// An index subset thunk is used transform JVP/VJPs into a version that is
  /// "wrt" fewer differentiation parameters.
  /// - Differentials of thunked JVPs use zero for non-requested differentiation
  //    parameters.
  /// - Pullbacks of thunked VJPs discard results for non-requested
  ///   differentiation parameters.
  IndexSubset
};

/// Build the type of a function transformation thunk.
static CanSILFunctionType buildThunkType(SILFunction *fn,
                                         CanSILFunctionType &sourceType,
                                         CanSILFunctionType &expectedType,
                                         GenericEnvironment *&genericEnv,
                                         SubstitutionMap &interfaceSubs,
                                         bool withoutActuallyEscaping,
                                         DifferentiationThunkKind thunkKind) {
  assert(!expectedType->isPolymorphic());
  assert(!sourceType->isPolymorphic());

  auto &module = fn->getModule();
  auto origType = sourceType;

  // Cannot build a reabstraction thunk without context. Ownership semantics
  // on the result type are required.
  if (thunkKind == DifferentiationThunkKind::Reabstraction)
    assert(expectedType->getExtInfo().hasContext());

  // This may inherit @noescape from the expected type. The `@noescape`
  // attribute is only stripped when using this type to materialize a new decl.
  // Use `@convention(thin)` if:
  // - Building a reabstraction thunk type.
  // - Building an index subset thunk type, where the expected type has context
  //   (i.e. is `@convention(thick)`).
  auto extInfo = expectedType->getExtInfo();
  if (thunkKind == DifferentiationThunkKind::Reabstraction ||
      extInfo.hasContext()) {
    extInfo = extInfo.withRepresentation(
        SILFunctionType::Representation::Thin);
  }
  if (withoutActuallyEscaping)
    extInfo = extInfo.withNoEscape(false);

  // Does the thunk type involve archetypes other than opened existentials?
  bool hasArchetypes = false;
  // Does the thunk type involve an open existential type?
  CanOpenedArchetypeType openedExistential;
  auto archetypeVisitor = [&](CanType t) {
    if (auto archetypeTy = dyn_cast<OpenedArchetypeType>(t)) {
      if (auto opened = dyn_cast<OpenedArchetypeType>(archetypeTy)) {
        assert((openedExistential == CanArchetypeType() ||
                openedExistential == opened) &&
               "one too many open existentials");
        openedExistential = opened;
      } else {
        hasArchetypes = true;
      }
    }
  };

  // Use the generic signature from the context if the thunk involves
  // generic parameters.
  CanGenericSignature genericSig;
  SubstitutionMap contextSubs;
  ArchetypeType *newArchetype = nullptr;

  if (expectedType->hasArchetype() || sourceType->hasArchetype()) {
    expectedType.visit(archetypeVisitor);
    sourceType.visit(archetypeVisitor);
    genericSig = buildThunkSignature(
        fn, hasArchetypes, openedExistential, genericEnv, contextSubs,
        interfaceSubs, newArchetype);
  }

  // Utility function to apply contextSubs, and also replace the
  // opened existential with the new archetype.
  auto substIntoThunkContext = [&](CanType t) -> CanType {
    return t.subst(
        [&](SubstitutableType *type) -> Type {
          if (CanType(type) == openedExistential)
            return newArchetype;
          return Type(type).subst(contextSubs);
        },
        LookUpConformanceInSubstitutionMap(contextSubs),
        SubstFlags::AllowLoweredTypes)->getCanonicalType();
  };

  sourceType = cast<SILFunctionType>(substIntoThunkContext(sourceType));
  expectedType = cast<SILFunctionType>(substIntoThunkContext(expectedType));

  // If our parent function was pseudogeneric, this thunk must also be
  // pseudogeneric, since we have no way to pass generic parameters.
  if (genericSig)
    if (origType->isPseudogeneric())
      extInfo = extInfo.withIsPseudogeneric();

  // Add the function type as the parameter.
  auto contextConvention =
      SILType::getPrimitiveObjectType(sourceType).isTrivial(*fn)
          ? ParameterConvention::Direct_Unowned
          : ParameterConvention::Direct_Guaranteed;
  SmallVector<SILParameterInfo, 4> params;
  params.append(expectedType->getParameters().begin(),
                expectedType->getParameters().end());
  // Add reabstraction function parameter only if building a reabstraction thunk
  // type.
  if (thunkKind == DifferentiationThunkKind::Reabstraction)
    params.push_back({sourceType, sourceType->getExtInfo().hasContext()
                                      ? contextConvention
                                      : ParameterConvention::Direct_Unowned});

  // Map the parameter and expected types out of context to get the interface
  // type of the thunk.
  SmallVector<SILParameterInfo, 4> interfaceParams;
  interfaceParams.reserve(params.size());
  for (auto &param : params) {
    auto paramIfaceTy = param.getType()->mapTypeOutOfContext();
    interfaceParams.push_back(SILParameterInfo(
        paramIfaceTy->getCanonicalType(genericSig), param.getConvention()));
  }

  SmallVector<SILYieldInfo, 4> interfaceYields;
  for (auto &yield : expectedType->getYields()) {
    auto yieldIfaceTy = yield.getType()->mapTypeOutOfContext();
    auto interfaceYield =
        yield.getWithType(yieldIfaceTy->getCanonicalType(genericSig));
    interfaceYields.push_back(interfaceYield);
  }

  SmallVector<SILResultInfo, 4> interfaceResults;
  for (auto &result : expectedType->getResults()) {
    auto resultIfaceTy = result.getType()->mapTypeOutOfContext();
    auto interfaceResult =
        result.getWithType(resultIfaceTy->getCanonicalType(genericSig));
    interfaceResults.push_back(interfaceResult);
  }

  Optional<SILResultInfo> interfaceErrorResult;
  if (expectedType->hasErrorResult()) {
    auto errorResult = expectedType->getErrorResult();
    auto errorIfaceTy = errorResult.getType()->mapTypeOutOfContext();
    interfaceErrorResult =
        SILResultInfo(errorIfaceTy->getCanonicalType(genericSig),
                      expectedType->getErrorResult().getConvention());
  }

  // The type of the thunk function.
  return SILFunctionType::get(
      genericSig, extInfo, expectedType->getCoroutineKind(),
      ParameterConvention::Direct_Unowned, interfaceParams, interfaceYields,
      interfaceResults, interfaceErrorResult, module.getASTContext());
}

/// Get or create a reabstraction thunk from `fromType` to `toType`, to be
/// called in `caller`.
static SILFunction *getOrCreateReabstractionThunk(SILOptFunctionBuilder &fb,
                                                  SILModule &module,
                                                  SILLocation loc,
                                                  SILFunction *caller,
                                                  CanSILFunctionType fromType,
                                                  CanSILFunctionType toType) {
  SubstitutionMap interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;
  auto thunkType = buildThunkType(
      caller, fromType, toType, genericEnv, interfaceSubs,
      /*withoutActuallyEscaping*/ false,
      DifferentiationThunkKind::Reabstraction);
  auto thunkDeclType =
      thunkType->getWithExtInfo(thunkType->getExtInfo().withNoEscape(false));

  auto fromInterfaceType = fromType->mapTypeOutOfContext()->getCanonicalType();
  auto toInterfaceType = toType->mapTypeOutOfContext()->getCanonicalType();

  Mangle::ASTMangler mangler;
  std::string name = mangler.mangleReabstractionThunkHelper(
      thunkType, fromInterfaceType, toInterfaceType,
      Type(), module.getSwiftModule());

  auto *thunk = fb.getOrCreateSharedFunction(
      loc, name, thunkDeclType, IsBare, IsTransparent, IsSerialized,
      ProfileCounter(), IsReabstractionThunk, IsNotDynamic);
  if (!thunk->empty())
    return thunk;

  thunk->setGenericEnvironment(genericEnv);
  thunk->setOwnershipEliminated();
  auto *entry = thunk->createBasicBlock();
  SILBuilder builder(entry);
  createEntryArguments(thunk);

  SILFunctionConventions fromConv(fromType, module);
  SILFunctionConventions toConv(toType, module);
  assert(toConv.useLoweredAddresses());

  auto *fnArg = thunk->getArgumentsWithoutIndirectResults().back();

  SmallVector<SILValue, 4> arguments;
  auto toArgIter = thunk->getArguments().begin();
  auto useNextArgument = [&]() {
    arguments.push_back(*toArgIter++);
  };

  SmallVector<AllocStackInst *, 4> localAllocations;
  auto createAllocStack = [&](SILType type) {
    auto *alloc = builder.createAllocStack(loc, type);
    localAllocations.push_back(alloc);
    return alloc;
  };

  // Handle indirect results.
  assert(fromType->getNumResults() == toType->getNumResults());
  for (unsigned resIdx : range(toType->getNumResults())) {
    auto fromRes = fromConv.getResults()[resIdx];
    auto toRes = toConv.getResults()[resIdx];
    // No abstraction mismatch.
    if (fromRes.isFormalIndirect() == toRes.isFormalIndirect()) {
      // If result types are indirect, directly pass as next argument.
      if (toRes.isFormalIndirect())
        useNextArgument();
      continue;
    }
    // Convert indirect result to direct result.
    if (fromRes.isFormalIndirect()) {
      SILType resultTy = fromConv.getSILType(fromRes);
      assert(resultTy.isAddress());
      auto *indRes = createAllocStack(resultTy);
      arguments.push_back(indRes);
      continue;
    }
    // Convert direct result to indirect result.
    // Increment thunk argument iterator; reabstraction handled later.
    toArgIter++;
  }

  // Reabstract parameters.
  assert(toType->getNumParameters() == fromType->getNumParameters());
  for (unsigned paramIdx : range(toType->getNumParameters())) {
    auto fromParam = fromConv.getParameters()[paramIdx];
    auto toParam = toConv.getParameters()[paramIdx];
    // No abstraction mismatch. Directly use next argument.
    if (fromParam.isFormalIndirect() == toParam.isFormalIndirect()) {
      useNextArgument();
      continue;
    }
    // Convert indirect parameter to direct parameter.
    if (fromParam.isFormalIndirect()) {
      auto paramTy = fromConv.getSILType(fromType->getParameters()[paramIdx]);
      if (!paramTy.hasArchetype())
        paramTy = thunk->mapTypeIntoContext(paramTy);
      assert(paramTy.isAddress());
      auto *toArg = *toArgIter++;
      auto *buf = createAllocStack(toArg->getType());
      builder.createStore(loc, toArg, buf,
          getBufferSOQ(toArg->getType().getASTType(), *thunk));
      arguments.push_back(buf);
      continue;
    }
    // Convert direct parameter to indirect parameter.
    assert(toParam.isFormalIndirect());
    auto *toArg = *toArgIter++;
    auto *load = builder.createLoad(loc, toArg,
        getBufferLOQ(toArg->getType().getASTType(), *thunk));
    arguments.push_back(load);
  }

  auto *apply = builder.createApply(
      loc, fnArg, SubstitutionMap(), arguments, /*isNonThrowing*/ false);

  // Get return elements.
  SmallVector<SILValue, 4> results;
  // Extract all direct results.
  SmallVector<SILValue, 4> directResults;
  extractAllElements(apply, builder, directResults);

  auto fromDirResultsIter = directResults.begin();
  auto fromIndResultsIter = apply->getIndirectSILResults().begin();
  auto toIndResultsIter = thunk->getIndirectResults().begin();
  // Reabstract results.
  for (unsigned resIdx : range(toType->getNumResults())) {
    auto fromRes = fromConv.getResults()[resIdx];
    auto toRes = toConv.getResults()[resIdx];
    // No abstraction mismatch.
    if (fromRes.isFormalIndirect() == toRes.isFormalIndirect()) {
      // If result types are direct, add call result as direct thunk result.
      if (toRes.isFormalDirect())
        results.push_back(*fromDirResultsIter++);
      // If result types are indirect, increment indirect result iterators.
      else {
        ++fromIndResultsIter;
        ++toIndResultsIter;
      }
      continue;
    }
    // Load direct results from indirect results.
    if (fromRes.isFormalIndirect()) {
      auto indRes = *fromIndResultsIter++;
      auto *load = builder.createLoad(loc, indRes,
          getBufferLOQ(indRes->getType().getASTType(), *thunk));
      results.push_back(load);
      continue;
    }
    // Store direct results to indirect results.
    assert(toRes.isFormalIndirect());
    SILType resultTy = toConv.getSILType(toRes);
    assert(resultTy.isAddress());
    auto indRes = *toIndResultsIter++;
    builder.createStore(loc, *fromDirResultsIter++, indRes,
                        getBufferSOQ(indRes->getType().getASTType(), *thunk));
  }
  auto retVal = joinElements(results, builder, loc);

  // Deallocate local allocations.
  for (auto *alloc : reversed(localAllocations))
    builder.createDeallocStack(loc, alloc);

  // Create return.
  builder.createReturn(loc, retVal);

  LLVM_DEBUG(auto &s = getADDebugStream() << "Created reabstraction thunk.\n";
             s << "  From type: " << fromType << '\n';
             s << "  To type: " << toType << '\n';
             s << '\n' << *thunk);

  return thunk;
}

/// Given an parameter argument (not indirect result) and some differentiation
/// indices, figure out whether the parent function is being differentiated with
/// respect to this parameter, according to the indices.
static bool isDifferentiationParameter(SILArgument *argument,
                                       AutoDiffIndexSubset *indices) {
  if (!argument) return false;
  auto *function = argument->getFunction();
  auto paramArgs = function->getArgumentsWithoutIndirectResults();
  for (unsigned i : indices->getIndices())
    if (paramArgs[i] == argument)
      return true;
  return false;
}

/// For a nested function call that has results active on the differentiation
/// path, compute the set of minimal indices for differentiating this function
/// as required by the data flow.
static void collectMinimalIndicesForFunctionCall(
    ApplyInst *ai, SmallVectorImpl<SILValue> &results,
    const SILAutoDiffIndices &parentIndices,
    const DifferentiableActivityInfo &activityInfo,
    SmallVectorImpl<unsigned> &paramIndices,
    SmallVectorImpl<unsigned> &resultIndices) {
  // Make sure the function call has active results.
  assert(llvm::any_of(results, [&](SILValue result) {
    return activityInfo.isActive(result, parentIndices);
  }));
  auto fnTy = ai->getCallee()->getType().castTo<SILFunctionType>();
  SILFunctionConventions convs(fnTy, ai->getModule());
  auto arguments = ai->getArgumentOperands();
  // Parameter indices are indices (in the type signature) of parameter
  // arguments that are varied or are arguments.
  unsigned currentParamIdx = 0;
  for (auto applyArg : ai->getArgumentsWithoutIndirectResults()) {
    if (activityInfo.isVaried(applyArg, parentIndices.parameters) ||
        isDifferentiationParameter(dyn_cast<SILArgument>(applyArg),
                                   parentIndices.parameters))
      paramIndices.push_back(currentParamIdx);
    ++currentParamIdx;
  }
  // Result indices are indices (in the type signature) of results that are
  // useful.
  //
  // If the function returns only one result, then we just see if that is
  // useful.
  if (fnTy->getNumDirectFormalResults() == 1) {
    if (activityInfo.isUseful(ai, parentIndices.source))
      resultIndices.push_back(0);
    return;
  }
  // If the function returns more than 1 results, the return type is a tuple. We
  // need to find all `tuple_extract`s on that tuple, and determine if each
  // found extracted element is useful.
  // Collect direct results being retrieved using `tuple_extract`.
  SmallVector<SILValue, 8> usedDirectResults(convs.getNumDirectSILResults());
  for (auto *use : ai->getUses())
    if (auto *tei = dyn_cast<TupleExtractInst>(use->getUser()))
      usedDirectResults[tei->getFieldNo()] = tei;
  // Add differentiation indices based on activity analysis.
  unsigned dirResIdx = 0;
  unsigned indResIdx = convs.getSILArgIndexOfFirstIndirectResult();
  for (auto &resAndIdx : enumerate(convs.getResults())) {
    auto &res = resAndIdx.value();
    unsigned idx = resAndIdx.index();
    if (res.isFormalDirect()) {
      if (auto dirRes = usedDirectResults[dirResIdx])
        if (dirRes && activityInfo.isUseful(dirRes, parentIndices.source))
          resultIndices.push_back(idx);
      ++dirResIdx;
    } else {
      if (activityInfo.isUseful(arguments[indResIdx].get(),
                                parentIndices.source))
        resultIndices.push_back(idx);
      ++indResIdx;
    }
  }
}

namespace {
class VJPEmitter final
    : public TypeSubstCloner<VJPEmitter, SILOptFunctionBuilder> {
  friend class PullbackEmitter;

private:
  /// The global context.
  ADContext &context;

  /// The original function.
  SILFunction *const original;

  /// The `[differentiable]` attribute.
  SILDifferentiableAttr *const attr;

  /// The VJP function.
  SILFunction *const vjp;

  /// The pullback function.
  SILFunction *pullback;

  /// The pullback info.
  PullbackInfo pullbackInfo;

  /// The differentiation invoker.
  DifferentiationInvoker invoker;

  /// Info from activity analysis on the original function.
  const DifferentiableActivityInfo &activityInfo;

  /// Caches basic blocks whose phi arguments have been remapped (adding a
  /// predecessor enum argument).
  SmallPtrSet<SILBasicBlock *, 4> remappedBasicBlocks;

  bool errorOccurred = false;

  /// Mapping from original blocks to pullback values. Used to build pullback
  /// struct instances.
  DenseMap<SILBasicBlock *, SmallVector<SILValue, 8>> pullbackValues;

  ASTContext &getASTContext() const { return vjp->getASTContext(); }
  SILModule &getModule() const { return vjp->getModule(); }
  const SILAutoDiffIndices &getIndices() const { return attr->getIndices(); }

  static SubstitutionMap getSubstitutionMap(SILFunction *original,
                                            SILFunction *vjp) {
    auto substMap = original->getForwardingSubstitutionMap();
    if (auto *vjpGenEnv = vjp->getGenericEnvironment())
      substMap = substMap.subst(vjpGenEnv->getForwardingSubstitutionMap());
    return substMap;
  }

  static const DifferentiableActivityInfo &getActivityInfo(
      ADContext &context, SILFunction *original,
      const SILAutoDiffIndices &indices, SILFunction *vjp) {
    // Get activity info of the original function.
    auto &passManager = context.getPassManager();
    auto *activityAnalysis =
        passManager.getAnalysis<DifferentiableActivityAnalysis>();
    auto &activityCollection = *activityAnalysis->get(original);
    auto &activityInfo = activityCollection.getActivityInfo(
        vjp->getLoweredFunctionType()->getGenericSignature());
    LLVM_DEBUG(dumpActivityInfo(*original, indices, activityInfo,
                                getADDebugStream()));
    return activityInfo;
  }

public:
  explicit VJPEmitter(ADContext &context, SILFunction *original,
                      SILDifferentiableAttr *attr, SILFunction *vjp,
                      DifferentiationInvoker invoker)
      : TypeSubstCloner(*vjp, *original, getSubstitutionMap(original, vjp)),
        context(context), original(original), attr(attr), vjp(vjp),
        pullbackInfo(context, original, vjp, attr->getIndices()),
        invoker(invoker), activityInfo(getActivityInfo(
                              context, original, attr->getIndices(), vjp)) {
    // Create empty pullback function.
    pullback = createEmptyPullback();
    context.getGeneratedFunctions().push_back(pullback);
  }

  SILFunction *createEmptyPullback() {
    auto &module = context.getModule();
    auto origTy = original->getLoweredFunctionType();
    auto lookupConformance = LookUpConformanceInModule(module.getSwiftModule());

    // RAII that pushes the original function's generic signature to
    // `module.Types` so that the calls `module.Types.getTypeLowering()` below
    // will know the original function's generic parameter types.
    Lowering::GenericContextScope genericContextScope(
        module.Types, origTy->getGenericSignature());

    // Given a type, returns its formal SIL parameter info.
    auto getTangentParameterInfoForOriginalResult = [&](
        CanType tanType, ResultConvention origResConv) -> SILParameterInfo {
      auto &tl = context.getTypeConverter().getTypeLowering(
          tanType, ResilienceExpansion::Minimal);
      ParameterConvention conv;
      switch (origResConv) {
      case ResultConvention::Owned:
      case ResultConvention::Autoreleased:
        conv = tl.isTrivial()
            ? ParameterConvention::Direct_Unowned
            : ParameterConvention::Direct_Guaranteed;
        break;
      case ResultConvention::Unowned:
      case ResultConvention::UnownedInnerPointer:
        conv = ParameterConvention::Direct_Unowned;
        break;
      case ResultConvention::Indirect:
        conv = ParameterConvention::Indirect_In_Guaranteed;
        break;
      }
      return {tanType, conv};
    };

    // Given a type, returns its formal SIL result info.
    auto getTangentResultInfoForOriginalParameter = [&](
        CanType tanType, ParameterConvention origParamConv) -> SILResultInfo {
      auto &tl = context.getTypeConverter().getTypeLowering(
          tanType, ResilienceExpansion::Minimal);
      ResultConvention conv;
      switch (origParamConv) {
      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Unowned:
        conv = tl.isTrivial()
            ? ResultConvention::Unowned
            : ResultConvention::Owned;
        break;
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_Inout:
      case ParameterConvention::Indirect_In_Constant:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_InoutAliasable:
        conv = ResultConvention::Indirect;
        break;
      }
      return {tanType, conv};
    };

    // Parameters of the pullback are:
    // - the tangent vectors of the original results, and
    // - a pullback struct.
    // Results of the pullback are in the tangent space of the original
    // parameters.
    SmallVector<SILParameterInfo, 8> pbParams;
    SmallVector<SILResultInfo, 8> adjResults;
    auto origParams = origTy->getParameters();
    auto indices = attr->getIndices();

    // Add pullback parameter for the seed.
    auto origResInfo = origTy->getResults()[indices.source];
    pbParams.push_back(getTangentParameterInfoForOriginalResult(
        origResInfo.getType()
            ->getAutoDiffAssociatedTangentSpace(lookupConformance)
            ->getCanonicalType(), origResInfo.getConvention()));

    // Accept a pullback struct in the pullback parameter list. This is the
    // returned pullback's closure context.
    auto *origExit = &*original->findReturnBB();
    auto *pbStruct = pullbackInfo.getPullbackStruct(origExit);
    auto pbStructType = pbStruct->getDeclaredInterfaceType()
        ->getCanonicalType();
    pbParams.push_back({pbStructType, ParameterConvention::Direct_Guaranteed});

    // Add pullback results for the requested wrt parameters.
    for (auto i : indices.parameters->getIndices()) {
      auto origParam = origParams[i];
      adjResults.push_back(getTangentResultInfoForOriginalParameter(
          origParam.getType()
              ->getAutoDiffAssociatedTangentSpace(lookupConformance)
              ->getCanonicalType(), origParam.getConvention()));
    }

    auto pbName = original->getASTContext()
                       .getIdentifier("AD__" + original->getName().str() +
                                      "__pullback_" + indices.mangle())
                       .str();
    auto pbGenericSig = getAssociatedFunctionGenericSignature(attr, original);
    auto *adjGenericEnv = pbGenericSig
        ? pbGenericSig->createGenericEnvironment()
        : nullptr;
    auto pbType = SILFunctionType::get(
        pbGenericSig, origTy->getExtInfo(), origTy->getCoroutineKind(),
        origTy->getCalleeConvention(), pbParams, {}, adjResults, None,
        original->getASTContext());

    SILOptFunctionBuilder fb(context.getTransform());
    // The generated pullback linkage is set to Hidden because generated
    // pullbacks are never called cross-module.
    auto linkage = SILLinkage::Hidden;
    auto *pullback = fb.createFunction(
        linkage, pbName, pbType, adjGenericEnv, original->getLocation(),
        original->isBare(), IsNotTransparent, original->isSerialized(),
        original->isDynamicallyReplaceable());
    pullback->setOwnershipEliminated();
    pullback->setDebugScope(new (module)
                                SILDebugScope(original->getLocation(),
                                              pullback));
    return pullback;
  }

  /// Run VJP generation. Returns true on error.
  bool run();

  void postProcess(SILInstruction *orig, SILInstruction *cloned) {
    if (errorOccurred)
      return;
    SILClonerWithScopes::postProcess(orig, cloned);
  }

  /// Remap original basic blocks, adding predecessor enum arguments.
  SILBasicBlock *remapBasicBlock(SILBasicBlock *bb) {
    auto *vjpBB = BBMap[bb];
    // If error has occurred, or if block has already been remapped, return
    // remapped, return remapped block.
    if (errorOccurred || remappedBasicBlocks.count(bb))
      return vjpBB;
    // Add predecessor enum argument to the remapped block.
    auto *predEnum = pullbackInfo.getPredecessorEnum(bb);
    auto enumTy = getOpASTType(predEnum->getDeclaredInterfaceType()
                                 ->getCanonicalType());
    auto enumLoweredTy = context.getTypeConverter().getLoweredType(
        enumTy, ResilienceExpansion::Minimal);
    vjpBB->createPhiArgument(enumLoweredTy, ValueOwnershipKind::Guaranteed);
    remappedBasicBlocks.insert(bb);
    return vjpBB;
  }

  /// General visitor for all instructions. If any error is emitted by previous
  /// visits, bail out.
  void visit(SILInstruction *inst) {
    if (errorOccurred)
      return;
    TypeSubstCloner::visit(inst);
  }

  void visitSILInstruction(SILInstruction *inst) {
    context.emitNondifferentiabilityError(inst, invoker,
        diag::autodiff_expression_not_differentiable_note);
    errorOccurred = true;
  }

private:
  /// Get the lowered SIL type of the given nominal type declaration.
  SILType getNominalDeclLoweredType(NominalTypeDecl *nominal) {
    auto nomType = getOpASTType(
        nominal->getDeclaredInterfaceType()->getCanonicalType());
    auto nomSILType = context.getTypeConverter().getLoweredType(
        nomType, ResilienceExpansion::Minimal);
    return nomSILType;
  }

  /// Build a pullback struct value for the original block corresponding to the
  /// given terminator.
  StructInst *buildPullbackValueStructValue(TermInst *termInst) {
    assert(termInst->getFunction() == original);
    auto loc = termInst->getFunction()->getLocation();
    auto *origBB = termInst->getParent();
    auto *vjpBB = BBMap[origBB];
    auto *pbStruct = pullbackInfo.getPullbackStruct(origBB);
    auto structLoweredTy = getNominalDeclLoweredType(pbStruct);
    auto bbPullbackValues = pullbackValues[origBB];
    if (!origBB->isEntry()) {
      auto *predEnumArg = vjpBB->getArguments().back();
      bbPullbackValues.insert(bbPullbackValues.begin(), predEnumArg);
    }
    return getBuilder().createStruct(loc, structLoweredTy, bbPullbackValues);
  }

  /// Build a predecessor enum instance using the given builder for the given
  /// original predecessor/successor blocks and pullback struct value.
  EnumInst *buildPredecessorEnumValue(SILBuilder &builder,
                                      SILBasicBlock *predBB,
                                      SILBasicBlock *succBB,
                                      StructInst *pbStructVal) {
    auto loc = pbStructVal->getLoc();
    auto *succEnum = pullbackInfo.getPredecessorEnum(succBB);
    auto enumLoweredTy = getNominalDeclLoweredType(succEnum);
    auto *enumEltDecl =
        pullbackInfo.lookUpPredecessorEnumElement(predBB, succBB);
    auto enumEltType = remapType(
        enumLoweredTy.getEnumElementType(enumEltDecl, getModule()));
    // If the enum element type does not have a box type (i.e. the enum case is
    // not indirect), then directly create an enum.
    auto boxType = dyn_cast<SILBoxType>(enumEltType.getASTType());
    if (!boxType)
      return builder.createEnum(loc, pbStructVal, enumEltDecl, enumLoweredTy);
    // Otherwise, box the pullback struct value and create an enum.
    auto *allocBox = builder.createAllocBox(loc, boxType);
    auto *projectBox = builder.createProjectBox(loc, allocBox, /*index*/ 0);
    builder.createStore(loc, pbStructVal, projectBox,
                        getBufferSOQ(projectBox->getType().getASTType(), *vjp));
    // NOTE(TF-585): `fix_lifetime` is generated to avoid AllocBoxToStack crash
    // for nested loop AD.
    builder.createFixLifetime(loc, allocBox);
    return builder.createEnum(loc, allocBox, enumEltDecl, enumLoweredTy);
  }

public:
  void visitReturnInst(ReturnInst *ri) {
    auto loc = ri->getOperand().getLoc();
    auto *origExit = ri->getParent();
    auto &builder = getBuilder();
    auto *pbStructVal = buildPullbackValueStructValue(ri);

    // Get the VJP value corresponding to the original functions's return value.
    auto *origRetInst = cast<ReturnInst>(origExit->getTerminator());
    auto origResult = getOpValue(origRetInst->getOperand());
    SmallVector<SILValue, 8> origResults;
    extractAllElements(origResult, builder, origResults);

    // Get and partially apply the pullback.
    auto vjpGenericEnv = vjp->getGenericEnvironment();
    auto vjpSubstMap = vjpGenericEnv
        ? vjpGenericEnv->getForwardingSubstitutionMap()
        : vjp->getForwardingSubstitutionMap();
    auto *pullbackRef = builder.createFunctionRef(loc, pullback);
    auto *pullbackPartialApply = builder.createPartialApply(
        loc, pullbackRef, vjpSubstMap, {pbStructVal},
        ParameterConvention::Direct_Guaranteed);

    // Return a tuple of the original result and pullback.
    SmallVector<SILValue, 8> directResults;
    directResults.append(origResults.begin(), origResults.end());
    directResults.push_back(pullbackPartialApply);
    builder.createReturn(
        ri->getLoc(), joinElements(directResults, builder, loc));
  }

  void visitBranchInst(BranchInst *bi) {
    // Build pullback struct value for original block.
    // Build predecessor enum value for destination block.
    auto *origBB = bi->getParent();
    auto *pbStructVal = buildPullbackValueStructValue(bi);
    auto *enumVal = buildPredecessorEnumValue(
        getBuilder(), origBB, bi->getDestBB(), pbStructVal);

    // Remap arguments, appending the new enum values.
    SmallVector<SILValue, 8> args;
    for (auto origArg : bi->getArgs())
      args.push_back(getOpValue(origArg));
    args.push_back(enumVal);

    // Create a new `br` instruction.
    getBuilder().createBranch(
        bi->getLoc(), getOpBasicBlock(bi->getDestBB()), args);
  }

  void visitCondBranchInst(CondBranchInst *cbi) {
    // Build pullback struct value for original block.
    // Build predecessor enum values for true/false blocks.
    auto *origBB = cbi->getParent();
    auto *pbStructVal = buildPullbackValueStructValue(cbi);
    auto *trueEnumVal = buildPredecessorEnumValue(
        getBuilder(), origBB, cbi->getTrueBB(), pbStructVal);
    auto *falseEnumVal = buildPredecessorEnumValue(
        getBuilder(), origBB, cbi->getFalseBB(), pbStructVal);

    // Remap arguments, appending the new enum values.
    SmallVector<SILValue, 8> trueArgs;
    for (auto &origTrueOp : cbi->getTrueOperands())
      trueArgs.push_back(getOpValue(origTrueOp.get()));
    trueArgs.push_back(trueEnumVal);

    SmallVector<SILValue, 8> falseArgs;
    for (auto &origFalseOp : cbi->getFalseOperands())
      falseArgs.push_back(getOpValue(origFalseOp.get()));
    falseArgs.push_back(falseEnumVal);

    // Create a new `cond_br` instruction.
    getBuilder().createCondBranch(
        cbi->getLoc(), getOpValue(cbi->getCondition()),
        getOpBasicBlock(cbi->getTrueBB()), trueArgs,
        getOpBasicBlock(cbi->getFalseBB()), falseArgs);
  }

  void visitSwitchEnumInst(SwitchEnumInst *sei) {
    // Build pullback struct value for original block.
    auto *origBB = sei->getParent();
    auto *pbStructVal = buildPullbackValueStructValue(sei);

    // Creates a trampoline block for given original successor block. The
    // trampoline block has the same arguments as the VJP successor block but
    // drops the last predecessor enum argument. The generated `switch_enum`
    // instruction branches to the trampoline block, and the trampoline block
    // constructs a predecessor enum value and branches to the VJP successor
    // block.
    auto createTrampolineBasicBlock =
        [&](SILBasicBlock *origSuccBB) -> SILBasicBlock * {
      auto *vjpSuccBB = getOpBasicBlock(origSuccBB);
      // Create the trampoline block.
      auto *trampolineBB = vjp->createBasicBlockBefore(vjpSuccBB);
      for (auto *arg : vjpSuccBB->getArguments().drop_back())
        trampolineBB->createPhiArgument(arg->getType(),
                                        arg->getOwnershipKind());
      // Build predecessor enum value for successor block and branch to it.
      SILBuilder trampolineBuilder(trampolineBB);
      auto *succEnumVal = buildPredecessorEnumValue(trampolineBuilder, origBB,
                                                    origSuccBB, pbStructVal);
      SmallVector<SILValue, 4> forwardedArguments(
          trampolineBB->getArguments().begin(),
          trampolineBB->getArguments().end());
      forwardedArguments.push_back(succEnumVal);
      trampolineBuilder.createBranch(sei->getLoc(), vjpSuccBB,
                                     forwardedArguments);
      return trampolineBB;
    };

    // Create trampoline successor basic blocks.
    SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 4> caseBBs;
    for (unsigned i : range(sei->getNumCases())) {
      auto caseBB = sei->getCase(i);
      auto *trampolineBB = createTrampolineBasicBlock(caseBB.second);
      caseBBs.push_back({caseBB.first, trampolineBB});
    }
    // Create trampoline default basic block.
    SILBasicBlock *newDefaultBB = nullptr;
    if (auto *defaultBB = sei->getDefaultBBOrNull().getPtrOrNull())
      newDefaultBB = createTrampolineBasicBlock(defaultBB);

    // Create a new `switch_enum` instruction.
    getBuilder().createSwitchEnum(
        sei->getLoc(), getOpValue(sei->getOperand()),
        newDefaultBB, caseBBs);
  }

  // If an `apply` has active results or active inout parameters, replace it
  // with an `apply` of its VJP.
  void visitApplyInst(ApplyInst *ai) {
    // Special handling logic only applies when `apply` has active results or
    // active arguments at an active 'inout' parameter position. If not, just do
    // standard cloning.
    SmallVector<SILValue, 4> allResults;
    allResults.push_back(ai);
    allResults.append(ai->getIndirectSILResults().begin(),
                      ai->getIndirectSILResults().end());
    auto hasActiveResults = llvm::any_of(
        allResults, [this](SILValue res) {
      return activityInfo.isActive(res, getIndices());
    });
    auto hasActiveArguments = llvm::any_of(
        ai->getArgumentsWithoutIndirectResults(), [this](SILValue arg) {
      return activityInfo.isActive(arg, getIndices());
    });
    // Check for active 'inout' arguments.
    auto paramInfos = ai->getSubstCalleeConv().getParameters();
    bool hasActiveInoutParams = false;
    for (unsigned i : swift::indices(paramInfos))
      if (paramInfos[i].isIndirectInOut() &&
          activityInfo.isActive(ai->getArgumentsWithoutIndirectResults()[i],
                                getIndices()))
        hasActiveInoutParams = true;
    // Reject functions with active inout arguments. It's not yet supported.
    if (hasActiveInoutParams) {
      context.emitNondifferentiabilityError(ai, invoker,
          diag::autodiff_cannot_differentiate_through_inout_arguments);
      errorOccurred = true;
      return;
    }
    // If there's no active results, this function should not be differentiated.
    // Do standard cloning.
    if (!hasActiveResults || !hasActiveArguments) {
      LLVM_DEBUG(getADDebugStream() << "No active results:\n" << *ai << '\n');
      TypeSubstCloner::visitApplyInst(ai);
      return;
    }

    // Get the parameter indices required for differentiating this function.
    LLVM_DEBUG(getADDebugStream() << "VJP-transforming:\n" << *ai << '\n');
    SmallVector<unsigned, 8> activeParamIndices;
    SmallVector<unsigned, 8> activeResultIndices;
    collectMinimalIndicesForFunctionCall(ai, allResults, getIndices(),
                                         activityInfo, activeParamIndices,
                                         activeResultIndices);
    assert(!activeParamIndices.empty() && "Parameter indices cannot be empty");
    assert(!activeResultIndices.empty() && "Result indices cannot be empty");
    LLVM_DEBUG(auto &s = getADDebugStream() << "Active indices: params={";
               interleave(activeParamIndices.begin(), activeParamIndices.end(),
                          [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
               s << "}, results={"; interleave(
                   activeResultIndices.begin(), activeResultIndices.end(),
                   [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
               s << "}\n";);
    // FIXME: We don't support multiple active results yet.
    if (activeResultIndices.size() > 1) {
      context.emitNondifferentiabilityError(
          ai, invoker, diag::autodiff_expression_not_differentiable_note);
      errorOccurred = true;
      return;
    }
    // Form expected indices by assuming there's only one result.
    SILAutoDiffIndices indices(activeResultIndices.front(),
        AutoDiffIndexSubset::get(
            getASTContext(),
            ai->getArgumentsWithoutIndirectResults().size(),
            activeParamIndices));

    // Emit the VJP.
    auto loc = ai->getLoc();
    auto &builder = getBuilder();
    auto original = getOpValue(ai->getCallee());
    auto functionSource = original;
    SILValue vjpValue;
    // If functionSource is a @differentiable function, just extract it.
    auto originalFnTy = original->getType().castTo<SILFunctionType>();
    if (originalFnTy->isDifferentiable()) {
      auto paramIndices = originalFnTy->getDifferentiationParameterIndices();
      for (auto i : indices.parameters->getIndices()) {
        if (!paramIndices->contains(i)) {
          context.emitNondifferentiabilityError(original, invoker,
              diag::autodiff_function_nondiff_parameter_not_differentiable);
          errorOccurred = true;
          return;
        }
      }
      vjpValue = builder.createAutoDiffFunctionExtract(
          loc, AutoDiffFunctionExtractInst::Extractee::VJP,
          /*differentiationOrder*/ 1, functionSource);
    }

    // Check and diagnose non-differentiable arguments.
    for (unsigned paramIndex : range(originalFnTy->getNumParameters())) {
      if (indices.isWrtParameter(paramIndex) &&
              !originalFnTy->getParameters()[paramIndex]
              .getSILStorageType()
              .isDifferentiable(getModule())) {
        context.emitNondifferentiabilityError(
            original, invoker, diag::autodiff_nondifferentiable_argument);
        errorOccurred = true;
        return;
      }
    }
    // Check and diagnose non-differentiable results.
    if (!originalFnTy->getResults()[indices.source]
            .getSILStorageType()
            .isDifferentiable(getModule())) {
      context.emitNondifferentiabilityError(
          original, invoker, diag::autodiff_nondifferentiable_result);
      errorOccurred = true;
      return;
    }
    // If VJP has not yet been found, emit an `autodiff_function` instruction
    // on the remapped original function operand and `autodiff_function_extract`
    // the VJP. The actual JVP/VJP functions will be populated in the
    // `autodiff_function` during the transform main loop.
    SILValue differentiableFunc;
    if (!vjpValue) {
      // FIXME: Handle indirect differentiation invokers. This may require some
      // redesign: currently, each original function + attribute pair is mapped
      // only to one invoker.
      /*
      DifferentiationInvoker indirect(ai, attr);
      auto insertion =
          context.getInvokers().try_emplace({this->original, attr}, indirect);
      auto &invoker = insertion.first->getSecond();
      invoker = indirect;
      */

      // If the original `apply` instruction has a substitution map, then the
      // applied function is specialized.
      // In the VJP, specialization is also necessary for parity. The original
      // function operand is specialized with a remapped version of same
      // substitution map using an argument-less `partial_apply`.
      if (ai->getSubstitutionMap().empty()) {
        builder.createRetainValue(loc, original, builder.getDefaultAtomicity());
      } else {
        auto substMap = getOpSubstitutionMap(ai->getSubstitutionMap());
        auto vjpPartialApply = getBuilder().createPartialApply(
            ai->getLoc(), original, substMap, {},
            ParameterConvention::Direct_Guaranteed);
        original = vjpPartialApply;
      }

      auto *autoDiffFuncInst = context.createAutoDiffFunction(
          getBuilder(), loc, indices.parameters, /*differentiationOrder*/ 1,
          original);
      differentiableFunc = autoDiffFuncInst;

      // Record the `autodiff_function` instruction.
      context.getAutoDiffFunctionInsts().push_back(autoDiffFuncInst);
      context.getResultIndices()[autoDiffFuncInst] =
          activeResultIndices.front();

      vjpValue = getBuilder().createAutoDiffFunctionExtract(
          loc, AutoDiffFunctionExtractInst::Extractee::VJP,
          /*differentiationOrder*/ 1, autoDiffFuncInst);
    }

    // Record desired/actual VJP indices.
    // Temporarily set original pullback type to `None`.
    NestedApplyInfo info{indices, /*originalPullbackType*/ None};
    auto insertion = context.getNestedApplyInfo().try_emplace(ai, info);
    auto &nestedApplyInfo = insertion.first->getSecond();
    nestedApplyInfo = info;

    // Call the VJP using the original parameters.
    SmallVector<SILValue, 8> vjpArgs;
    auto vjpFnTy = getOpType(vjpValue->getType()).castTo<SILFunctionType>();
    auto numVJPArgs =
        vjpFnTy->getNumParameters() + vjpFnTy->getNumIndirectFormalResults();
    vjpArgs.reserve(numVJPArgs);
    // Collect substituted arguments.
    for (auto origArg : ai->getArguments())
      vjpArgs.push_back(getOpValue(origArg));
    assert(vjpArgs.size() == numVJPArgs);
    // Apply the VJP.
    // The VJP should be specialized, so no substitution map is necessary.
    auto *vjpCall = getBuilder().createApply(loc, vjpValue, SubstitutionMap(),
                                             vjpArgs, ai->isNonThrowing());
    LLVM_DEBUG(getADDebugStream() << "Applied vjp function\n" << *vjpCall);

    // Release the differentiable function.
    if (differentiableFunc)
      builder.createReleaseValue(loc, differentiableFunc,
                                 builder.getDefaultAtomicity());

    // Get the VJP results (original results and pullback).
    SmallVector<SILValue, 8> vjpDirectResults;
    extractAllElements(vjpCall, getBuilder(), vjpDirectResults);
    ArrayRef<SILValue> originalDirectResults =
        ArrayRef<SILValue>(vjpDirectResults).drop_back(1);
    SILValue originalDirectResult = joinElements(originalDirectResults,
                                                 getBuilder(),
                                                 vjpCall->getLoc());
    SILValue pullback = vjpDirectResults.back();

    // Store the original result to the value map.
    mapValue(ai, originalDirectResult);

    // Checkpoint the pullback.
    auto *pullbackDecl =
        pullbackInfo.addPullbackDecl(ai, getOpType(pullback->getType()));

    // If actual pullback type does not match lowered pullback type, reabstract
    // the pullback using a thunk.
    auto actualPullbackType =
        getOpType(pullback->getType()).getAs<SILFunctionType>();
    auto vjpGenSig = SubsMap.getGenericSignature()
        ? SubsMap.getGenericSignature()->getCanonicalSignature()
        : nullptr;
    Lowering::GenericContextScope genericContextScope(
        context.getTypeConverter(), vjpGenSig);
    auto loweredPullbackType =
        getOpType(context.getTypeConverter().getLoweredType(
                      pullbackDecl->getInterfaceType()->getCanonicalType(),
                      ResilienceExpansion::Minimal))
            .castTo<SILFunctionType>();
    if (!loweredPullbackType->isEqual(actualPullbackType)) {
      // Set non-reabstracted original pullback type in nested apply info.
      nestedApplyInfo.originalPullbackType = actualPullbackType;
      SILOptFunctionBuilder fb(context.getTransform());
      auto *thunk = getOrCreateReabstractionThunk(
          fb, getModule(), loc, /*caller*/ vjp, actualPullbackType,
          loweredPullbackType);
      auto *thunkRef = getBuilder().createFunctionRef(loc, thunk);
      pullback = getBuilder().createPartialApply(
          ai->getLoc(), thunkRef,
          getOpSubstitutionMap(thunk->getForwardingSubstitutionMap()),
          {pullback}, actualPullbackType->getCalleeConvention());
    }
    pullbackValues[ai->getParent()].push_back(pullback);

    // Some instructions that produce the callee may have been cloned.
    // If the original callee did not have any users beyond this `apply`,
    // recursively kill the cloned callee.
    if (auto *origCallee = cast_or_null<SingleValueInstruction>(
            ai->getCallee()->getDefiningInstruction()))
      if (origCallee->hasOneUse())
        recursivelyDeleteTriviallyDeadInstructions(
            getOpValue(origCallee)->getDefiningInstruction());
  }

  void visitAutoDiffFunctionInst(AutoDiffFunctionInst *adfi) {
    // Clone `autodiff_function` from original to VJP, then add the cloned
    // instruction to the `autodiff_function` worklist.
    SILClonerWithScopes::visitAutoDiffFunctionInst(adfi);
    auto *newADFI = cast<AutoDiffFunctionInst>(getOpValue(adfi));
    context.getAutoDiffFunctionInsts().push_back(newADFI);
  }
};
} // end anonymous namespace

namespace {
class JVPEmitter final
    : public TypeSubstCloner<JVPEmitter, SILOptFunctionBuilder> {
private:
  /// The global context.
  ADContext &context;

  /// The original function.
  SILFunction *const original;

  /// The `[differentiable]` attribute.
  SILDifferentiableAttr *const attr;

  /// The JVP function.
  SILFunction *const jvp;

  /// The differential function.
  SILFunction *differential;

  /// The differentiation invoker.
  DifferentiationInvoker invoker;

  /// Info from activity analysis on the original function.
  const DifferentiableActivityInfo &activityInfo;

  bool errorOccurred = false;

  /// Stores the differential functions returns in the 'original' body part of the JVP, which will then be
  /// turned into the differential struct that will be partially applied onto the differential function.
  SmallVector<SILValue, 8> differentialFuncCalls;

  ASTContext &getASTContext() const { return jvp->getASTContext(); }
  SILModule &getModule() const { return jvp->getModule(); }
  const SILAutoDiffIndices &getIndices() const { return attr->getIndices(); }

  static SubstitutionMap getSubstitutionMap(SILFunction *original,
                                            SILFunction *jvp) {
    auto substMap = original->getForwardingSubstitutionMap();
    if (auto *jvpGenEnv = jvp->getGenericEnvironment())
      substMap = substMap.subst(jvpGenEnv->getForwardingSubstitutionMap());
    return substMap;
  }

  static const DifferentiableActivityInfo &getActivityInfo(
       ADContext &context, SILFunction *original,
       const SILAutoDiffIndices &indices, SILFunction *jvp) {
    // Get activity info of the original function.
    auto &passManager = context.getPassManager();
    auto *activityAnalysis =
    passManager.getAnalysis<DifferentiableActivityAnalysis>();
    auto &activityCollection = *activityAnalysis->get(original);
    auto &activityInfo = activityCollection.getActivityInfo(
        jvp->getLoweredFunctionType()->getGenericSignature());
    LLVM_DEBUG(dumpActivityInfo(*original, indices, activityInfo,
                                getADDebugStream()));
    return activityInfo;
  }

public:
  explicit JVPEmitter(ADContext &context, SILFunction *original,
                      SILDifferentiableAttr *attr, SILFunction *jvp,
                      DifferentiationInvoker invoker)
      : TypeSubstCloner(*jvp, *original, getSubstitutionMap(original, jvp)),
        context(context), original(original), attr(attr), jvp(jvp),
      invoker(invoker), activityInfo(getActivityInfo(
                            context, original, attr->getIndices(), jvp)) {
    // Get JVP generic signature.
    CanGenericSignature jvpGenSig = nullptr;
    if (auto *jvpGenEnv = jvp->getGenericEnvironment())
      jvpGenSig = jvpGenEnv->getGenericSignature()->getCanonicalSignature();

    // Create empty differential function.
    differential = createEmptyDifferential();
    context.getGeneratedFunctions().push_back(differential);
  }

  SILFunction *createEmptyDifferential() {
    auto &module = context.getModule();
    auto origTy = original->getLoweredFunctionType();
    auto lookupConformance = LookUpConformanceInModule(module.getSwiftModule());

    // RAII that pushes the original function's generic signature to
    // `module.Types` so that the calls `module.Types.getTypeLowering()` below
    // will know the original function's generic parameter types.
    Lowering::GenericContextScope genericContextScope(
        module.Types, origTy->getGenericSignature());

    // Parameters of the pullback are:
    // - the tangent vectors of the parameters we are differentiating with
    //   respect to, and
    // - a differential struct.
    // Results of the differential are in the tangent space of the original
    // results.
    SmallVector<SILParameterInfo, 8> dfParams;
    SmallVector<SILResultInfo, 8> dfResults;
    auto origParams = origTy->getParameters();
    auto indices = attr->getIndices();

    // Add differential results.
    auto origResInfo = origTy->getResults()[indices.source];
    dfResults.push_back(
        SILResultInfo(origResInfo.getType()
            ->getAutoDiffAssociatedTangentSpace(lookupConformance)
            ->getCanonicalType(), origResInfo.getConvention()));

    // Add differential parameters for the requested wrt parameters.
    for (auto i : indices.parameters->getIndices()) {
      auto origParam = origParams[i];
      dfParams.push_back(
          SILParameterInfo(origParam.getType()
              ->getAutoDiffAssociatedTangentSpace(lookupConformance)
              ->getCanonicalType(), origParam.getConvention()));
    }

    auto diffName = original->getASTContext()
        .getIdentifier("AD__" + original->getName().str() + "__differential_" +
                       indices.mangle())
        .str();
    auto diffGenericSig = getAssociatedFunctionGenericSignature(attr, original);
    auto *diffGenericEnv = diffGenericSig
        ? diffGenericSig->createGenericEnvironment()
        : nullptr;
    auto diffType = SILFunctionType::get(
        diffGenericSig, origTy->getExtInfo(), origTy->getCoroutineKind(),
        origTy->getCalleeConvention(), dfParams, {}, dfResults, None,
        original->getASTContext());

    SILOptFunctionBuilder fb(context.getTransform());
    // The generated tangent linkage is set to Hidden because generated tangent
    // are never called cross-module.
    auto linkage = SILLinkage::Hidden;
    auto *differential = fb.createFunction(
        linkage, diffName, diffType, diffGenericEnv, original->getLocation(),
        original->isBare(), IsNotTransparent, original->isSerialized(),
        original->isDynamicallyReplaceable());
    differential->setOwnershipEliminated();
    differential->setDebugScope(
        new (module) SILDebugScope(original->getLocation(), differential));

    // Create empty body of differential.
    auto diffConv = differential->getConventions();
    auto *entry = differential->createBasicBlock();
    createEntryArguments(differential);

    SILBuilder builder(entry);
    auto loc = differential->getLocation();
    builder.createReturn(loc,
                         SILUndef::get(differential->mapTypeIntoContext(
                             diffConv.getSILResultType()), *differential));
    return differential;
  }

  /// Run JVP generation. Returns true on error.
  bool run();

  void postProcess(SILInstruction *orig, SILInstruction *cloned) {
    if (errorOccurred)
      return;
    SILClonerWithScopes::postProcess(orig, cloned);
  }

  /// Remap original basic blocks, adding predecessor enum arguments.
  SILBasicBlock *remapBasicBlock(SILBasicBlock *bb) {
    auto *jvpBB = BBMap[bb];
    return jvpBB;
  }

  /// General visitor for all instructions. If any error is emitted by previous
  /// visits, bail out.
  void visit(SILInstruction *inst) {
    if (errorOccurred)
      return;
    TypeSubstCloner::visit(inst);
  }

  void visitSILInstruction(SILInstruction *inst) {
    context.emitNondifferentiabilityError(inst, invoker,
        diag::autodiff_expression_not_differentiable_note);
    errorOccurred = true;
  }

private:
  SourceFile &getDeclarationFileUnit() {
    if (original->hasLocation())
      if (auto *declContext = original->getLocation().getAsDeclContext())
        if (auto *parentSourceFile = declContext->getParentSourceFile())
          return *parentSourceFile;
    for (auto *file : original->getModule().getSwiftModule()->getFiles())
      if (auto *src = dyn_cast<SourceFile>(file))
        return *src;
    llvm_unreachable("No files?");
  }

  /// Compute and set the access level for the given pullback data structure,
  /// given the original function linkage.
  void computeAccessLevel(
      NominalTypeDecl *nominal, SILLinkage originalLinkage) {
    auto &astCtx = nominal->getASTContext();
    switch (originalLinkage) {
      case swift::SILLinkage::Public:
      case swift::SILLinkage::PublicNonABI:
        nominal->setAccess(AccessLevel::Internal);
        nominal->getAttrs().add(
            new (astCtx) UsableFromInlineAttr(/*Implicit*/ true));
        break;
      case swift::SILLinkage::Hidden:
      case swift::SILLinkage::Shared:
        nominal->setAccess(AccessLevel::Internal);
        break;
      case swift::SILLinkage::Private:
        nominal->setAccess(AccessLevel::FilePrivate);
        break;
      default:
        // When the original function has external linkage, we create an internal
        // struct for use by our own module. This is necessary for cross-cell
        // differentiation in Jupyter.
        // TODO: Add a test in the compiler that exercises a similar situation as
        // cross-cell differentiation in Jupyter.
        nominal->setAccess(AccessLevel::Internal);
    }
  }

  VarDecl *createDifferentialVarDecl(
      NominalTypeDecl *nominal, StringRef name, Type type) {
    auto &astCtx = nominal->getASTContext();
    auto id = astCtx.getIdentifier(name);
    auto *varDecl = new (astCtx) VarDecl(
       /*IsStatic*/ false, VarDecl::Specifier::Var,
       /*IsCaptureList*/ false, SourceLoc(), id, nominal);
    varDecl->setAccess(nominal->getEffectiveAccess());
    if (type->hasArchetype())
      varDecl->setInterfaceType(type->mapTypeOutOfContext());
    else
      varDecl->setInterfaceType(type);
    nominal->addMember(varDecl);
    return varDecl;
  }

public:
  void visitReturnInst(ReturnInst *ri) {
    auto loc = ri->getOperand().getLoc();
    auto *origExit = ri->getParent();
    auto &builder = getBuilder();

    // Get the JVP value corresponding to the original functions's return value.
    auto *origRetInst = cast<ReturnInst>(origExit->getTerminator());
    auto origResult = getOpValue(origRetInst->getOperand());
    SmallVector<SILValue, 8> origResults;
    extractAllElements(origResult, builder, origResults);

    // Get and partially apply the differential.
    auto jvpGenericEnv = jvp->getGenericEnvironment();
    auto jvpSubstMap = jvpGenericEnv
        ? jvpGenericEnv->getForwardingSubstitutionMap()
        : jvp->getForwardingSubstitutionMap();
    auto *differentialRef = builder.createFunctionRef(loc, differential);
    auto *differentialPartialApply = builder.createPartialApply(
        loc, differentialRef, jvpSubstMap, {},
        ParameterConvention::Direct_Guaranteed);

    // Return a tuple of the original result and pullback.
    SmallVector<SILValue, 8> directResults;
    directResults.append(origResults.begin(), origResults.end());
    directResults.push_back(differentialPartialApply);
    builder.createReturn(
        ri->getLoc(), joinElements(directResults, builder, loc));
  }

  // If an `apply` has active results or active inout parameters, replace it
  // with an `apply` of its JVP.
  void visitApplyInst(ApplyInst *ai) {
    // Special handling logic only applies when `apply` has active results or
    // active arguments at an active parameter position. If not, just do
    // standard cloning.
    SmallVector<SILValue, 4> allResults;
    allResults.push_back(ai);
    allResults.append(ai->getIndirectSILResults().begin(),
                      ai->getIndirectSILResults().end());
    auto hasActiveResults = llvm::any_of(
        allResults, [this](SILValue res) {
      return activityInfo.isActive(res, getIndices());
    });
    auto hasActiveArguments = llvm::any_of(
        ai->getArgumentsWithoutIndirectResults(), [this](SILValue arg) {
      return activityInfo.isActive(arg, getIndices());
    });
    // Check for active 'inout' arguments.
    auto paramInfos = ai->getSubstCalleeConv().getParameters();
    for (unsigned i : swift::indices(paramInfos)) {
      if (paramInfos[i].isIndirectInOut() &&
          activityInfo.isActive(ai->getArgumentsWithoutIndirectResults()[i],
                                getIndices())) {
        // Reject functions with active inout arguments. It's not yet supported.
        context.emitNondifferentiabilityError(ai, invoker,
            diag::autodiff_cannot_differentiate_through_inout_arguments);
        errorOccurred = true;
        return;
      }
    }

    // If there's no active results, this function should not be differentiated.
    // Do standard cloning.
    if (!hasActiveResults || !hasActiveArguments) {
      LLVM_DEBUG(getADDebugStream() << "No active results:\n" << *ai << '\n');
      TypeSubstCloner::visitApplyInst(ai);
      return;
    }

    // Get the parameter indices required for differentiating this function.
    LLVM_DEBUG(getADDebugStream() << "JVP-transforming:\n" << *ai << '\n');
    SmallVector<unsigned, 8> activeParamIndices;
    SmallVector<unsigned, 8> activeResultIndices;
    collectMinimalIndicesForFunctionCall(ai, allResults, getIndices(),
                                         activityInfo, activeParamIndices,
                                         activeResultIndices);
    assert(!activeParamIndices.empty() && "Parameter indices cannot be empty");
    assert(!activeResultIndices.empty() && "Result indices cannot be empty");
    LLVM_DEBUG(auto &s = getADDebugStream() << "Active indices: params={";
               interleave(activeParamIndices.begin(), activeParamIndices.end(),
                          [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
               s << "}, results={"; interleave(
                   activeResultIndices.begin(), activeResultIndices.end(),
                   [&s](unsigned i) { s << i; }, [&s] { s << ", "; });
               s << "}\n";);
    // FIXME: We don't support multiple active results yet.
    if (activeResultIndices.size() > 1) {
      context.emitNondifferentiabilityError(
          ai, invoker, diag::autodiff_expression_not_differentiable_note);
      errorOccurred = true;
      return;
    }

    // Form expected indices by assuming there's only one result.
    SILAutoDiffIndices indices(
        activeResultIndices.front(),
        AutoDiffIndexSubset::get(
            getASTContext(),
            ai->getArgumentsWithoutIndirectResults().size(),
            activeParamIndices));

    // Emit the JVP.
    auto loc = ai->getLoc();
    auto &builder = getBuilder();
    auto original = getOpValue(ai->getCallee());
    auto functionSource = original;
    SILValue jvpValue;
    // If functionSource is a @differentiable function, just extract it.
    auto originalFnTy = original->getType().castTo<SILFunctionType>();
    if (originalFnTy->isDifferentiable()) {
      auto paramIndices = originalFnTy->getDifferentiationParameterIndices();
      for (auto i : indices.parameters->getIndices()) {
        if (!paramIndices->contains(i)) {
          context.emitNondifferentiabilityError(original, invoker,
              diag::autodiff_function_nondiff_parameter_not_differentiable);
          errorOccurred = true;
          return;
        }
      }
      jvpValue = builder.createAutoDiffFunctionExtract(
          loc, AutoDiffFunctionExtractInst::Extractee::JVP,
          /*differentiationOrder*/ 1, functionSource);
    }

    // Check and diagnose non-differentiable arguments.
    for (unsigned paramIndex : range(originalFnTy->getNumParameters())) {
      if (indices.isWrtParameter(paramIndex) &&
          !originalFnTy->getParameters()[paramIndex]
          .getSILStorageType()
          .isDifferentiable(getModule())) {
        context.emitNondifferentiabilityError(
            original, invoker, diag::autodiff_nondifferentiable_argument);
        errorOccurred = true;
        return;
      }
    }

    // Check and diagnose non-differentiable results.
    if (!originalFnTy->getResults()[indices.source]
        .getSILStorageType()
        .isDifferentiable(getModule())) {
      context.emitNondifferentiabilityError(
          original, invoker, diag::autodiff_nondifferentiable_result);
      errorOccurred = true;
      return;
    }

    // If VJP has not yet been found, emit an `autodiff_function` instruction
    // on the remapped original function operand and `autodiff_function_extract`
    // the VJP. The actual JVP/VJP functions will be populated in the
    // `autodiff_function` during the transform main loop.
    SILValue differentiableFunc;
    if (!jvpValue) {
      // FIXME: Handle indirect differentiation invokers. This may require some
      // redesign: currently, each original function + attribute pair is mapped
      // only to one invoker.
      /*
       DifferentiationInvoker indirect(ai, attr);
       auto insertion =
       context.getInvokers().try_emplace({this->original, attr}, indirect);
       auto &invoker = insertion.first->getSecond();
       invoker = indirect;
       */

      // If the original `apply` instruction has a substitution map, then the
      // applied function is specialized.
      // In the JVP, specialization is also necessary for parity. The original
      // function operand is specialized with a remapped version of same
      // substitution map using an argument-less `partial_apply`.
      if (ai->getSubstitutionMap().empty()) {
        builder.createRetainValue(loc, original, builder.getDefaultAtomicity());
      } else {
        auto substMap = getOpSubstitutionMap(ai->getSubstitutionMap());
        auto jvpPartialApply = getBuilder().createPartialApply(
            ai->getLoc(), original, substMap, {},
            ParameterConvention::Direct_Guaranteed);
        original = jvpPartialApply;
      }

      auto *autoDiffFuncInst = context.createAutoDiffFunction(
          getBuilder(), loc, indices.parameters, /*differentiationOrder*/ 1,
          original);
      differentiableFunc = autoDiffFuncInst;

      // Record the `autodiff_function` instruction.
      context.getAutoDiffFunctionInsts().push_back(autoDiffFuncInst);
      context.getResultIndices()[autoDiffFuncInst] =
          activeResultIndices.front();

      jvpValue = getBuilder().createAutoDiffFunctionExtract(
          loc, AutoDiffFunctionExtractInst::Extractee::JVP,
          /*differentiationOrder*/ 1, autoDiffFuncInst);
    }

    // Record desired/actual JVP indices.
    // Temporarily set original differential type to `None`.
    NestedApplyInfo info{indices, /*originalDifferentialType*/ None};
    auto insertion = context.getNestedApplyInfo().try_emplace(ai, info);
    auto &nestedApplyInfo = insertion.first->getSecond();
    nestedApplyInfo = info;

    // Call the JVP using the original parameters.
    SmallVector<SILValue, 8> jvpArgs;
    auto jvpFnTy = getOpType(jvpValue->getType()).castTo<SILFunctionType>();
    auto numJVPArgs =
        jvpFnTy->getNumParameters() + jvpFnTy->getNumIndirectFormalResults();
    jvpArgs.reserve(numJVPArgs);
    // Collect substituted arguments.
    for (auto origArg : ai->getArguments())
      jvpArgs.push_back(getOpValue(origArg));
    assert(jvpArgs.size() == numJVPArgs);
    // Apply the JVP.
    // The JVP should be specialized, so no substitution map is necessary.
    auto *jvpCall = getBuilder().createApply(loc, jvpValue, SubstitutionMap(),
                                             jvpArgs, ai->isNonThrowing());
    LLVM_DEBUG(getADDebugStream() << "Applied jvp function\n" << *jvpCall);

    // Release the differentiable function.
    if (differentiableFunc)
      builder.createReleaseValue(loc, differentiableFunc,
                                 builder.getDefaultAtomicity());

    // Get the JVP results (original results and differential).
    SmallVector<SILValue, 8> jvpDirectResults;
    extractAllElements(jvpCall, getBuilder(), jvpDirectResults);
    ArrayRef<SILValue> originalDirectResults =
        ArrayRef<SILValue>(jvpDirectResults).drop_back(1);
    SILValue originalDirectResult = joinElements(originalDirectResults,
                                                 getBuilder(),
                                                 jvpCall->getLoc());

    // Store the original result to the value map.
    mapValue(ai, originalDirectResult);

    // Some instructions that produce the callee may have been cloned.
    // If the original callee did not have any users beyond this `apply`,
    // recursively kill the cloned callee.
    if (auto *origCallee = cast_or_null<SingleValueInstruction>(
            ai->getCallee()->getDefiningInstruction()))
      if (origCallee->hasOneUse())
        recursivelyDeleteTriviallyDeadInstructions(
            getOpValue(origCallee)->getDefiningInstruction());
  }

  void visitAutoDiffFunctionInst(AutoDiffFunctionInst *adfi) {
    // Clone `autodiff_function` from original to JVP, then add the cloned
    // instruction to the `autodiff_function` worklist.
    SILClonerWithScopes::visitAutoDiffFunctionInst(adfi);
    auto *newADFI = cast<AutoDiffFunctionInst>(getOpValue(adfi));
    context.getAutoDiffFunctionInsts().push_back(newADFI);
  }
};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// AdjointValue - a symbolic representation for adjoint values that allows
// for efficient differentiation of aggregates.
//===----------------------------------------------------------------------===//

namespace {
class PullbackEmitter;
class AdjointValue;

class Cleanup {
public:
  using Func = void(*)(SILBuilder &, SILLocation, SILValue);

private:
  SILValue value;
  Func func;
  unsigned numChildren;

  Cleanup **getChildrenData() {
    return reinterpret_cast<Cleanup **>(this + 1);
  }

  Cleanup(SILValue value, Func func, ArrayRef<Cleanup *> children)
      : value(value), func(func), numChildren(children.size()) {
    assert(((func && value) || !func) &&
           "Value must be non-null when the function is non-null");
    assert(llvm::all_of(children, [](Cleanup *c) { return (bool)c; }));
    LLVM_DEBUG(getADDebugStream() << "Creating a cleanup with " << numChildren
               << " children.\n");
    std::uninitialized_copy(children.begin(), children.end(),
                            getChildrenData());
    assert(llvm::all_of(llvm::zip(children, getChildren()),
                        [](std::tuple<Cleanup *, Cleanup *> pair) {
      return std::get<0>(pair) == std::get<1>(pair);
    }));
  }

public:
  Cleanup() = delete;
  Cleanup(Cleanup &) = delete;
  Cleanup &operator=(const Cleanup &) = delete;

  static Cleanup *create(llvm::BumpPtrAllocator &allocator, SILValue value,
                         Func func, ArrayRef<Cleanup *> children) {
    auto *buf = allocator.Allocate(
        sizeof(Cleanup) + sizeof(Cleanup *) * children.size(),
        alignof(Cleanup));
    return new (buf) Cleanup(value, func, children);
  }

  unsigned getNumChildren() const {
    return numChildren;
  }

  ArrayRef<Cleanup *> getChildren() const {
    return {const_cast<Cleanup *>(this)->getChildrenData(), numChildren};
  }

  /// Disable this cleanup and makes its application a no-op.
  void disable() {
    func = nullptr;
  }

  /// Apply and invaliate the cleanup.
  void apply(SILBuilder &builder, SILLocation loc) {
    if (!func) return;
    assert(value);
    LLVM_DEBUG(getADDebugStream() << "Running `Cleanup::apply` for " << value);
    func(builder, loc, value);
    func = nullptr;
  }

  /// Apply the cleanup and its children recursively and invalidate them.
  void applyRecursively(SILBuilder &builder, SILLocation loc) {
    apply(builder, loc);
    for (auto *child : getChildren()) {
      assert(child);
      child->applyRecursively(builder, loc);
    }
  }
};

class ValueWithCleanup {
private:
  SILValue value;
  Cleanup *cleanup;

public:
  explicit ValueWithCleanup(SILValue value = SILValue(),
                            Cleanup *cleanup = nullptr)
      : value(value), cleanup(cleanup) {}
  ValueWithCleanup(const ValueWithCleanup &) = default;

public:
  SILValue getValue() const { return value; }
  operator SILValue() const { return getValue(); }
  void setValue(SILValue value) { this->value = value; }
  Cleanup *getCleanup() const { return cleanup; }
  void setCleanup(Cleanup *cleanup) { this->cleanup = cleanup; }

  SILLocation getLoc() const { return value.getLoc(); }
  SILType getType() const { return value->getType(); }
};

enum AdjointValueKind {
  /// An empty adjoint, i.e. zero. This case exists due to its special
  /// mathematical properties: `0 + x = x`. This is a guaranteed optimization
  /// when we combine a zero adjoint with another (e.g. differentiating a
  /// fanout).
  Zero,

  /// An aggregate of adjoint values.
  Aggregate,

  /// A concrete SIL value.
  Concrete,
};

class AdjointValueBase {
  friend class AdjointValue;

  /// The kind of this adjoint value.
  AdjointValueKind kind;

  /// The type of this value as if it were materialized as a SIL value.
  SILType type;

  /// The underlying value.
  union Value {
    ArrayRef<AdjointValue> aggregate;
    ValueWithCleanup concrete;
    Value(ArrayRef<AdjointValue> v) : aggregate(v) {}
    Value(ValueWithCleanup v) : concrete(v) {}
    Value() {}
  } value;

  explicit AdjointValueBase(SILType type,
                            ArrayRef<AdjointValue> aggregate)
      : kind(AdjointValueKind::Aggregate), type(type), value(aggregate) {}

  explicit AdjointValueBase(ValueWithCleanup v)
      : kind(AdjointValueKind::Concrete), type(v.getType()), value(v) {}

  explicit AdjointValueBase(SILType type)
      : kind(AdjointValueKind::Zero), type(type) {}
};

/// A symbolic adjoint value that is capable of representing zero value 0 and
/// 1, in addition to a materialized SILValue. This is expected to be passed
/// around by value in most cases, as it's two words long.
class AdjointValue final {
  friend class PullbackEmitter;

private:
  /// The kind of this adjoint value.
  AdjointValueBase *base;
  /*implicit*/ AdjointValue(AdjointValueBase *base = nullptr) : base(base) {}

public:
  AdjointValueBase *operator->() const { return base; }
  AdjointValueBase &operator*() const { return *base; }

  static AdjointValue createConcrete(llvm::BumpPtrAllocator &allocator,
                                     ValueWithCleanup value) {
    return new (allocator.Allocate<AdjointValueBase>()) AdjointValueBase(value);
  }

  template<typename EltRange>
  static AdjointValue createAggregate(llvm::BumpPtrAllocator &allocator,
                                      SILType type, EltRange elements) {
    AdjointValue *buf = reinterpret_cast<AdjointValue *>(allocator.Allocate(
        elements.size() * sizeof(AdjointValue), alignof(AdjointValue)));
    MutableArrayRef<AdjointValue> elementsCopy(buf, elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            elementsCopy.begin());
    return new (allocator.Allocate<AdjointValueBase>())
        AdjointValueBase(type, elementsCopy);
  }

  static AdjointValue createZero(llvm::BumpPtrAllocator &allocator,
                                 SILType type) {
    return new (allocator.Allocate<AdjointValueBase>()) AdjointValueBase(type);
  }

  AdjointValueKind getKind() const { return base->kind; }
  SILType getType() const { return base->type; }
  CanType getSwiftType() const { return getType().getASTType(); }

  NominalTypeDecl *getAnyNominal() const {
    return getSwiftType()->getAnyNominal();
  }

  bool isZero() const { return getKind() == AdjointValueKind::Zero; }
  bool isAggregate() const { return getKind() == AdjointValueKind::Aggregate; }
  bool isConcrete() const { return getKind() == AdjointValueKind::Concrete; }

  unsigned getNumAggregateElements() const {
    assert(isAggregate());
    return base->value.aggregate.size();
  }

  AdjointValue getAggregateElement(unsigned i) const {
    assert(isAggregate());
    return base->value.aggregate[i];
  }

  ArrayRef<AdjointValue> getAggregateElements() const {
    return base->value.aggregate;
  }

  ValueWithCleanup getConcreteValue() const {
    assert(isConcrete());
    return base->value.concrete;
  }

  void print(llvm::raw_ostream &s) const {
    switch (getKind()) {
    case AdjointValueKind::Zero:
      s << "Zero";
      break;
    case AdjointValueKind::Aggregate:
      s << "Aggregate<";
      if (auto *decl =
            getType().getASTType()->getStructOrBoundGenericStruct()) {
        s << "Struct>(";
        interleave(llvm::zip(decl->getStoredProperties(),
                             base->value.aggregate),
                             [&s](std::tuple<VarDecl *,
                                             const AdjointValue &> elt) {
                               s << std::get<0>(elt)->getName() << ": ";
                               std::get<1>(elt).print(s);
                             }, [&s] { s << ", "; });
      } else if (auto tupleType = getType().getAs<TupleType>()) {
        s << "Tuple>(";
        interleave(base->value.aggregate,
                   [&s](const AdjointValue &elt) { elt.print(s); },
                   [&s] { s << ", "; });
      } else {
        llvm_unreachable("Invalid aggregate");
      }
      s << ')';
      break;
    case AdjointValueKind::Concrete:
      s << "Concrete(" << base->value.concrete.getValue() << ')';
      break;
    }
  }
};

enum TangentValueKind {
  /// An empty adjoint, i.e. zero. This case exists due to its special
  /// mathematical properties: `0 + x = x`. This is a guaranteed optimization
  /// when we combine a zero adjoint with another (e.g. differentiating a
  /// fanout).
  ZeroT,

  /// An aggregate of adjoint values.
  AggregateT,

  /// A concrete SIL value.
  ConcreteT,
};

class TangentValueBase {
  friend class TangentValue;

  /// The kind of this tangent value.
  TangentValueKind kind;

  /// The type of this value as if it were materialized as a SIL value.
  SILType type;

  /// The underlying value.
  union Value {
    ArrayRef<AdjointValue> aggregate;
    ValueWithCleanup concrete;
    Value(ArrayRef<AdjointValue> v) : aggregate(v) {}
    Value(ValueWithCleanup v) : concrete(v) {}
    Value() {}
  } value;

  explicit TangentValueBase(SILType type,
                            ArrayRef<AdjointValue> aggregate)
  : kind(TangentValueKind::AggregateT), type(type), value(aggregate) {}

  explicit TangentValueBase(ValueWithCleanup v)
  : kind(TangentValueKind::ConcreteT), type(v.getType()), value(v) {}

  explicit TangentValueBase(SILType type)
  : kind(TangentValueKind::ZeroT), type(type) {}
};

/// A symbolic adjoint value that is capable of representing zero value 0 and
/// 1, in addition to a materialized SILValue. This is expected to be passed
/// around by value in most cases, as it's two words long.
class TangentValue final {

private:
  /// The kind of this adjoint value.
  TangentValueBase *base;
  /*implicit*/ TangentValue(TangentValueBase *base = nullptr) : base(base) {}

public:
  TangentValueBase *operator->() const { return base; }
  TangentValueBase &operator*() const { return *base; }

  static TangentValue createConcrete(llvm::BumpPtrAllocator &allocator,
                                     ValueWithCleanup value) {
    return new (allocator.Allocate<AdjointValueBase>()) TangentValueBase(value);
  }

  template<typename EltRange>
  static TangentValue createAggregate(llvm::BumpPtrAllocator &allocator,
                                      SILType type, EltRange elements) {
    TangentValue *buf = reinterpret_cast<TangentValue *>(allocator.Allocate(
        elements.size() * sizeof(TangentValue), alignof(TangentValue)));
    MutableArrayRef<AdjointValue> elementsCopy(buf, elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            elementsCopy.begin());
    return new (allocator.Allocate<TangentValueBase>())
    TangentValueBase(type, elementsCopy);
  }

  static TangentValue createZero(llvm::BumpPtrAllocator &allocator,
                                 SILType type) {
    return new (allocator.Allocate<TangentValueBase>()) TangentValueBase(type);
  }

  TangentValueKind getKind() const { return base->kind; }
  SILType getType() const { return base->type; }
  CanType getSwiftType() const { return getType().getASTType(); }

  NominalTypeDecl *getAnyNominal() const {
    return getSwiftType()->getAnyNominal();
  }

  bool isZero() const { return getKind() == AdjointValueKind::Zero; }
  bool isAggregate() const { return getKind() == AdjointValueKind::Aggregate; }
  bool isConcrete() const { return getKind() == AdjointValueKind::Concrete; }

  unsigned getNumAggregateElements() const {
    assert(isAggregate());
    return base->value.aggregate.size();
  }

  AdjointValue getAggregateElement(unsigned i) const {
    assert(isAggregate());
    return base->value.aggregate[i];
  }

  ArrayRef<AdjointValue> getAggregateElements() const {
    return base->value.aggregate;
  }

  ValueWithCleanup getConcreteValue() const {
    assert(isConcrete());
    return base->value.concrete;
  }

  void print(llvm::raw_ostream &s) const {
    switch (getKind()) {
      case TangentValueKind::ZeroT:
        s << "Zero";
        break;
      case TangentValueKind::AggregateT:
        s << "Aggregate<";
        if (auto *decl =
            getType().getASTType()->getStructOrBoundGenericStruct()) {
          s << "Struct>(";
          interleave(llvm::zip(decl->getStoredProperties(),
                               base->value.aggregate),
                     [&s](std::tuple<VarDecl *,
                          const AdjointValue &> elt) {
            s << std::get<0>(elt)->getName() << ": ";
            std::get<1>(elt).print(s);
          }, [&s] { s << ", "; });
        } else if (auto tupleType = getType().getAs<TupleType>()) {
          s << "Tuple>(";
          interleave(base->value.aggregate,
                     [&s](const AdjointValue &elt) { elt.print(s); },
                     [&s] { s << ", "; });
        } else {
          llvm_unreachable("Invalid aggregate");
        }
        s << ')';
        break;
      case TangentValueKind::ConcreteT:
        s << "Concrete(" << base->value.concrete.getValue() << ')';
        break;
    }
  }
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const AdjointValue &adjVal) {
  adjVal.print(os);
  return os;
}

} // end anonymous namespace

//===----------------------------------------------------------------------===//
// PullbackEmitter - visitors on the original function for pullback code
// generation
//===----------------------------------------------------------------------===//

namespace {
class PullbackEmitter final : public SILInstructionVisitor<PullbackEmitter> {
private:
  /// The parent VJP emitter.
  VJPEmitter &vjpEmitter;

  /// Dominance info for the original function.
  DominanceInfo *domInfo = nullptr;

  /// Post-dominance info for the original function.
  PostDominanceInfo *postDomInfo = nullptr;

  /// Post-order info for the original function.
  PostOrderFunctionInfo *postOrderInfo = nullptr;

  /// Mapping from original basic blocks and original values to corresponding
  /// adjoint values.
  DenseMap<std::pair<SILBasicBlock *, SILValue>, AdjointValue> valueMap;

  /// Mapping from original basic blocks and original buffers to corresponding
  /// adjoint buffers.
  DenseMap<std::pair<SILBasicBlock *, SILValue>, ValueWithCleanup> bufferMap;

  /// Mapping from original basic blocks to corresponding pullback basic blocks.
  /// Pullback basic blocks always have the predecessor as the single argument.
  DenseMap<SILBasicBlock *, SILBasicBlock *> adjointBBMap;

  /// Mapping from pullback basic blocks to pullback struct arguments.
  DenseMap<SILBasicBlock *, SILArgument *> pullbackStructArguments;

  /// Mapping from original basic blocks and successor basic blocks to
  /// corresponding pullback trampoline basic blocks. Trampoline basic blocks
  /// take additional arguments in addition to the predecessor enum argument.
  DenseMap<std::pair<SILBasicBlock *, SILBasicBlock *>, SILBasicBlock *>
      pullbackTrampolineBBMap;

  /// Mapping from original basic blocks to dominated active values.
  DenseMap<SILBasicBlock *, SmallVector<SILValue, 8>> activeValues;

  /// Mapping from original basic blocks and original active values to
  /// corresponding adjoint block arguments.
  DenseMap<std::pair<SILBasicBlock *, SILValue>, SILArgument *>
      activeValueAdjointBBArgumentMap;

  /// Mapping from original basic blocks to local adjoint values to be cleaned
  /// up. This is populated when adjoint emission is run on one basic block and
  /// cleaned before processing another basic block.
  DenseMap<SILBasicBlock *, SmallVector<AdjointValue, 8>>
      blockLocalAdjointValues;

  /// Stack buffers allocated for storing local adjoint values.
  SmallVector<ValueWithCleanup, 8> functionLocalAllocations;

  /// The seed argument in the pullback function.
  SILArgument *seed = nullptr;

  /// The main builder.
  SILBuilder builder;

  /// An auxiliary local allocation builder.
  SILBuilder localAllocBuilder;

  llvm::BumpPtrAllocator allocator;

  bool errorOccurred = false;

  ADContext &getContext() const { return vjpEmitter.context; }
  SILModule &getModule() const { return getContext().getModule(); }
  ASTContext &getASTContext() const { return getPullback().getASTContext(); }
  SILFunction &getOriginal() const { return *vjpEmitter.original; }
  SILFunction &getPullback() const { return *vjpEmitter.pullback; }
  SILDifferentiableAttr *getAttr() const { return vjpEmitter.attr; }
  DifferentiationInvoker getInvoker() const { return vjpEmitter.invoker; }
  PullbackInfo &getPullbackInfo() { return vjpEmitter.pullbackInfo; }
  const SILAutoDiffIndices &getIndices() const {
    return vjpEmitter.getIndices();
  }
  const DifferentiableActivityInfo &getActivityInfo() const {
    return vjpEmitter.activityInfo;
  }
  SILArgument *getPullbackBlockPullbackStructArgument(SILBasicBlock *origBB) {
#ifndef NDEBUG
    auto *pbStruct = pullbackStructArguments[origBB]->getType()
        .getStructOrBoundGenericStruct();
    assert(pbStruct == getPullbackInfo().getPullbackStruct(origBB));
#endif
    return pullbackStructArguments[origBB];
  }

public:
  explicit PullbackEmitter(VJPEmitter &vjpEmitter)
      : vjpEmitter(vjpEmitter), builder(getPullback()),
        localAllocBuilder(getPullback()) {
    // Get dominance and post-order info for the original function.
    auto &passManager = getContext().getPassManager();
    auto *domAnalysis = passManager.getAnalysis<DominanceAnalysis>();
    auto *postDomAnalysis = passManager.getAnalysis<PostDominanceAnalysis>();
    auto *postOrderAnalysis = passManager.getAnalysis<PostOrderAnalysis>();
    domInfo = domAnalysis->get(vjpEmitter.original);
    postDomInfo = postDomAnalysis->get(vjpEmitter.original);
    postOrderInfo = postOrderAnalysis->get(vjpEmitter.original);
  }

private:
  //--------------------------------------------------------------------------//
  // Managed value factory methods
  //--------------------------------------------------------------------------//

  Cleanup *makeCleanup(SILValue value, Cleanup::Func func,
                       ArrayRef<Cleanup *> children = {});

  Cleanup *makeCleanupFromChildren(ArrayRef<Cleanup *> children);

  AdjointValue makeZeroAdjointValue(SILType type);

  AdjointValue makeConcreteAdjointValue(ValueWithCleanup value);

  template<typename EltRange>
  AdjointValue makeAggregateAdjointValue(SILType type, EltRange elements);

  //--------------------------------------------------------------------------//
  // Symbolic value materializers
  //--------------------------------------------------------------------------//

  /// Materialize an adjoint value. The type of the given adjoint value must be
  /// loadable.
  ValueWithCleanup materializeAdjointDirect(AdjointValue val,
                                            SILLocation loc);

  /// Materialize an adjoint value indirectly to a SIL buffer.
  void materializeAdjointIndirect(
      AdjointValue val, ValueWithCleanup &destBuffer);

  /// Materialize the given adjoint value indirectly to the specified buffer.
  /// The root address derivation of `seedBufAccess` must be the result of
  /// a `begin_access`.
  void materializeAdjointIndirectHelper(
      AdjointValue val, ValueWithCleanup &destBufferAccess);

  //--------------------------------------------------------------------------//
  // Helpers for symbolic value materializers
  //--------------------------------------------------------------------------//

  /// Emit a zero value by calling `AdditiveArithmetic.zero`. The given type
  /// must conform to `AdditiveArithmetic`.
  void emitZeroIndirect(CanType type, SILValue bufferAccess, SILLocation loc);

  /// Emit a zero value by calling `AdditiveArithmetic.zero`. The given type
  /// must conform to `AdditiveArithmetic` and be loadable in SIL.
  SILValue emitZeroDirect(CanType type, SILLocation loc);

  //--------------------------------------------------------------------------//
  // Memory cleanup tools
  //--------------------------------------------------------------------------//

  void emitCleanupForAdjointValue(AdjointValue value);

  //--------------------------------------------------------------------------//
  // Accumulator
  //--------------------------------------------------------------------------//

  /// Materialize an adjoint value in the most efficient way.
  ValueWithCleanup materializeAdjoint(AdjointValue val, SILLocation loc);

  /// Given two adjoint values, accumulate them.
  AdjointValue accumulateAdjointsDirect(AdjointValue lhs, AdjointValue rhs);

  /// Given two materialized adjoint values, accumulate them. These two
  /// adjoints must be objects of loadable type.
  SILValue accumulateDirect(SILValue lhs, SILValue rhs);

  /// Given two materialized adjoint values, accumulate them using
  /// `AdditiveArithmetic.+`, depending on the differentiation mode.
  void accumulateIndirect(SILValue resultBufAccess,
                          SILValue lhsBufAccess, SILValue rhsBufAccess);

  /// Given two buffers of an `AdditiveArithmetic` type, accumulate the right
  /// hand side into the left hand side using `+=`.
  void accumulateIndirect(SILValue lhsDestAccess, SILValue rhsAccess);

  //--------------------------------------------------------------------------//
  // Type transformer
  //--------------------------------------------------------------------------//

  /// Remap any archetypes into the current function's context.
  SILType remapType(SILType ty) {
    if (ty.hasArchetype())
      return getPullback().mapTypeIntoContext(ty.mapTypeOutOfContext());
    return getPullback().mapTypeIntoContext(ty);
  }

  Optional<VectorSpace> getTangentSpace(CanType type) {
    return type->getAutoDiffAssociatedTangentSpace(
        LookUpConformanceInModule(getModule().getSwiftModule()));
  }

  /// Assuming the given type conforms to `Differentiable` after remapping,
  /// returns the associated tangent space type.
  SILType getRemappedTangentType(SILType type) {
    return SILType::getPrimitiveType(
        getTangentSpace(remapType(type).getASTType())->getCanonicalType(),
        type.getCategory());
  }

  /// Substitutes all replacement types of the given substitution map using the
  /// adjoint function's substitution map.
  SubstitutionMap remapSubstitutionMap(SubstitutionMap substMap) {
    return substMap.subst(getPullback().getForwardingSubstitutionMap());
  }

  //--------------------------------------------------------------------------//
  // Managed value mapping
  //--------------------------------------------------------------------------//

  /// Returns true if the original value has a corresponding adjoint value.
  bool hasAdjointValue(SILBasicBlock *origBB, SILValue originalValue) const {
    assert(origBB->getParent() == &getOriginal());
    assert(originalValue->getType().isObject());
    return valueMap.count({origBB, originalValue});
  }

  /// Initializes an original value's corresponding adjoint value. Its adjoint
  /// value must not be present before this function is called.
  void initializeAdjointValue(SILBasicBlock *origBB, SILValue originalValue,
                              AdjointValue adjointValue) {
    assert(origBB->getParent() == &getOriginal());
    assert(originalValue->getType().isObject());
    assert(adjointValue.getType().isObject());
    assert(originalValue->getFunction() == &getOriginal());
    // The adjoint value must be in the tangent space.
    assert(adjointValue.getType() ==
               getRemappedTangentType(originalValue->getType()));
    auto insertion =
        valueMap.try_emplace({origBB, originalValue}, adjointValue);
    assert(insertion.second && "Adjoint value inserted before");
  }

  /// Get the adjoint for an original value. The given value must be in the
  /// original function.
  ///
  /// This method first tries to find an entry in `adjointMap`. If an adjoint
  /// doesn't exist, create a zero adjoint.
  AdjointValue getAdjointValue(SILBasicBlock *origBB, SILValue originalValue) {
    assert(origBB->getParent() == &getOriginal());
    assert(originalValue->getType().isObject());
    assert(originalValue->getFunction() == &getOriginal());
    auto insertion = valueMap.try_emplace(
        {origBB, originalValue}, makeZeroAdjointValue(
            getRemappedTangentType(originalValue->getType())));
    auto it = insertion.first;
    return it->getSecond();
  }

  /// Add an adjoint value for the given original value.
  void addAdjointValue(SILBasicBlock *origBB, SILValue originalValue,
                       AdjointValue newAdjointValue) {
    assert(origBB->getParent() == &getOriginal());
    assert(originalValue->getType().isObject());
    assert(newAdjointValue.getType().isObject());
    assert(originalValue->getFunction() == &getOriginal());
    LLVM_DEBUG(getADDebugStream() << "Adding adjoint for " << originalValue);
    // The adjoint value must be in the tangent space.
    assert(newAdjointValue.getType() ==
               getRemappedTangentType(originalValue->getType()));
    auto insertion =
        valueMap.try_emplace({origBB, originalValue}, newAdjointValue);
    auto inserted = insertion.second;
    if (inserted)
      return;
    // If adjoint already exists, accumulate the adjoint onto the existing
    // adjoint.
    auto it = insertion.first;
    auto &&existingValue = it->getSecond();
    valueMap.erase(it);
    auto adjVal = accumulateAdjointsDirect(existingValue, newAdjointValue);
    initializeAdjointValue(origBB, originalValue, adjVal);
    blockLocalAdjointValues[origBB].push_back(adjVal);
  }

  /// Get the adjoint block argument corresponding to the given original block
  /// and active value.
  SILArgument *getActiveValueAdjointBlockArgument(SILBasicBlock *origBB,
                                                  SILValue activeValue) {
    assert(origBB->getParent() == &getOriginal());
    auto adjointBBArg = activeValueAdjointBBArgumentMap[{origBB, activeValue}];
    assert(adjointBBArg);
    assert(adjointBBArg->getParent() == getAdjointBlock(origBB));
    return adjointBBArg;
  }

  //--------------------------------------------------------------------------//
  // Buffer mapping
  //--------------------------------------------------------------------------//

  void setAdjointBuffer(SILBasicBlock *origBB,
                        SILValue originalBuffer,
                        ValueWithCleanup adjointBuffer) {
    assert(originalBuffer->getType().isAddress());
    auto insertion =
        bufferMap.try_emplace({origBB, originalBuffer}, adjointBuffer);
    assert(insertion.second); (void)insertion;
  }

  SILValue getAdjointProjection(SILBasicBlock *origBB,
                                SILValue originalProjection) {
    // Handle `struct_element_addr`.
    if (auto *seai = dyn_cast<StructElementAddrInst>(originalProjection)) {
      auto adjSource = getAdjointBuffer(origBB, seai->getOperand());
      auto *tangentVectorDecl =
          adjSource.getType().getStructOrBoundGenericStruct();
      auto tanFieldLookup =
          tangentVectorDecl->lookupDirect(seai->getField()->getName());
      assert(tanFieldLookup.size() == 1);
      auto *tanField = cast<VarDecl>(tanFieldLookup.front());
      return builder.createStructElementAddr(
          seai->getLoc(), adjSource.getValue(), tanField);
    }
    // Handle `tuple_element_addr`.
    if (auto *teai = dyn_cast<TupleElementAddrInst>(originalProjection)) {
      auto source = teai->getOperand();
      auto adjSource = getAdjointBuffer(origBB, source);
      if (!adjSource.getType().is<TupleType>())
        return adjSource;
      auto origTupleTy = source->getType().castTo<TupleType>();
      unsigned adjIndex = 0;
      for (unsigned i : range(teai->getFieldNo())) {
        if (getTangentSpace(
                origTupleTy->getElement(i).getType()->getCanonicalType()))
          ++adjIndex;
      }
      return builder.createTupleElementAddr(
          teai->getLoc(), adjSource.getValue(), adjIndex);
    }
    // Handle `begin_access`.
    if (auto *bai = dyn_cast<BeginAccessInst>(originalProjection)) {
      auto adjBase = getAdjointBuffer(origBB, bai->getOperand());
      if (errorOccurred)
        return (bufferMap[{origBB, originalProjection}] = ValueWithCleanup());
      // Return the base buffer's adjoint buffer.
      return adjBase;
    }
    return SILValue();
  }

  SILBasicBlock::iterator getNextFunctionLocalAllocationInsertionPoint() {
    // If there are no local allocations, insert at the adjont entry beginning.
    if (functionLocalAllocations.empty())
      return getPullback().getEntryBlock()->begin();
    // Otherwise, insert before the last local allocation. Inserting before
    // rather than after ensures that allocation and zero initialization
    // instructions are grouped together.
    auto lastLocalAlloc = functionLocalAllocations.back().getValue();
    auto it = lastLocalAlloc->getDefiningInstruction()->getIterator();
    return it;
  }

  ValueWithCleanup &getAdjointBuffer(SILBasicBlock *origBB,
                                     SILValue originalBuffer) {
    assert(originalBuffer->getType().isAddress());
    assert(originalBuffer->getFunction() == &getOriginal());
    auto insertion = bufferMap.try_emplace({origBB, originalBuffer},
                                           ValueWithCleanup(SILValue()));
    if (!insertion.second) // not inserted
      return insertion.first->getSecond();

    // Diagnose `struct_element_addr` instructions to `@noDerivative` fields.
    if (auto *seai = dyn_cast<StructElementAddrInst>(originalBuffer)) {
      if (seai->getField()->getAttrs().hasAttribute<NoDerivativeAttr>()) {
        getContext().emitNondifferentiabilityError(
            originalBuffer, getInvoker(),
            diag::autodiff_noderivative_stored_property);
        errorOccurred = true;
        return (bufferMap[{origBB, originalBuffer}] = ValueWithCleanup());
      }
    }

    // If the original buffer is a projection, return a corresponding projection
    // into the adjoint buffer.
    if (auto adjProj = getAdjointProjection(origBB, originalBuffer)) {
      ValueWithCleanup projWithCleanup(
          adjProj, makeCleanup(adjProj, /*cleanup*/ nullptr));
      return (bufferMap[{origBB, originalBuffer}] = projWithCleanup);
    }

    // Set insertion point for local allocation builder: before the last local
    // allocation, or at the start of the adjoint function's entry if no local
    // allocations exist yet.
    localAllocBuilder.setInsertionPoint(
        getPullback().getEntryBlock(),
        getNextFunctionLocalAllocationInsertionPoint());
    // Allocate local buffer and initialize to zero.
    auto *newBuf = localAllocBuilder.createAllocStack(
        originalBuffer.getLoc(),
        getRemappedTangentType(originalBuffer->getType()));
    auto *access = localAllocBuilder.createBeginAccess(
        newBuf->getLoc(), newBuf, SILAccessKind::Init,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    // Temporarily change global builder insertion point and emit zero into the
    // local buffer.
    auto insertionPoint = builder.getInsertionBB();
    builder.setInsertionPoint(
        localAllocBuilder.getInsertionBB(),
        localAllocBuilder.getInsertionPoint());
    emitZeroIndirect(access->getType().getASTType(), access, access->getLoc());
    builder.setInsertionPoint(insertionPoint);
    localAllocBuilder.createEndAccess(
        access->getLoc(), access, /*aborted*/ false);
    // Create cleanup for local buffer.
    ValueWithCleanup bufWithCleanup(newBuf, makeCleanup(newBuf, emitCleanup));
    functionLocalAllocations.push_back(bufWithCleanup);
    return (insertion.first->getSecond() = bufWithCleanup);
  }

  // Accumulates `rhsBufferAccess` into the adjoint buffer corresponding to
  // `originalBuffer`.
  void addToAdjointBuffer(SILBasicBlock *origBB, SILValue originalBuffer,
                          SILValue rhsBufferAccess) {
    assert(originalBuffer->getType().isAddress() &&
           rhsBufferAccess->getType().isAddress());
    assert(originalBuffer->getFunction() == &getOriginal());
    assert(rhsBufferAccess->getFunction() == &getPullback());
    auto adjointBuffer = getAdjointBuffer(origBB, originalBuffer);
    if (errorOccurred)
      return;
    auto *destAccess = builder.createBeginAccess(
        rhsBufferAccess.getLoc(), adjointBuffer, SILAccessKind::Modify,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    accumulateIndirect(destAccess, rhsBufferAccess);
    builder.createEndAccess(
        destAccess->getLoc(), destAccess, /*aborted*/ false);
  }

  //--------------------------------------------------------------------------//
  // CFG mapping
  //--------------------------------------------------------------------------//

  SILBasicBlock *getAdjointBlock(SILBasicBlock *originalBlock) {
    return adjointBBMap.lookup(originalBlock);
  }

  SILBasicBlock *getAdjointTrampolineBlock(
      SILBasicBlock *originalBlock, SILBasicBlock *successorBlock) {
    return pullbackTrampolineBBMap.lookup({originalBlock, successorBlock});
  }

  //--------------------------------------------------------------------------//
  // Other utilities
  //--------------------------------------------------------------------------//
  
  bool shouldBeDifferentiated(SILInstruction *inst,
                              const SILAutoDiffIndices &indices) {
    // Anything with an active result should be differentiated.
    if (llvm::any_of(inst->getResults(), [&](SILValue val) {
          return getActivityInfo().isActive(val, indices);
        }))
      return true;
    if (auto *ai = dyn_cast<ApplyInst>(inst)) {
      // Function applications with an active indirect result should be
      // differentiated.
      for (auto indRes : ai->getIndirectSILResults())
        if (getActivityInfo().isActive(indRes, indices))
          return true;
      // Function applications with an inout argument should be differentiated.
      auto paramInfos = ai->getSubstCalleeConv().getParameters();
      for (auto i : swift::indices(paramInfos))
        if (paramInfos[i].isIndirectInOut() &&
            getActivityInfo().isActive(
                ai->getArgumentsWithoutIndirectResults()[i], indices))
          return true;
    }
    // Instructions that may write to memory and that have an active operand
    // should be differentiated.
    if (inst->mayWriteToMemory())
      for (auto &op : inst->getAllOperands())
        if (getActivityInfo().isActive(op.get(), indices))
          return true;
    return false;
  }

public:
  //--------------------------------------------------------------------------//
  // Entry point
  //--------------------------------------------------------------------------//

  /// Performs adjoint synthesis on the empty adjoint function. Returns true if
  /// any error occurs.
  bool run() {
    auto &original = getOriginal();
    auto &pullback = getPullback();
    auto pbLoc = getPullback().getLocation();
    LLVM_DEBUG(getADDebugStream() << "Running PullbackEmitter on\n" << original);

    auto *adjGenEnv = getPullback().getGenericEnvironment();
    auto adjGenSig = adjGenEnv
        ? adjGenEnv->getGenericSignature()->getCanonicalSignature()
        : nullptr;
    Lowering::GenericContextScope genericContextScope(
        getContext().getTypeConverter(), adjGenSig);
    auto origExitIt = original.findReturnBB();
    assert(origExitIt != original.end() &&
           "Functions without returns must have been diagnosed");
    auto *origExit = &*origExitIt;

    // Get dominated active values in original blocks.
    // Adjoint values of dominated active values are passed as adjoint block
    // arguments.
    DominanceOrder domOrder(original.getEntryBlock(), domInfo);
    while (auto *bb = domOrder.getNext()) {
      auto &bbActiveValues = activeValues[bb];
      // If the current block has an immediate dominator, append the immediate
      // dominator block's active values to the current block's active values.
      if (auto *domNode = domInfo->getNode(bb)->getIDom()) {
        auto &domBBActiveValues = activeValues[domNode->getBlock()];
        bbActiveValues.append(domBBActiveValues.begin(),
                              domBBActiveValues.end());
      }
      SmallPtrSet<SILValue, 8> visited(bbActiveValues.begin(),
                                       bbActiveValues.end());
      // Register a value as active if it has not yet been visited.
      auto addActiveValue = [&](SILValue v) {
        if (visited.count(v))
          return;
        // Diagnose active enum values. Differentiation of enum values is not
        // yet supported; requires special adjoint handling.
        if (v->getType().getEnumOrBoundGenericEnum()) {
          getContext().emitNondifferentiabilityError(
              v, getInvoker(), diag::autodiff_enums_unsupported);
          errorOccurred = true;
        }
        // Skip address projections.
        // Address projections do not need their own adjoint buffers; they
        // become projections into their adjoint base buffer.
        if (Projection::isAddressProjection(v))
          return;
        visited.insert(v);
        bbActiveValues.push_back(v);
      };
      // Register bb arguments and all instruction operands/results.
      for (auto *arg : bb->getArguments())
        if (getActivityInfo().isActive(arg, getIndices()))
          addActiveValue(arg);
      for (auto &inst : *bb) {
        for (auto op : inst.getOperandValues())
          if (getActivityInfo().isActive(op, getIndices()))
            addActiveValue(op);
        for (auto result : inst.getResults())
          if (getActivityInfo().isActive(result, getIndices()))
            addActiveValue(result);
      }
      if (errorOccurred)
        break;
      domOrder.pushChildren(bb);
    }
    if (errorOccurred)
      return true;

    // Create adjoint blocks and arguments, visiting original blocks in
    // post-order post-dominance order.
    SmallVector<SILBasicBlock *, 8> postOrderPostDomOrder;
    // Start from the root node, which may have a marker `nullptr` block if
    // there are multiple roots.
    PostOrderPostDominanceOrder postDomOrder(postDomInfo->getRootNode(),
                                             postOrderInfo, original.size());
    while (auto *origNode = postDomOrder.getNext()) {
      auto *origBB = origNode->getBlock();
      postDomOrder.pushChildren(origNode);
      // If node is the `nullptr` marker basic block, do not push it.
      if (!origBB)
        continue;
      postOrderPostDomOrder.push_back(origBB);
    }
    for (auto *origBB : postOrderPostDomOrder) {
      auto *adjointBB = pullback.createBasicBlock();
      adjointBBMap.insert({origBB, adjointBB});
      auto pbStructLoweredType =
          remapType(getPullbackInfo().getPullbackStructLoweredType(origBB));
      // If the BB is the original exit, then the adjoint block that we just
      // created must be the pullback function's entry. For the adjoint entry,
      // create entry arguments and continue to the next block.
      if (origBB == origExit) {
        assert(adjointBB->isEntry());
        createEntryArguments(&getPullback());
        auto *lastArg = adjointBB->getArguments().back();
        assert(lastArg->getType() == pbStructLoweredType);
        pullbackStructArguments[origBB] = lastArg;
        continue;
      }
      // Get all active values in the original block.
      // If the original block has no active values, continue.
      auto &bbActiveValues = activeValues[origBB];
      if (bbActiveValues.empty())
        continue;
      // Otherwise, if the original block has active values:
      // - For each active buffer in the original block, allocate a new local
      //   buffer in the adjoint entry. (All adjoint buffers are allocated in
      //   the adjoint entry and deallocated in the adjoint exit.)
      // - For each active value in the original block, add adjoint value
      //   arguments to the adjoint block.
      for (auto activeValue : bbActiveValues) {
        if (activeValue->getType().isAddress()) {
          // Allocate and zero initialize a new local buffer using
          // `getAdjointBuffer`.
          builder.setInsertionPoint(pullback.getEntryBlock());
          getAdjointBuffer(origBB, activeValue);
        } else {
          // Create and register adjoint block argument for the active value.
          auto *adjointArg = adjointBB->createPhiArgument(
              getRemappedTangentType(activeValue->getType()),
              ValueOwnershipKind::Guaranteed);
          activeValueAdjointBBArgumentMap[{origBB, activeValue}] = adjointArg;
        }
      }
      // Add a pullback struct argument.
      auto *pbStructArg = adjointBB->createPhiArgument(
          pbStructLoweredType, ValueOwnershipKind::Guaranteed);
      pullbackStructArguments[origBB] = pbStructArg;
      // - Create adjoint trampoline blocks for each successor block of the
      //   original block. Adjoint trampoline blocks only have a pullback
      //   struct argument, and branch from the adjoint successor block to the
      //   adjoint original block, trampoline adjoint values of active values.
      for (auto *succBB : origBB->getSuccessorBlocks()) {
        auto *adjointTrampolineBB = pullback.createBasicBlockBefore(adjointBB);
        pullbackTrampolineBBMap.insert({{origBB, succBB}, adjointTrampolineBB});
        // Get the enum element type (i.e. the pullback struct type). The enum
        // element type may be boxed if the enum is indirect.
        auto enumLoweredTy =
            getPullbackInfo().getPredecessorEnumLoweredType(succBB);
        auto *enumEltDecl =
            getPullbackInfo().lookUpPredecessorEnumElement(origBB, succBB);
        auto enumEltType = remapType(
            enumLoweredTy.getEnumElementType(enumEltDecl, getModule()));
        adjointTrampolineBB->createPhiArgument(enumEltType,
                                               ValueOwnershipKind::Guaranteed);
      }
    }

    auto *adjointEntry = pullback.getEntryBlock();
    // The adjoint function has type (seed, exit_pbs) -> ([arg0], ..., [argn]).
    auto adjParamArgs = pullback.getArgumentsWithoutIndirectResults();
    assert(adjParamArgs.size() == 2);
    seed = adjParamArgs[0];

    // Assign adjoint for original result.
    SmallVector<SILValue, 8> origFormalResults;
    collectAllFormalResultsInTypeOrder(original, origFormalResults);
    auto origResult = origFormalResults[getIndices().source];
    // Emit warning if original result is not varied, because it will always
    // have a zero derivative.
    if (!getActivityInfo().isVaried(origResult, getIndices().parameters)) {
      // Emit fixit if original result has a valid source location.
      auto startLoc = origResult.getLoc().getStartSourceLoc();
      auto endLoc = origResult.getLoc().getEndSourceLoc();
      if (startLoc.isValid() && endLoc.isValid()) {
        getContext()
            .diagnose(startLoc, diag::autodiff_nonvaried_result_fixit)
            .fixItInsert(startLoc, "withoutDerivative(at:")
            .fixItInsertAfter(endLoc, ")");
      }
    }
    builder.setInsertionPoint(
        adjointEntry, getNextFunctionLocalAllocationInsertionPoint());
    if (seed->getType().isAddress()) {
      // Create a local copy so that it can be written to by later adjoint
      // zero'ing logic.
      auto *seedBufCopy = builder.createAllocStack(pbLoc, seed->getType());
      builder.createCopyAddr(pbLoc, seed, seedBufCopy, IsNotTake,
                             IsInitialization);
      if (seed->getType().isLoadable(builder.getFunction()))
        builder.createRetainValueAddr(pbLoc, seedBufCopy,
                                      builder.getDefaultAtomicity());
      ValueWithCleanup seedBufferCopyWithCleanup(
          seedBufCopy, makeCleanup(seedBufCopy, emitCleanup));
      setAdjointBuffer(origExit, origResult, seedBufferCopyWithCleanup);
      functionLocalAllocations.push_back(seedBufferCopyWithCleanup);
    } else {
      builder.createRetainValue(pbLoc, seed, builder.getDefaultAtomicity());
      initializeAdjointValue(origExit, origResult, makeConcreteAdjointValue(
          ValueWithCleanup(seed, makeCleanup(seed, emitCleanup))));
    }
    LLVM_DEBUG(getADDebugStream()
               << "Assigned seed " << *seed
               << " as the adjoint of original result " << origResult);

    // Visit original blocks blocks in post-order and perform differentiation
    // in corresponding adjoint blocks.
    for (auto *bb : postOrderPostDomOrder) {
      if (errorOccurred)
        break;
      // Get the corresponding adjoint basic block.
      auto adjBB = getAdjointBlock(bb);
      builder.setInsertionPoint(adjBB);

      LLVM_DEBUG({
        auto &s = getADDebugStream()
            << "Original bb" + std::to_string(bb->getDebugID())
            << ": To differentiate or not to differentiate?\n";
        for (auto &inst : reversed(*bb)) {
          s << (shouldBeDifferentiated(&inst, getIndices()) ? "[∂] " : "[ ] ")
            << inst;
        }
      });

      // Visit each instruction in reverse order.
      for (auto &inst : reversed(*bb)) {
        if (!shouldBeDifferentiated(&inst, getIndices()))
          continue;
        // Differentiate instruction.
        visit(&inst);
        if (errorOccurred)
          return true;
      }

      // If the original block is the original entry, then the adjoint block is
      // the adjoint exit, which is handled specially below this loop. Continue.
      if (bb->isEntry())
        continue;

      // Otherwise, add a `switch_enum` terminator for non-exit adjoint blocks.
      // 1. Get the pullback struct adjoint bb argument.
      // 2. Extract the predecessor enum value from the pullback struct value.
      auto *pbStructVal = getPullbackBlockPullbackStructArgument(bb);
      auto *predEnum = getPullbackInfo().getPredecessorEnum(bb);
      auto *predEnumField =
          getPullbackInfo().lookUpPullbackStructPredecessorField(bb);
      auto *predEnumVal =
          builder.createStructExtract(pbLoc, pbStructVal, predEnumField);

      // Propagate adjoint values from active basic block arguments to
      // predecessor terminator operands.
      for (auto *bbArg : bb->getArguments()) {
        if (!getActivityInfo().isActive(bbArg, getIndices()))
          continue;
        // Get predecessor terminator operands.
        SmallVector<std::pair<SILBasicBlock *, SILValue>, 4> incomingValues;
        bbArg->getSingleTerminatorOperands(incomingValues);
        // Initialize adjoint value of predecessor terminator operands as
        // adjoint value of current block arguments.
        auto bbArgAdj = getAdjointValue(bb, bbArg);
        for (auto pair : incomingValues) {
          auto *predBB = std::get<0>(pair);
          auto incomingValue = std::get<1>(pair);
          initializeAdjointValue(predBB, incomingValue, bbArgAdj);
        }
      }

      // 3. Build the adjoint successor cases for the `switch_enum` instruction.
      //    The adjoint successors correspond to the predecessors of the current
      //    block.
      SmallVector<std::pair<EnumElementDecl *, SILBasicBlock *>, 4>
          adjointSuccessorCases;
      for (auto *predBB : bb->getPredecessorBlocks()) {
        // Get the adjoint block and optional adjoint trampoline block of the
        // predecessor block.
        auto *adjointBB = getAdjointBlock(predBB);
        auto *adjointTrampolineBB = getAdjointTrampolineBlock(predBB, bb);
        SILBasicBlock *adjointSuccBB = nullptr;
        // If the predecessor block does not have a corresponding adjoint
        // trampoline block, then the adjoint successor is the adjoint block.
        if (!adjointTrampolineBB) {
          adjointSuccBB = adjointBB;
        }
        // Otherwise, the adjoint successor is the adjoint trampoline block,
        // which branches to the adjoint block and propagates adjoint values of
        // active values.
        else {
          adjointSuccBB = adjointTrampolineBB;
          assert(adjointSuccBB && adjointSuccBB->getNumArguments() == 1);
          SILBuilder adjointTrampolineBBBuilder(adjointSuccBB);
          SmallVector<SILValue, 8> trampolineArguments;
          // Propagate adjoint values/buffers of active values/buffers to
          // predecessor blocks.
          auto &predBBActiveValues = activeValues[predBB];
          for (auto activeValue : predBBActiveValues) {
            if (activeValue->getType().isObject()) {
              auto activeValueAdj = getAdjointValue(bb, activeValue);
              auto concreteActiveValueAdj =
                  materializeAdjointDirect(activeValueAdj, pbLoc);
              // Emit cleanups for children.
              if (auto *cleanup = concreteActiveValueAdj.getCleanup()) {
                cleanup->disable();
                cleanup->applyRecursively(builder, pbLoc);
              }
              trampolineArguments.push_back(concreteActiveValueAdj);
              // If the adjoint block does not yet have a registered adjoint
              // value for the active value, set the adjoint value to the
              // forwarded adjoint value argument.
              // TODO: Hoist this logic out of loop over predecessor blocks to
              // remove the `hasAdjointValue` check.
              if (!hasAdjointValue(predBB, activeValue)) {
                auto *adjointBBArg =
                    getActiveValueAdjointBlockArgument(predBB, activeValue);
                auto forwardedArgAdj = makeConcreteAdjointValue(
                    ValueWithCleanup(adjointBBArg,
                                     makeCleanup(adjointBBArg, emitCleanup)));
                initializeAdjointValue(predBB, activeValue, forwardedArgAdj);
              }
            } else {
              // Propagate adjoint buffers using `copy_addr`.
              auto adjBuf = getAdjointBuffer(bb, activeValue);
              auto predAdjBuf = getAdjointBuffer(predBB, activeValue);
              // FIXME: Propagate cleanups to fix memory leaks.
              predAdjBuf.setCleanup(makeCleanupFromChildren(
                  {adjBuf.getCleanup(), predAdjBuf.getCleanup()}));
              builder.createCopyAddr(
                  pbLoc, adjBuf, predAdjBuf, IsNotTake, IsNotInitialization);
            }
          }
          // Propagate pullback struct argument.
          auto *predPBStructVal = adjointTrampolineBB->getArguments().front();
          auto boxType =
              dyn_cast<SILBoxType>(predPBStructVal->getType().getASTType());
          if (!boxType) {
            trampolineArguments.push_back(predPBStructVal);
          } else {
            auto *projectBox = adjointTrampolineBBBuilder.createProjectBox(
                pbLoc, predPBStructVal, /*index*/ 0);
            auto *loadInst = adjointTrampolineBBBuilder.createLoad(
                pbLoc, projectBox,
                getBufferLOQ(projectBox->getType().getASTType(), pullback));
            trampolineArguments.push_back(loadInst);
          }
          // Branch from adjoint trampoline block to adjoint block.
          adjointTrampolineBBBuilder.createBranch(pbLoc, adjointBB,
                                                  trampolineArguments);
        }
        auto *enumEltDecl =
            getPullbackInfo().lookUpPredecessorEnumElement(predBB, bb);
        adjointSuccessorCases.push_back({enumEltDecl, adjointSuccBB});
      }
      // Emit cleanups for all block-local adjoint values.
      for (auto adjVal : blockLocalAdjointValues[bb])
        emitCleanupForAdjointValue(adjVal);
      // - If the original block has exactly one predecessor, then the adjoint
      //   block has exactly one successor. Extract the pullback struct value
      //   from the predecessor enum value using `unchecked_enum_data` and
      //   branch to the adjoint successor block.
      assert(adjointSuccessorCases.size() == predEnum->getNumElements());
      if (adjointSuccessorCases.size() == 1) {
        auto *predBB = bb->getSinglePredecessorBlock();
        assert(predBB);
        EnumElementDecl *enumEltDecl;
        SILBasicBlock *adjointSuccBB;
        std::tie(enumEltDecl, adjointSuccBB) = adjointSuccessorCases.front();
        auto *predPBStructVal =
            builder.createUncheckedEnumData(pbLoc, predEnumVal, enumEltDecl);
        builder.createBranch(pbLoc, adjointSuccBB, {predPBStructVal});
      }
      // - Otherwise, if the original block has multiple predecessors, then the
      //   adjoint block has multiple successors. Do `switch_enum` to branch on
      //   the predecessor enum values to adjoint successor blocks.
      else {
        builder.createSwitchEnum(
            pbLoc, predEnumVal, /*DefaultBB*/ nullptr, adjointSuccessorCases);
      }
    }

    // If errors occurred, back out.
    if (errorOccurred)
      return true;

    // Place the builder at the adjoint exit, i.e. the adjoint block
    // corresponding to the original entry. Return the adjoints wrt parameters
    // in the adjoint exit.
    auto *origEntry = getOriginal().getEntryBlock();
    builder.setInsertionPoint(getAdjointBlock(origEntry));

    // This vector will contain all the materialized return elements.
    SmallVector<SILValue, 8> retElts;
    // This vector will contain all indirect parameter adjoint buffers.
    SmallVector<ValueWithCleanup, 4> indParamAdjoints;

    auto origParams = original.getArgumentsWithoutIndirectResults();

    // Materializes the return element corresponding to the parameter
    // `parameterIndex` into the `retElts` vector.
    auto addRetElt = [&](unsigned parameterIndex) -> void {
      auto origParam = origParams[parameterIndex];
      if (origParam->getType().isObject()) {
        auto adjVal = getAdjointValue(origEntry, origParam);
        auto val = materializeAdjointDirect(adjVal, pbLoc);
        if (auto *cleanup = val.getCleanup()) {
          LLVM_DEBUG(getADDebugStream() << "Disabling cleanup for "
                     << val.getValue() << "for return\n");
          cleanup->disable();
          LLVM_DEBUG(getADDebugStream() << "Applying "
                     << cleanup->getNumChildren() << " child cleanups\n");
          cleanup->applyRecursively(builder, pbLoc);
        }
        retElts.push_back(val);
      } else {
        auto adjBuf = getAdjointBuffer(origEntry, origParam);
        if (errorOccurred)
          return;
        indParamAdjoints.push_back(adjBuf);
      }
    };
    // Collect differentiation parameter adjoints.
    for (auto i : getIndices().parameters->getIndices())
      addRetElt(i);
    // Emit cleanups for all local values.
    for (auto adjVal : blockLocalAdjointValues[origEntry])
      emitCleanupForAdjointValue(adjVal);

    // Disable cleanup for original indirect parameter adjoint buffers.
    // Copy them to adjoint indirect results.
    assert(indParamAdjoints.size() == pullback.getIndirectResults().size() &&
           "Indirect parameter adjoint count mismatch");
    for (auto pair : zip(indParamAdjoints, pullback.getIndirectResults())) {
      auto &source = std::get<0>(pair);
      auto &dest = std::get<1>(pair);
      builder.createCopyAddr(pbLoc, source, dest, IsTake, IsInitialization);
      if (auto *cleanup = source.getCleanup())
        cleanup->disable();
    }

    builder.setInsertionPoint(getAdjointBlock(origEntry));
    // Deallocate local allocations.
    for (auto alloc : functionLocalAllocations) {
      // Assert that local allocations have at least one use.
      // Buffers should not be allocated needlessly.
      assert(!alloc.getValue()->use_empty());
      if (auto *cleanup = alloc.getCleanup())
        cleanup->applyRecursively(builder, pbLoc);
      builder.createDeallocStack(pbLoc, alloc);
    }
    builder.createReturn(pbLoc, joinElements(retElts, builder, pbLoc));

    LLVM_DEBUG(getADDebugStream() << "Generated adjoint for "
                                  << original.getName() << ":\n" << pullback);
    return errorOccurred;
  }

  void visit(SILInstruction *inst) {
    if (errorOccurred)
      return;

    LLVM_DEBUG(getADDebugStream() << "PullbackEmitter visited:\n[ORIG]"
               << *inst);
#ifndef NDEBUG
    auto beforeInsertion = std::prev(builder.getInsertionPoint());
#endif
    SILInstructionVisitor::visit(inst);
    LLVM_DEBUG({
      auto &s = llvm::dbgs() << "[ADJ] Emitted:\n";
      auto afterInsertion = builder.getInsertionPoint();
      for (auto it = ++beforeInsertion; it != afterInsertion; ++it)
        s << *it;
    });
  }

  void visitSILInstruction(SILInstruction *inst) {
    LLVM_DEBUG(getADDebugStream()
               << "Unhandled instruction in adjoint emitter: " << *inst);
    getContext().emitNondifferentiabilityError(inst, getInvoker(),
        diag::autodiff_expression_not_differentiable_note);
    errorOccurred = true;
  }

  AllocStackInst *
  emitDifferentiableViewSubscript(ApplyInst *ai, SILType elType,
                                  SILValue adjointArray, SILValue fnRef,
                                  CanGenericSignature genericSig, int index) {
    auto &ctx = builder.getASTContext();
    auto astType = elType.getASTType();
    auto literal = builder.createIntegerLiteral(
        ai->getLoc(), SILType::getBuiltinIntegerType(64, ctx), index);
    auto intType = SILType::getPrimitiveObjectType(
        ctx.getIntDecl()->getDeclaredType()->getCanonicalType());
    auto intStruct = builder.createStruct(ai->getLoc(), intType, {literal});
    AllocStackInst *subscriptBuffer =
        builder.createAllocStack(ai->getLoc(), elType);
    auto swiftModule = getModule().getSwiftModule();
    auto diffProto = ctx.getProtocol(KnownProtocolKind::Differentiable);
    auto diffConf = swiftModule->lookupConformance(astType, diffProto);
    assert(diffConf.hasValue() && "Missing conformance to `Differentiable`");
    auto addArithProto = ctx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
    auto addArithConf = swiftModule->lookupConformance(astType, addArithProto);
    assert(addArithConf.hasValue() &&
           "Missing conformance to `AdditiveArithmetic`");
    auto subMap =
        SubstitutionMap::get(genericSig, {astType}, {*addArithConf, *diffConf});
    builder.createApply(ai->getLoc(), fnRef, subMap,
                        {subscriptBuffer, intStruct, adjointArray});
    return subscriptBuffer;
  }

  void
  accumulateDifferentiableViewSubscriptDirect(ApplyInst *ai, SILType elType,
                                              StoreInst *si,
                                              AllocStackInst *subscriptBuffer) {
    auto astType = elType.getASTType();
    auto newAdjValue = builder.createLoad(ai->getLoc(), subscriptBuffer,
                                          getBufferLOQ(astType, getPullback()));
    addAdjointValue(si->getParent(), si->getSrc(),
                    makeConcreteAdjointValue(ValueWithCleanup(
                        newAdjValue, makeCleanup(newAdjValue, emitCleanup))));
    builder.createDeallocStack(ai->getLoc(), subscriptBuffer);
  }

  void accumulateDifferentiableViewSubscriptIndirect(
      ApplyInst *ai, CopyAddrInst *cai, AllocStackInst *subscriptBuffer) {
    auto subscriptBufferAccess = builder.createBeginAccess(
        ai->getLoc(), subscriptBuffer, SILAccessKind::Read,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    addToAdjointBuffer(cai->getParent(), cai->getSrc(), subscriptBufferAccess);
    builder.createEndAccess(ai->getLoc(), subscriptBufferAccess,
                            /*aborted*/ false);
    builder.createDeallocStack(ai->getLoc(), subscriptBuffer);
  }

  void visitArrayInitialization(ApplyInst *ai) {
    SILValue adjointArray;
    SILValue fnRef;
    CanGenericSignature genericSig;
    for (auto use : ai->getUses()) {
      auto tei = dyn_cast<TupleExtractInst>(use->getUser()->getResult(0));
      if (!tei || tei->getFieldNo() != 0) continue;
      // The first tuple field of the return value is the `Array`.
      adjointArray = getAdjointValue(
          ai->getParent(), tei).getConcreteValue().getValue();
      assert(adjointArray && "Array does not have adjoint value");
      auto astType = adjointArray->getType().getASTType();
      auto typeDecl = astType->getStructOrBoundGenericStruct();
      auto subscriptDecl = cast<SubscriptDecl>(typeDecl->lookupDirect(
          DeclBaseName::createSubscript()).front());
      auto subscriptGet = subscriptDecl->getAccessor(AccessorKind::Get);
      SILDeclRef subscriptRef(subscriptGet, SILDeclRef::Kind::Func);
      auto fnBuilder = SILOptFunctionBuilder(getContext().getTransform());
      auto fn = fnBuilder.getOrCreateFunction(
          ai->getLoc(), subscriptRef, NotForDefinition);
      genericSig = fn->getLoweredFunctionType()->getGenericSignature();
      assert(genericSig && "No generic signature");
      fnRef = builder.createFunctionRef(ai->getLoc(), fn);
      assert(fnRef && "cannot create function_ref");
    }
    // Two loops because the tuple_extract instructions can be reached in
    // either order.
    for (auto use : ai->getUses()) {
      auto tei = dyn_cast<TupleExtractInst>(use->getUser()->getResult(0));
      if (!tei || tei->getFieldNo() != 1) continue;
      // The second tuple field is the `RawPointer`.
      for (auto use : tei->getUses()) {
        // The `RawPointer` passes through a `pointer_to_address`. That
        // instruction's first use is a `store` whose src is useful; its
        // subsequent uses are `index_addr`s whose only use is a useful
        // `store`. In the indirect case, each `store` is instead a
        // `copy_addr`.
        for (auto use : use->getUser()->getResult(0)->getUses()) {
          auto inst = use->getUser();
          if (auto si = dyn_cast<StoreInst>(inst)) {
            auto tanType = getRemappedTangentType(si->getSrc()->getType());
            auto subscriptBuffer = emitDifferentiableViewSubscript(
                ai, tanType, adjointArray, fnRef, genericSig, 0);
            accumulateDifferentiableViewSubscriptDirect(
                ai, tanType, si, subscriptBuffer);
          } else if (auto cai = dyn_cast<CopyAddrInst>(inst)) {
            auto tanType = getRemappedTangentType(cai->getSrc()->getType());
            auto subscriptBuffer = emitDifferentiableViewSubscript(
                ai, tanType, adjointArray, fnRef, genericSig, 0);
            accumulateDifferentiableViewSubscriptIndirect(
                ai, cai, subscriptBuffer);
          } else if (auto iai = dyn_cast<IndexAddrInst>(inst)) {
            for (auto use : iai->getUses()) {
              if (auto si = dyn_cast<StoreInst>(use->getUser())) {
                auto literal = dyn_cast<IntegerLiteralInst>(iai->getIndex());
                auto tanType = getRemappedTangentType(
                    si->getSrc()->getType());
                auto subscriptBuffer = emitDifferentiableViewSubscript(
                    ai, tanType, adjointArray, fnRef,
                    genericSig, literal->getValue().getLimitedValue());
                accumulateDifferentiableViewSubscriptDirect(
                    ai, tanType, si, subscriptBuffer);
              } else if (auto cai = dyn_cast<CopyAddrInst>(use->getUser())) {
                auto literal = dyn_cast<IntegerLiteralInst>(iai->getIndex());
                auto tanType = getRemappedTangentType(
                    cai->getSrc()->getType());
                auto subscriptBuffer = emitDifferentiableViewSubscript(
                    ai, tanType, adjointArray, fnRef,
                    genericSig, literal->getValue().getLimitedValue());
                accumulateDifferentiableViewSubscriptIndirect(
                    ai, cai, subscriptBuffer);
              }
            }
          }
        }
      }
    }
  }

  void visitApplyInst(ApplyInst *ai) {
    if (ai->hasSemantics("array.uninitialized_intrinsic"))
      return visitArrayInitialization(ai);
    // Replace a call to a function with a call to its pullback.
    auto &nestedApplyInfo = getContext().getNestedApplyInfo();
    auto applyInfoLookup = nestedApplyInfo.find(ai);
    // If no `NestedApplyInfo` was found, then this task doesn't need to be
    // differentiated.
    if (applyInfoLookup == nestedApplyInfo.end()) {
      // Must not be active.
      assert(!getActivityInfo().isActive(ai, getIndices()));
      return;
    }
    auto applyInfo = applyInfoLookup->getSecond();

    // Get the pullback.
    auto *field = getPullbackInfo().lookUpPullbackDecl(ai);
    assert(field);
    auto loc = ai->getLoc();
    SILValue pullback = builder.createStructExtract(
        loc, getPullbackBlockPullbackStructArgument(ai->getParent()), field);

    // Get the original result of the `apply` instruction.
    SmallVector<SILValue, 8> args;
    SmallVector<SILValue, 8> origDirResults;
    collectAllExtractedElements(ai, origDirResults);
    SmallVector<SILValue, 8> origAllResults;
    collectAllActualResultsInTypeOrder(
        ai, origDirResults, ai->getIndirectSILResults(),
        origAllResults);
    auto origResult = origAllResults[applyInfo.indices.source];
    auto origNumIndRes = ai->getNumIndirectResults();

    auto pullbackType =
        remapType(pullback->getType()).castTo<SILFunctionType>();

    // Get the seed (i.e. adjoint value of the original result).
    ValueWithCleanup seed;
    auto *bb = ai->getParent();
    if (origResult->getType().isObject()) {
      // If original result is a `tuple_extract`, materialize adjoint value of
      // `ai` and extract the corresponding element adjoint value.
      if (auto *tupleExtract = dyn_cast<TupleExtractInst>(origResult)) {
        auto adjointTuple = materializeAdjoint(getAdjointValue(bb, ai), loc);
        auto seedVal = builder.emitTupleExtract(loc, adjointTuple,
                                                tupleExtract->getFieldNo());
        seed = ValueWithCleanup(seedVal, makeCleanup(seedVal, emitCleanup));
      }
      // Otherwise, materialize adjoint value of `ai`.
      else {
        seed = materializeAdjoint(getAdjointValue(bb, origResult), loc);
      }
    } else {
      seed = getAdjointBuffer(bb, origResult);
      if (errorOccurred)
        return;
    }

    // Create allocations for pullback indirect results.
    SmallVector<AllocStackInst *, 4> pullbackIndirectResults;
    auto actualPullbackType = applyInfo.originalPullbackType
        ? *applyInfo.originalPullbackType
        : pullbackType;
    for (auto indRes : actualPullbackType->getIndirectFormalResults()) {
      auto *alloc =
          builder.createAllocStack(loc, remapType(indRes.getSILStorageType()));
      pullbackIndirectResults.push_back(alloc);
      args.push_back(alloc);
    }

    // If pullback was reabstracted in VJP, reabstract pullback in adjoint.
    if (applyInfo.originalPullbackType) {
      SILOptFunctionBuilder fb(getContext().getTransform());
      auto *thunk = getOrCreateReabstractionThunk(
          fb, getContext().getModule(), loc, &getPullback(),
          pullbackType, *applyInfo.originalPullbackType);
      auto *thunkRef = builder.createFunctionRef(loc, thunk);
      pullback = builder.createPartialApply(
          loc, thunkRef,
          remapSubstitutionMap(thunk->getForwardingSubstitutionMap()),
          {pullback}, pullbackType->getCalleeConvention());
    }
    args.push_back(seed);

    // Call the pullback.
    auto *pullbackCall = builder.createApply(
        loc, pullback, SubstitutionMap(), args, /*isNonThrowing*/ false);

    // Extract all results from `pullbackCall`.
    SmallVector<SILValue, 8> dirResults;
    extractAllElements(pullbackCall, builder, dirResults);
    // Get all results in type-defined order.
    SmallVector<SILValue, 8> allResults;
    collectAllActualResultsInTypeOrder(
        pullbackCall, dirResults, pullbackCall->getIndirectSILResults(),
        allResults);
    LLVM_DEBUG({
      auto &s = getADDebugStream();
      s << "All direct results of the nested pullback call:\n";
      llvm::for_each(dirResults, [&](SILValue v) { s << v; });
      s << "All indirect results of the nested pullback call:\n";
      llvm::for_each(pullbackCall->getIndirectSILResults(),
                     [&](SILValue v) { s << v; });
      s << "All results of the nested pullback call:\n";
      llvm::for_each(allResults, [&](SILValue v) { s << v; });
    });

    // Accumulate adjoints for original differentiation parameters.
    auto allResultsIt = allResults.begin();
    for (unsigned i : applyInfo.indices.parameters->getIndices()) {
      auto origArg = ai->getArgument(origNumIndRes + i);
      auto tan = *allResultsIt++;
      if (tan->getType().isAddress()) {
        addToAdjointBuffer(bb, origArg, tan);
        emitCleanup(builder, loc, tan);
      } else {
        if (origArg->getType().isAddress()) {
          auto adjBuf = getAdjointBuffer(bb, origArg);
          if (errorOccurred)
            return;
          auto *tmpBuf = builder.createAllocStack(loc, tan->getType());
          builder.createStore(loc, tan, tmpBuf,
              getBufferSOQ(tmpBuf->getType().getASTType(), getPullback()));
          auto *readAccess = builder.createBeginAccess(
              loc, tmpBuf, SILAccessKind::Read, SILAccessEnforcement::Static,
              /*noNestedConflict*/ true, /*fromBuiltin*/ false);
          accumulateIndirect(adjBuf, readAccess);
          builder.createEndAccess(loc, readAccess, /*aborted*/ false);
          emitCleanup(builder, loc, tmpBuf);
          builder.createDeallocStack(loc, tmpBuf);
        }
        else
          addAdjointValue(bb, origArg, makeConcreteAdjointValue(
              ValueWithCleanup(tan,
                  makeCleanup(tan, emitCleanup, {seed.getCleanup()}))));
      }
    }
    // Deallocate pullback indirect results.
    for (auto *alloc : reversed(pullbackIndirectResults))
      builder.createDeallocStack(loc, alloc);
  }

  /// Handle `struct` instruction.
  ///   Original: y = struct (x0, x1, x2, ...)
  ///    Adjoint: adj[x0] += struct_extract adj[y], #x0
  ///             adj[x1] += struct_extract adj[y], #x1
  ///             adj[x2] += struct_extract adj[y], #x2
  ///             ...
  void visitStructInst(StructInst *si) {
    auto *bb = si->getParent();
    auto loc = si->getLoc();
    auto *structDecl = si->getStructDecl();
    auto av = getAdjointValue(bb, si);
    switch (av.getKind()) {
    case AdjointValueKind::Zero:
      for (auto *field : structDecl->getStoredProperties()) {
        auto fv = si->getFieldValue(field);
        addAdjointValue(bb, fv, makeZeroAdjointValue(
            getRemappedTangentType(fv->getType())));
      }
      break;
    case AdjointValueKind::Concrete: {
      auto adjStruct = materializeAdjointDirect(std::move(av), loc);
      // Find the struct `TangentVector` type.
      auto structTy = remapType(si->getType()).getASTType();
      auto tangentVectorTy =
          getTangentSpace(structTy)->getType()->getCanonicalType();
      assert(!getModule().Types.getTypeLowering(
                 tangentVectorTy, ResilienceExpansion::Minimal)
                     .isAddressOnly());
      auto *tangentVectorDecl =
          tangentVectorTy->getStructOrBoundGenericStruct();
      assert(tangentVectorDecl);

      // Accumulate adjoints for the fields of the `struct` operand.
      for (auto *field : structDecl->getStoredProperties()) {
        // There does not exist a corresponding tangent field for original
        // fields with `@noDerivative` attribute. Emit an error.
        if (field->getAttrs().hasAttribute<NoDerivativeAttr>())
          continue;
        // Find the corresponding field in the tangent space.
        VarDecl *tanField = nullptr;
        if (tangentVectorDecl == structDecl)
          tanField = field;
        // Otherwise, look up the field by name.
        else {
          auto tanFieldLookup =
              tangentVectorDecl->lookupDirect(field->getName());
          if (tanFieldLookup.empty()) {
            getContext().emitNondifferentiabilityError(
                si, getInvoker(),
                diag::autodiff_stored_property_no_corresponding_tangent,
                tangentVectorDecl->getNameStr(), field->getNameStr());
            errorOccurred = true;
            return;
          }
          tanField = cast<VarDecl>(tanFieldLookup.front());
        }
        auto *adjStructElt =
            builder.createStructExtract(loc, adjStruct, tanField);
        addAdjointValue(
            bb, si->getFieldValue(field),
            makeConcreteAdjointValue(ValueWithCleanup(
                adjStructElt, makeCleanup(adjStructElt, emitCleanup))));
      }
      break;
    }
    case AdjointValueKind::Aggregate: {
      // Note: All user-called initializations go through the calls to the
      // initializer, and synthesized initializers only have one level of struct
      // formation which will not result into any aggregate adjoint valeus.
      llvm_unreachable("Aggregate adjoint values should not occur for `struct` "
                       "instructions");
    }
    }
  }

  /// Handle `struct_extract` instruction.
  ///   Original: y = struct_extract x, #field
  ///    Adjoint: adj[x] += struct (0, ..., #field': adj[y], ..., 0)
  ///                                       ^~~~~~~
  ///                     field in tangent space corresponding to #field
  void visitStructExtractInst(StructExtractInst *sei) {
    assert(!sei->getField()->getAttrs().hasAttribute<NoDerivativeAttr>() &&
           "`struct_extract` with `@noDerivative` field should not be "
           "differentiated; activity analysis should not marked as varied");
    auto *bb = sei->getParent();
    auto structTy = remapType(sei->getOperand()->getType()).getASTType();
    auto tangentVectorTy =
        getTangentSpace(structTy)->getType()->getCanonicalType();
    assert(!getModule().Types.getTypeLowering(
               tangentVectorTy, ResilienceExpansion::Minimal)
                   .isAddressOnly());
    auto tangentVectorSILTy =
        SILType::getPrimitiveObjectType(tangentVectorTy);
    auto *tangentVectorDecl =
        tangentVectorTy->getStructOrBoundGenericStruct();
    assert(tangentVectorDecl);
    // Find the corresponding field in the tangent space.
    VarDecl *tanField = nullptr;
    // If the tangent space is the original struct, then field is the same.
    if (tangentVectorDecl == sei->getStructDecl())
      tanField = sei->getField();
    // Otherwise, look up the field by name.
    else {
      auto tanFieldLookup =
          tangentVectorDecl->lookupDirect(sei->getField()->getName());
      if (tanFieldLookup.empty()) {
        getContext().emitNondifferentiabilityError(
            sei, getInvoker(),
            diag::autodiff_stored_property_no_corresponding_tangent,
            sei->getStructDecl()->getNameStr(),
            sei->getField()->getNameStr());
        errorOccurred = true;
        return;
      }
      tanField = cast<VarDecl>(tanFieldLookup.front());
    }
    // Accumulate adjoint for the `struct_extract` operand.
    auto av = getAdjointValue(bb, sei);
    switch (av.getKind()) {
    case AdjointValueKind::Zero:
      addAdjointValue(bb, sei->getOperand(),
                      makeZeroAdjointValue(tangentVectorSILTy));
      break;
    case AdjointValueKind::Concrete:
    case AdjointValueKind::Aggregate: {
      SmallVector<AdjointValue, 8> eltVals;
      for (auto *field : tangentVectorDecl->getStoredProperties()) {
        if (field == tanField) {
          eltVals.push_back(av);
        } else {
          auto substMap = tangentVectorTy->getMemberSubstitutionMap(
              field->getModuleContext(), field);
          auto fieldTy = field->getType().subst(substMap);
          auto fieldSILTy =
              getContext().getTypeConverter().getLoweredType(
                  fieldTy, ResilienceExpansion::Minimal);
          assert(fieldSILTy.isObject());
          eltVals.push_back(makeZeroAdjointValue(fieldSILTy));
        }
      }
      addAdjointValue(bb, sei->getOperand(),
          makeAggregateAdjointValue(tangentVectorSILTy, eltVals));
    }
    }
  }

  /// Handle `tuple` instruction.
  ///   Original: y = tuple (x0, x1, x2, ...)
  ///    Adjoint: adj[x0] += tuple_extract adj[y], 0
  ///             ...
  void visitTupleInst(TupleInst *ti) {
    auto *bb = ti->getParent();
    auto av = getAdjointValue(bb, ti);
    switch (av.getKind()) {
    case AdjointValueKind::Zero:
      for (auto eltVal : ti->getElements()) {
        if (!getTangentSpace(eltVal->getType().getASTType()))
          continue;
        addAdjointValue(bb, eltVal, makeZeroAdjointValue(
            getRemappedTangentType(eltVal->getType())));
      }
      break;
    case AdjointValueKind::Concrete: {
      auto val = av.getConcreteValue();
      unsigned adjIdx = 0;
      for (auto i : range(ti->getNumOperands())) {
        if (!getTangentSpace(ti->getOperand(i)->getType().getASTType()))
          continue;
        auto adjElt = val;
        if (val.getType().is<TupleType>())
          adjElt = ValueWithCleanup(builder.createTupleExtract(
              ti->getLoc(), val, adjIdx++), val.getCleanup());
        addAdjointValue(bb, ti->getOperand(i), makeConcreteAdjointValue(adjElt));
      }
      break;
    }
    case AdjointValueKind::Aggregate:
      unsigned adjIdx = 0;
      for (auto i : range(ti->getElements().size())) {
        if (!getTangentSpace(ti->getElement(i)->getType().getASTType()))
          continue;
        addAdjointValue(bb, ti->getElement(i), av.getAggregateElement(adjIdx++));
      }
      break;
    }
  }

  /// Handle `tuple_extract` instruction.
  ///   Original: y = tuple_extract x, <n>
  ///    Adjoint: adj[x] += tuple (0, 0, ..., adj[y], ..., 0, 0)
  ///                                         ^~~~~~
  ///                            n'-th element, where n' is tuple tangent space
  ///                            index corresponding to n
  void visitTupleExtractInst(TupleExtractInst *tei) {
    auto *bb = tei->getParent();
    auto tupleTanTy = getRemappedTangentType(tei->getOperand()->getType());
    auto av = getAdjointValue(bb, tei);
    switch (av.getKind()) {
    case AdjointValueKind::Zero:
      addAdjointValue(bb, tei->getOperand(), makeZeroAdjointValue(tupleTanTy));
      break;
    case AdjointValueKind::Aggregate:
    case AdjointValueKind::Concrete: {
      auto tupleTy = tei->getTupleType();
      auto tupleTanTupleTy = tupleTanTy.getAs<TupleType>();
      if (!tupleTanTupleTy) {
        addAdjointValue(bb, tei->getOperand(), av);
        break;
      }
      SmallVector<AdjointValue, 8> elements;
      unsigned adjIdx = 0;
      for (unsigned i : range(tupleTy->getNumElements())) {
        if (!getTangentSpace(
                tupleTy->getElement(i).getType()->getCanonicalType()))
          continue;
        if (tei->getFieldNo() == i)
          elements.push_back(av);
        else
          elements.push_back(makeZeroAdjointValue(
              getRemappedTangentType(SILType::getPrimitiveObjectType(
                  tupleTanTupleTy->getElementType(adjIdx++)
                      ->getCanonicalType()))));
      }
      if (elements.size() == 1) {
        addAdjointValue(bb, tei->getOperand(), elements.front());
        break;
      }
      addAdjointValue(bb, tei->getOperand(),
          makeAggregateAdjointValue(tupleTanTy, elements));
      break;
    }
    }
  }

  /// Handle `load` instruction.
  ///   Original: y = load x
  ///    Adjoint: adj[x] += adj[y]
  void visitLoadInst(LoadInst *li) {
    auto *bb = li->getParent();
    auto adjVal = materializeAdjointDirect(getAdjointValue(bb, li), li->getLoc());
    // Allocate a local buffer and store the adjoint value. This buffer will be
    // used for accumulation into the adjoint buffer.
    auto *localBuf = builder.createAllocStack(li->getLoc(), adjVal.getType());
    auto *initAccess = builder.createBeginAccess(
        li->getLoc(), localBuf, SILAccessKind::Init,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    builder.createStore(li->getLoc(), adjVal, initAccess,
        getBufferSOQ(localBuf->getType().getASTType(), getPullback()));
    builder.createEndAccess(li->getLoc(), initAccess, /*aborted*/ false);
    // Get the adjoint buffer.
    auto &adjBuf = getAdjointBuffer(bb, li->getOperand());
    if (errorOccurred)
      return;
    // Accumulate the adjoint value in the local buffer into the adjoint buffer.
    auto *readAccess = builder.createBeginAccess(
        li->getLoc(), localBuf, SILAccessKind::Read,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    accumulateIndirect(adjBuf, readAccess);
    // Combine the adjoint buffer's original child cleanups with the adjoint
    // value's cleanup.
    adjBuf.setCleanup(makeCleanupFromChildren({adjBuf.getCleanup(),
                                               adjVal.getCleanup()}));
    builder.createEndAccess(li->getLoc(), readAccess, /*aborted*/ false);
    builder.createDeallocStack(li->getLoc(), localBuf);
  }

  /// Handle `store` instruction.
  ///   Original: store x to y
  ///    Adjoint: adj[x] += load adj[y]; adj[y] = 0
  void visitStoreInst(StoreInst *si) {
    auto *bb = si->getParent();
    auto &adjBuf = getAdjointBuffer(bb, si->getDest());
    if (errorOccurred)
      return;
    auto bufType = remapType(adjBuf.getType());
    auto adjVal = builder.createLoad(si->getLoc(), adjBuf,
        getBufferLOQ(bufType.getASTType(), getPullback()));
    // Disable the buffer's top-level cleanup (which is supposed to operate on
    // the buffer), create a cleanup for the value that carrys all child
    // cleanups.
    auto valueCleanup = makeCleanup(adjVal, emitCleanup,
        adjBuf.getCleanup()
            ? adjBuf.getCleanup()->getChildren() : ArrayRef<Cleanup *>());
    addAdjointValue(bb, si->getSrc(), makeConcreteAdjointValue(
        ValueWithCleanup(adjVal, valueCleanup)));
    // Set the buffer to zero, with a cleanup.
    emitZeroIndirect(bufType.getASTType(), adjBuf, si->getLoc());
  }

  /// Handle `copy_addr` instruction.
  ///   Original: copy_addr x to y
  ///    Adjoint: adj[x] += adj[y]; adj[y] = 0
  void visitCopyAddrInst(CopyAddrInst *cai) {
    auto *bb = cai->getParent();
    auto &adjDest = getAdjointBuffer(bb, cai->getDest());
    if (errorOccurred)
      return;
    auto destType = remapType(adjDest.getType());
    // Disable the buffer's top-level cleanup (which is supposed to operate on
    // the buffer), create a cleanup for the value that carrys all child
    // cleanups.
    auto valueCleanup = makeCleanup(adjDest, emitCleanup,
        adjDest.getCleanup()
            ? adjDest.getCleanup()->getChildren() : ArrayRef<Cleanup *>());
    adjDest.setCleanup(valueCleanup);
    auto *readAccess = builder.createBeginAccess(
        cai->getLoc(), adjDest, SILAccessKind::Read,
        SILAccessEnforcement::Static, /*noNestedConflict*/ true,
        /*fromBuiltin*/ false);
    addToAdjointBuffer(bb, cai->getSrc(), readAccess);
    builder.createEndAccess(cai->getLoc(), readAccess, /*aborted*/ false);
    // Set the buffer to zero, with a cleanup.
    emitZeroIndirect(destType.getASTType(), adjDest, cai->getLoc());
    auto cleanup = makeCleanup(adjDest, emitCleanup);
    adjDest.setCleanup(cleanup);
  }

  /// Handle `begin_access` instruction.
  ///   Original: y = begin_access x
  ///    Adjoint: nothing (differentiability checks, cleanup propagation)
  void visitBeginAccessInst(BeginAccessInst *bai) {
    // Check for non-differentiable writes.
    if (bai->getAccessKind() == SILAccessKind::Modify) {
      if (auto *gai = dyn_cast<GlobalAddrInst>(bai->getSource())) {
        getContext().emitNondifferentiabilityError(bai, getInvoker(),
            diag::autodiff_cannot_differentiate_writes_to_global_variables);
        errorOccurred = true;
        return;
      }
      if (auto *pbi = dyn_cast<ProjectBoxInst>(bai->getSource())) {
        getContext().emitNondifferentiabilityError(bai, getInvoker(),
            diag::autodiff_cannot_differentiate_writes_to_mutable_captures);
        errorOccurred = true;
        return;
      }
    }
    auto *bb = bai->getParent();
    auto accessBuf = getAdjointBuffer(bb, bai);
    auto &sourceBuf = getAdjointBuffer(bb, bai->getSource());
    sourceBuf.setCleanup(makeCleanupFromChildren({sourceBuf.getCleanup(),
                                                  accessBuf.getCleanup()}));
  }

#define PROPAGATE_BUFFER_CLEANUP(INST) \
  void visit##INST##Inst(INST##Inst *inst) { \
    auto *bb = inst->getParent(); \
    auto &adjBase = getAdjointBuffer(bb, inst->getOperand()); \
    auto &adjProj = getAdjointBuffer(bb, inst); \
    adjProj.setCleanup(makeCleanupFromChildren( \
        {adjProj.getCleanup(), adjBase.getCleanup()})); \
  }
  PROPAGATE_BUFFER_CLEANUP(StructElementAddr)
  PROPAGATE_BUFFER_CLEANUP(TupleElementAddr)
#undef PROPAGATE_CLEANUP

#define NOT_DIFFERENTIABLE(INST, DIAG) \
  void visit##INST##Inst(INST##Inst *inst) { \
    getContext().emitNondifferentiabilityError( \
        inst, getInvoker(), DIAG); \
    errorOccurred = true; \
    return; \
  }
#undef NOT_DIFFERENTIABLE

#define NO_ADJOINT(INST) \
  void visit##INST##Inst(INST##Inst *inst) {}
  // Terminators.
  NO_ADJOINT(Return)
  NO_ADJOINT(Branch)
  NO_ADJOINT(CondBranch)

  // Memory allocation/access.
  NO_ADJOINT(AllocStack)
  NO_ADJOINT(DeallocStack)
  NO_ADJOINT(EndAccess)

  // Debugging/reference counting instructions.
  NO_ADJOINT(DebugValue)
  NO_ADJOINT(DebugValueAddr)
  NO_ADJOINT(RetainValue)
  NO_ADJOINT(RetainValueAddr)
  NO_ADJOINT(ReleaseValue)
  NO_ADJOINT(ReleaseValueAddr)
  NO_ADJOINT(StrongRetain)
  NO_ADJOINT(StrongRelease)
  NO_ADJOINT(UnownedRetain)
  NO_ADJOINT(UnownedRelease)
  NO_ADJOINT(StrongRetainUnowned)
  NO_ADJOINT(DestroyValue)
  NO_ADJOINT(DestroyAddr)
#undef NO_DERIVATIVE
};
} // end anonymous namespace

Cleanup *PullbackEmitter::makeCleanup(SILValue value, Cleanup::Func func,
                                      ArrayRef<Cleanup *> children) {
  SmallVector<Cleanup *, 2> nonnullChildren;
  for (auto *c : children)
    if (c) nonnullChildren.push_back(c);
  return Cleanup::create(allocator, value, func, nonnullChildren);
}

Cleanup *
PullbackEmitter::makeCleanupFromChildren(ArrayRef<Cleanup *> children) {
  if (children.empty())
    return nullptr;
  if (children.size() == 1)
    return children.front();
  SmallSetVector<Cleanup *, 8> uniqued(children.begin(), children.end());
  return makeCleanup(SILValue(), /*func*/ nullptr, uniqued.getArrayRef());
}

AdjointValue PullbackEmitter::makeZeroAdjointValue(SILType type) {
  return AdjointValue::createZero(allocator, remapType(type));
}

AdjointValue
PullbackEmitter::makeConcreteAdjointValue(ValueWithCleanup value) {
  return AdjointValue::createConcrete(allocator, value);
}

template<typename EltRange>
AdjointValue PullbackEmitter::makeAggregateAdjointValue(
    SILType type, EltRange elements) {
  return AdjointValue::createAggregate(allocator, remapType(type), elements);
}

ValueWithCleanup PullbackEmitter::materializeAdjointDirect(
    AdjointValue val, SILLocation loc) {
  assert(val.getType().isObject());
  LLVM_DEBUG(getADDebugStream() <<
             "Materializing adjoints for " << val << '\n');
  switch (val.getKind()) {
  case AdjointValueKind::Zero: {
    auto zeroVal = emitZeroDirect(val.getSwiftType(), loc);
    return ValueWithCleanup(zeroVal, nullptr);
  }
  case AdjointValueKind::Aggregate: {
    SmallVector<SILValue, 8> elements;
    SmallVector<Cleanup *, 8> cleanups;
    for (auto i : range(val.getNumAggregateElements())) {
      auto eltVal = materializeAdjointDirect(val.getAggregateElement(i), loc);
      elements.push_back(eltVal.getValue());
      cleanups.push_back(eltVal.getCleanup());
    }
    if (val.getType().is<TupleType>())
      return ValueWithCleanup(
          builder.createTuple(loc, val.getType(), elements),
                              makeCleanupFromChildren(cleanups));
    else {
      auto *adj = builder.createStruct(loc, val.getType(), elements);
      builder.createRetainValue(loc, adj, builder.getDefaultAtomicity());
      auto cleanupFn = [](SILBuilder &b, SILLocation l, SILValue v) {
        b.createReleaseValue(l, v, b.getDefaultAtomicity());
      };
      return ValueWithCleanup(adj, makeCleanup(adj, cleanupFn, cleanups));
    }
  }
  case AdjointValueKind::Concrete:
    return val.getConcreteValue();
  }
}

void PullbackEmitter::materializeAdjointIndirect(
    AdjointValue val, ValueWithCleanup &destBuffer) {
  ValueWithCleanup access(
      builder.createBeginAccess(
          destBuffer.getLoc(), destBuffer, SILAccessKind::Init,
          SILAccessEnforcement::Static, /*noNestedConflict*/ true,
          /*fromBuiltin*/ false),
          /*cleanup*/ nullptr);
  materializeAdjointIndirectHelper(val, access);
  destBuffer.setCleanup(access.getCleanup());
  builder.createEndAccess(access.getLoc(), access, /*aborted*/ false);
}

ValueWithCleanup PullbackEmitter::materializeAdjoint(AdjointValue val,
                                                     SILLocation loc) {
  if (val.isConcrete()) {
    LLVM_DEBUG(getADDebugStream()
        << "Materializing adjoint: Value is concrete.\n");
    return val.getConcreteValue();
  }
  LLVM_DEBUG(getADDebugStream() << "Materializing adjoint: Value is "
                                   "non-concrete. Materializing directly.\n");
  return materializeAdjointDirect(val, loc);
}

void PullbackEmitter::materializeAdjointIndirectHelper(
    AdjointValue val, ValueWithCleanup &destBufferAccess) {
  auto loc = destBufferAccess.getLoc();
  auto soq = getBufferSOQ(val.getType().getASTType(), builder.getFunction());
  switch (val.getKind()) {
  /// Given a `%buf : *T, emit instructions that produce a zero or an aggregate
  /// of zeros of the expected type. When `T` conforms to
  /// `AdditiveArithmetic`, we emit a call to `AdditiveArithmetic.zero`. When
  /// `T` is a builtin float, we emit a `float_literal` instruction.
  /// Otherwise, we assert that `T` must be an aggregate where each element
  /// conforms to `AdditiveArithmetic` or is a builtin float. We expect to emit
  /// a zero for each element and use the appropriate aggregate constructor
  /// instruction (in this case, `tuple`) to produce a tuple. But currently,
  /// since we need indirect passing for aggregate instruction, we just use
  /// `tuple_element_addr` to get element buffers and write elements to them.
  case AdjointValueKind::Zero:
    emitZeroIndirect(val.getSwiftType(), destBufferAccess, loc);
    break;
  /// Given a `%buf : *(T0, T1, T2, ...)` or `%buf : *Struct` recursively emit
  /// instructions to materialize the symbolic tuple or struct, filling the
  /// buffer.
  case AdjointValueKind::Aggregate: {
    if (auto *tupTy = val.getSwiftType()->getAs<TupleType>()) {
      for (auto idx : range(val.getNumAggregateElements())) {
        auto eltTy = SILType::getPrimitiveAddressType(
            tupTy->getElementType(idx)->getCanonicalType());
        ValueWithCleanup eltBuf(
            builder.createTupleElementAddr(loc, destBufferAccess, idx, eltTy),
            /*cleanup*/ nullptr);
        materializeAdjointIndirectHelper(val.getAggregateElement(idx), eltBuf);
        destBufferAccess.setCleanup(makeCleanupFromChildren(
            {destBufferAccess.getCleanup(), eltBuf.getCleanup()}));
      }
    } else if (auto *structDecl =
                   val.getSwiftType()->getStructOrBoundGenericStruct()) {
      auto fieldIt = structDecl->getStoredProperties().begin();
      for (unsigned i = 0; fieldIt != structDecl->getStoredProperties().end();
           ++fieldIt, ++i) {
        ValueWithCleanup eltBuf(
            builder.createStructElementAddr(loc, destBufferAccess, *fieldIt),
            /*cleanup*/ nullptr);
        materializeAdjointIndirectHelper(val.getAggregateElement(i), eltBuf);
        destBufferAccess.setCleanup(makeCleanupFromChildren(
            {destBufferAccess.getCleanup(), eltBuf.getCleanup()}));
      }
    } else {
      llvm_unreachable("Not an aggregate type");
    }
    break;
  }
  /// Value is already materialized!
  case AdjointValueKind::Concrete:
    auto concreteVal = val.getConcreteValue();
    builder.createStore(loc, concreteVal, destBufferAccess, soq);
    destBufferAccess.setCleanup(makeCleanupFromChildren(
        {destBufferAccess.getCleanup(), concreteVal.getCleanup()}));
    break;
  }
}

void PullbackEmitter::emitZeroIndirect(CanType type, SILValue bufferAccess,
                                       SILLocation loc) {
  auto tangentSpace = getTangentSpace(type);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector:
    emitZeroIntoBuffer(builder, type, bufferAccess, loc);
    return;
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    SmallVector<SILValue, 8> zeroElements;
    for (unsigned i : range(tupleType->getNumElements())) {
      auto eltAddr = builder.createTupleElementAddr(loc, bufferAccess, i);
      emitZeroIndirect(tupleType->getElementType(i)->getCanonicalType(),
                       eltAddr, loc);
    }
    return;
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
      "Unimplemented: Emit thunks for abstracting zero initialization");
  }
  }
}

SILValue PullbackEmitter::emitZeroDirect(CanType type, SILLocation loc) {
  auto silType = getModule().Types.getLoweredLoadableType(
      type, ResilienceExpansion::Minimal);
  auto *buffer = builder.createAllocStack(loc, silType);
  auto *initAccess = builder.createBeginAccess(loc, buffer, SILAccessKind::Init,
                                               SILAccessEnforcement::Static,
                                               /*noNestedConflict*/ true,
                                               /*fromBuiltin*/ false);
  emitZeroIndirect(type, initAccess, loc);
  builder.createEndAccess(loc, initAccess, /*aborted*/ false);
  auto readAccess = builder.createBeginAccess(loc, buffer, SILAccessKind::Read,
                                              SILAccessEnforcement::Static,
                                              /*noNestedConflict*/ true,
                                              /*fromBuiltin*/ false);
  auto *loaded = builder.createLoad(loc, readAccess,
                                    getBufferLOQ(type, getPullback()));
  builder.createEndAccess(loc, readAccess, /*aborted*/ false);
  builder.createDeallocStack(loc, buffer);
  return loaded;
}

void PullbackEmitter::emitCleanupForAdjointValue(AdjointValue value) {
  switch (value.getKind()) {
  case AdjointValueKind::Zero: return;
  case AdjointValueKind::Aggregate:
    for (auto element : value.getAggregateElements())
      emitCleanupForAdjointValue(element);
    break;
  case AdjointValueKind::Concrete: {
    auto concrete = value.getConcreteValue();
    auto *cleanup = concrete.getCleanup();
    LLVM_DEBUG(getADDebugStream() << "Applying "
               << cleanup->getNumChildren() << " for value "
               << concrete.getValue() << " child cleanups\n");
    cleanup->applyRecursively(builder, concrete.getLoc());
    break;
  }
  }
}

AdjointValue
PullbackEmitter::accumulateAdjointsDirect(AdjointValue lhs,
                                          AdjointValue rhs) {
  LLVM_DEBUG(getADDebugStream()
             << "Materializing adjoint directly.\nLHS: " << lhs
             << "\nRHS: " << rhs << '\n');

  switch (lhs.getKind()) {
  // x
  case AdjointValueKind::Concrete: {
    auto lhsVal = lhs.getConcreteValue();
    switch (rhs.getKind()) {
    // x + y
    case AdjointValueKind::Concrete: {
      auto rhsVal = rhs.getConcreteValue();
      auto sum = accumulateDirect(lhsVal, rhsVal);
      return makeConcreteAdjointValue(ValueWithCleanup(
          sum, makeCleanup(sum, emitCleanup, {lhsVal.getCleanup(),
                                              rhsVal.getCleanup()})));
    }
    // x + 0 => x
    case AdjointValueKind::Zero:
      return lhs;
    // x + (y, z) => (x.0 + y, x.1 + z)
    case AdjointValueKind::Aggregate:
      SmallVector<AdjointValue, 8> newElements;
      auto lhsTy = lhsVal.getValue()->getType().getASTType();
      if (auto *tupTy = lhsTy->getAs<TupleType>()) {
        for (auto idx : range(rhs.getNumAggregateElements())) {
          auto lhsElt = builder.createTupleExtract(
              lhsVal.getLoc(), lhsVal, idx);
          auto rhsElt = rhs.getAggregateElement(idx);
          newElements.push_back(accumulateAdjointsDirect(
              makeConcreteAdjointValue(
                  ValueWithCleanup(lhsElt, lhsVal.getCleanup())),
              rhsElt));
        }
      } else if (auto *structDecl = lhsTy->getStructOrBoundGenericStruct()) {
        auto fieldIt = structDecl->getStoredProperties().begin();
        for (unsigned i = 0; fieldIt != structDecl->getStoredProperties().end();
             ++fieldIt, ++i) {
          auto lhsElt = builder.createStructExtract(
              lhsVal.getLoc(), lhsVal, *fieldIt);
          auto rhsElt = rhs.getAggregateElement(i);
          newElements.push_back(accumulateAdjointsDirect(
              makeConcreteAdjointValue(
                  ValueWithCleanup(lhsElt, lhsVal.getCleanup())),
              rhsElt));
        }
      } else {
        llvm_unreachable("Not an aggregate type");
      }
      return makeAggregateAdjointValue(lhsVal.getType(), newElements);
    }
  }
  // 0
  case AdjointValueKind::Zero:
    // 0 + x => x
    return rhs;
  // (x, y)
  case AdjointValueKind::Aggregate:
    switch (rhs.getKind()) {
    // (x, y) + z => (x + z.0, y + z.1)
    case AdjointValueKind::Concrete:
    // x + 0 => x
    case AdjointValueKind::Zero:
      return lhs;
    // (x, y) + (z, w) => (x + z, y + w)
    case AdjointValueKind::Aggregate: {
      SmallVector<AdjointValue, 8> newElements;
      for (auto i : range(lhs.getNumAggregateElements()))
        newElements.push_back(
            accumulateAdjointsDirect(lhs.getAggregateElement(i),
                                     rhs.getAggregateElement(i)));
      return makeAggregateAdjointValue(lhs.getType(), newElements);
    }
    }
  }
}

SILValue PullbackEmitter::accumulateDirect(SILValue lhs, SILValue rhs) {
  // TODO: Optimize for the case when lhs == rhs.
  LLVM_DEBUG(getADDebugStream() <<
             "Emitting adjoint accumulation for lhs: " << lhs <<
             " and rhs: " << rhs << "\n");
  assert(lhs->getType() == rhs->getType() && "Adjoints must have equal types!");
  assert(lhs->getType().isObject() && rhs->getType().isObject() &&
         "Adjoint types must be both object types!");
  auto adjointTy = lhs->getType();
  auto adjointASTTy = adjointTy.getASTType();
  auto loc = lhs.getLoc();
  auto tangentSpace = getTangentSpace(adjointASTTy);
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector: {
    // Allocate buffers for inputs and output.
    auto *resultBuf = builder.createAllocStack(loc, adjointTy);
    auto *lhsBuf = builder.createAllocStack(loc, adjointTy);
    auto *rhsBuf = builder.createAllocStack(loc, adjointTy);
    // Initialize input buffers.
    auto *lhsBufInitAccess = builder.createBeginAccess(
        loc, lhsBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    auto *rhsBufInitAccess = builder.createBeginAccess(
        loc, rhsBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    builder.createStore(loc, lhs, lhsBufInitAccess,
                        getBufferSOQ(adjointASTTy, getPullback()));
    builder.createStore(loc, rhs, rhsBufInitAccess,
                        getBufferSOQ(adjointASTTy, getPullback()));
    builder.createEndAccess(loc, lhsBufInitAccess, /*aborted*/ false);
    builder.createEndAccess(loc, rhsBufInitAccess, /*aborted*/ false);
    // Accumulate the adjoints.
    auto *resultBufAccess = builder.createBeginAccess(
        loc, resultBuf, SILAccessKind::Init, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    auto *lhsBufReadAccess = builder.createBeginAccess(loc, lhsBuf,
        SILAccessKind::Read, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    auto *rhsBufReadAccess = builder.createBeginAccess(loc, rhsBuf,
        SILAccessKind::Read, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    accumulateIndirect(resultBufAccess, lhsBufReadAccess, rhsBufReadAccess);
    builder.createEndAccess(loc, resultBufAccess, /*aborted*/ false);
    builder.createEndAccess(loc, rhsBufReadAccess, /*aborted*/ false);
    builder.createEndAccess(loc, lhsBufReadAccess, /*aborted*/ false);
    // Deallocate input buffers.
    builder.createDeallocStack(loc, rhsBuf);
    builder.createDeallocStack(loc, lhsBuf);
    // Load result.
    resultBufAccess = builder.createBeginAccess(loc, resultBuf,
        SILAccessKind::Read, SILAccessEnforcement::Static,
        /*noNestedConflict*/ true, /*fromBuiltin*/ false);
    auto val = builder.createLoad(loc, resultBufAccess,
        getBufferLOQ(lhs->getType().getASTType(), getPullback()));
    builder.createEndAccess(loc, resultBufAccess, /*aborted*/ false);
    // Deallocate result buffer.
    builder.createDeallocStack(loc, resultBuf);
    return val;
  }
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    SmallVector<SILValue, 8> adjElements;
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *eltLHS = builder.createTupleExtract(loc, lhs, i);
      auto *eltRHS = builder.createTupleExtract(loc, rhs, i);
      adjElements.push_back(accumulateDirect(eltLHS, eltRHS));
    }
    return builder.createTuple(loc, adjointTy, adjElements);
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
      "Unimplemented: Emit thunks for abstracting adjoint accumulation");
  }
  }
}

void PullbackEmitter::accumulateIndirect(
    SILValue resultBufAccess, SILValue lhsBufAccess, SILValue rhsBufAccess) {
  // TODO: Optimize for the case when lhs == rhs.
  assert(lhsBufAccess->getType() == rhsBufAccess->getType() &&
         "Adjoint values must have same type!");
  assert(lhsBufAccess->getType().isAddress() &&
         rhsBufAccess->getType().isAddress() &&
         "Adjoint values must both have address types!");
  auto loc = resultBufAccess.getLoc();
  auto adjointTy = lhsBufAccess->getType();
  auto adjointASTTy = adjointTy.getASTType();
  auto *swiftMod = getModule().getSwiftModule();
  auto tangentSpace = adjointASTTy->getAutoDiffAssociatedTangentSpace(
      LookUpConformanceInModule(swiftMod));
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector: {
    auto *proto = getContext().getAdditiveArithmeticProtocol();
    auto *combinerFuncDecl = getContext().getPlusDecl();
    // Call the combiner function and return.
    auto adjointParentModule = tangentSpace->getNominal()
        ? tangentSpace->getNominal()->getModuleContext()
        : getModule().getSwiftModule();
    auto confRef = adjointParentModule->lookupConformance(adjointASTTy,
                                                           proto);
    assert(confRef.hasValue() && "Missing conformance to `AdditiveArithmetic`");
    SILDeclRef declRef(combinerFuncDecl, SILDeclRef::Kind::Func);
    auto silFnTy = getContext().getTypeConverter().getConstantType(declRef);
    // %0 = witness_method @+
    auto witnessMethod = builder.createWitnessMethod(loc, adjointASTTy,
                                                     *confRef, declRef,
                                                     silFnTy);
    auto subMap = SubstitutionMap::getProtocolSubstitutions(
        proto, adjointASTTy, *confRef);
    // %1 = metatype $T.Type
    auto metatypeType =
        CanMetatypeType::get(adjointASTTy, MetatypeRepresentation::Thick);
    auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
    auto metatype = builder.createMetatype(loc, metatypeSILType);
    // %2 = apply $0(%result, %new, %old, %1)
    builder.createApply(loc, witnessMethod, subMap,
                        {resultBufAccess, rhsBufAccess, lhsBufAccess, metatype},
                        /*isNonThrowing*/ false);
    return;
  }
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *destAddr = builder.createTupleElementAddr(loc, resultBufAccess, i);
      auto *eltAddrLHS = builder.createTupleElementAddr(loc, lhsBufAccess, i);
      auto *eltAddrRHS = builder.createTupleElementAddr(loc, rhsBufAccess, i);
      accumulateIndirect(destAddr, eltAddrLHS, eltAddrRHS);
    }
    return;
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
        "Unimplemented: Emit thunks for abstracting adjoint value "
        "accumulation");
  }
  }
}

void PullbackEmitter::accumulateIndirect(SILValue lhsDestAccess,
                                         SILValue rhsAccess) {
  assert(lhsDestAccess->getType().isAddress() &&
         rhsAccess->getType().isAddress());
  assert(lhsDestAccess->getFunction() == &getPullback());
  assert(rhsAccess->getFunction() == &getPullback());
  auto loc = lhsDestAccess.getLoc();
  auto type = lhsDestAccess->getType();
  auto astType = type.getASTType();
  auto *swiftMod = getModule().getSwiftModule();
  auto tangentSpace = astType->getAutoDiffAssociatedTangentSpace(
      LookUpConformanceInModule(swiftMod));
  assert(tangentSpace && "No tangent space for this type");
  switch (tangentSpace->getKind()) {
  case VectorSpace::Kind::Vector: {
    auto *proto = getContext().getAdditiveArithmeticProtocol();
    auto *accumulatorFuncDecl = getContext().getPlusEqualDecl();
    // Call the combiner function and return.
    auto confRef = swiftMod->lookupConformance(astType, proto);
    assert(confRef.hasValue() && "Missing conformance to `AdditiveArithmetic`");
    SILDeclRef declRef(accumulatorFuncDecl, SILDeclRef::Kind::Func);
    auto silFnTy = getContext().getTypeConverter().getConstantType(declRef);
    // %0 = witness_method @+=
    auto witnessMethod =
        builder.createWitnessMethod(loc, astType, *confRef, declRef, silFnTy);
    auto subMap =
        SubstitutionMap::getProtocolSubstitutions(proto, astType, *confRef);
    // %1 = metatype $T.Type
    auto metatypeType =
        CanMetatypeType::get(astType, MetatypeRepresentation::Thick);
    auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
    auto metatype = builder.createMetatype(loc, metatypeSILType);
    // %2 = apply $0(%lhs, %rhs, %1)
    builder.createApply(loc, witnessMethod, subMap,
                        {lhsDestAccess, rhsAccess, metatype},
                        /*isNonThrowing*/ false);
    return;
  }
  case VectorSpace::Kind::Tuple: {
    auto tupleType = tangentSpace->getTuple();
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *destAddr = builder.createTupleElementAddr(loc, lhsDestAccess, i);
      auto *eltAddrRHS = builder.createTupleElementAddr(loc, rhsAccess, i);
      accumulateIndirect(destAddr, eltAddrRHS);
    }
    return;
  }
  case VectorSpace::Kind::Function: {
    llvm_unreachable(
        "Unimplemented: Emit thunks for abstracting adjoint value "
        "accumulation");
  }
  }
}

bool VJPEmitter::run() {
  LLVM_DEBUG(getADDebugStream()
             << "Cloning original @" << original->getName()
             << " to vjp @" << vjp->getName() << '\n');
  // Create entry BB and arguments.
  auto *entry = vjp->createBasicBlock();
  createEntryArguments(vjp);

  // Clone.
  SmallVector<SILValue, 4> entryArgs(entry->getArguments().begin(),
                                     entry->getArguments().end());
  cloneFunctionBody(original, entry, entryArgs);
  // If errors occurred, back out.
  if (errorOccurred)
    return true;

  // Generate pullback code.
  PullbackEmitter PullbackEmitter(*this);
  if (PullbackEmitter.run()) {
    errorOccurred = true;
    return true;
  }
  LLVM_DEBUG(getADDebugStream() << "Generated VJP for "
                                << original->getName() << ":\n" << *vjp);
  return errorOccurred;
}

bool JVPEmitter::run() {
  LLVM_DEBUG(getADDebugStream()
             << "Cloning original @" << original->getName()
             << " to jvp @" << jvp->getName() << '\n');
  // Create entry BB and arguments.
  auto *entry = jvp->createBasicBlock();
  createEntryArguments(jvp);

  // Clone
  SmallVector<SILValue, 4> entryArgs(entry->getArguments().begin(),
                                     entry->getArguments().end());
  cloneFunctionBody(original, entry, entryArgs);
  // If errors occurred, back out.
  if (errorOccurred)
    return true;

  LLVM_DEBUG(getADDebugStream() << "Generated JVP for "
             << original->getName() << ":\n" << *jvp);
  return errorOccurred;
}

//===----------------------------------------------------------------------===//
// `[differentiable]` attribute processing
//===----------------------------------------------------------------------===//

SILFunction *
ADContext::declareExternalAssociatedFunction(
    SILFunction *original, SILDifferentiableAttr *attr, StringRef name,
    AutoDiffAssociatedFunctionKind kind) {
  auto &module = getModule();
  auto &indices = attr->getIndices();
  auto originalTy = original->getLoweredFunctionType();
  auto originalLoc = original->getLocation();
  auto assocGenSig = getAssociatedFunctionGenericSignature(attr, original);
  auto assocFnTy = originalTy->getAutoDiffAssociatedFunctionType(
      indices.parameters, indices.source, /*differentiationOrder*/ 1, kind,
      module, LookUpConformanceInModule(module.getSwiftModule()), assocGenSig);
  SILOptFunctionBuilder fb(getTransform());
  // Create external function declaration.
  auto *assocFn = fb.createFunction(
      SILLinkage::PublicExternal, name, assocFnTy,
      /*genericEnv*/ nullptr, originalLoc, original->isBare(), IsNotTransparent,
      original->isSerialized(), original->isDynamicallyReplaceable());
  // Note: Setting debug scope prevents crashes during later transforms.
  assocFn->setDebugScope(new (module) SILDebugScope(originalLoc, assocFn));
  return assocFn;
}

static SILFunction *createEmptyVJP(
    ADContext &context, SILFunction *original, SILDifferentiableAttr *attr,
    bool isExported) {
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Creating VJP:\n\t";
    s << "Original type: " << original->getLoweredFunctionType() << "\n\t";
  });

  auto &module = context.getModule();
  auto originalTy = original->getLoweredFunctionType();
  auto indices = attr->getIndices();

  // === Create an empty VJP. ===
  auto vjpName = original->getASTContext()
                     .getIdentifier("AD__" + original->getName().str() +
                                    "__vjp_" + indices.mangle())
                     .str();
  auto vjpGenericSig = getAssociatedFunctionGenericSignature(attr, original);

  // RAII that pushes the original function's generic signature to
  // `module.Types` so that the calls `module.Types.getTypeLowering()` below
  // will know the VJP's generic parameter types.
  Lowering::GenericContextScope genericContextScope(
      module.Types, vjpGenericSig);

  auto *vjpGenericEnv = vjpGenericSig
      ? vjpGenericSig->createGenericEnvironment()
      : nullptr;
  auto vjpType = originalTy->getAutoDiffAssociatedFunctionType(
      indices.parameters, indices.source, /*differentiationOrder*/ 1,
      AutoDiffAssociatedFunctionKind::VJP, module,
      LookUpConformanceInModule(module.getSwiftModule()), vjpGenericSig);

  SILOptFunctionBuilder fb(context.getTransform());
  auto linkage = autodiff::getAutoDiffAssociatedFunctionLinkage(
      original->getLinkage(), isExported);
  auto *vjp = fb.createFunction(linkage, vjpName, vjpType, vjpGenericEnv,
                                original->getLocation(), original->isBare(),
                                IsNotTransparent, original->isSerialized(),
                                original->isDynamicallyReplaceable());
  vjp->setOwnershipEliminated();
  vjp->setDebugScope(new (module) SILDebugScope(original->getLocation(), vjp));
  attr->setVJPName(vjpName);

  LLVM_DEBUG(llvm::dbgs() << "VJP type: " << vjp->getLoweredFunctionType()
                          << "\n");
  return vjp;
}

static SILFunction *createEmptyJVP(
    ADContext &context, SILFunction *original, SILDifferentiableAttr *attr,
    bool isExported) {
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    s << "Creating JVP:\n\t";
    s << "Original type: " << original->getLoweredFunctionType() << "\n\t";
  });

  auto &module = context.getModule();
  auto originalTy = original->getLoweredFunctionType();
  auto indices = attr->getIndices();

  // === Create an empty JVP. ===
  auto jvpName = original->getASTContext()
  .getIdentifier("AD__" + original->getName().str() +
                 "__jvp_" + indices.mangle())
  .str();
  auto jvpGenericSig = getAssociatedFunctionGenericSignature(attr, original);

  // RAII that pushes the original function's generic signature to
  // `module.Types` so that the calls `module.Types.getTypeLowering()` below
  // will know the VJP's generic parameter types.
  Lowering::GenericContextScope genericContextScope(
      module.Types, jvpGenericSig);

  auto *jvpGenericEnv = jvpGenericSig
      ? jvpGenericSig->createGenericEnvironment()
      : nullptr;
  auto jvpType = originalTy->getAutoDiffAssociatedFunctionType(
      indices.parameters, indices.source, /*differentiationOrder*/ 1,
      AutoDiffAssociatedFunctionKind::JVP, module,
      LookUpConformanceInModule(module.getSwiftModule()), jvpGenericSig);

  SILOptFunctionBuilder fb(context.getTransform());
  auto linkage = autodiff::getAutoDiffAssociatedFunctionLinkage(
      original->getLinkage(), isExported);
  auto *jvp = fb.createFunction(linkage, jvpName, jvpType, jvpGenericEnv,
                                original->getLocation(), original->isBare(),
                                IsNotTransparent, original->isSerialized(),
                                original->isDynamicallyReplaceable());
  jvp->setOwnershipEliminated();
  jvp->setDebugScope(new (module) SILDebugScope(original->getLocation(), jvp));
  attr->setJVPName(jvpName);

  LLVM_DEBUG(llvm::dbgs() << "JVP type: " << jvp->getLoweredFunctionType()
             << "\n");
  return jvp;
}

/// Returns true on error.
bool ADContext::processDifferentiableAttribute(
    SILFunction *original, SILDifferentiableAttr *attr,
    DifferentiationInvoker invoker) {
  auto &module = getModule();
  // Try to look up JVP only if attribute specifies JVP name or if original
  // function is an external declaration. If JVP function cannot be found,
  // create an external JVP reference.
  StringRef jvpName;
  SILFunction *jvp = nullptr;
  if (attr->hasJVP()) {
    jvpName = attr->getJVPName();
  } else if (original->isExternalDeclaration()) {
    jvpName = original->getASTContext()
                  .getIdentifier("AD__" + original->getName().str() +
                                 "__jvp_" + attr->getIndices().mangle())
                  .str();
  }
  if (!jvpName.empty()) {
    jvp = module.lookUpFunction(jvpName);
    if (!jvp)
      jvp = declareExternalAssociatedFunction(
          original, attr, jvpName, AutoDiffAssociatedFunctionKind::JVP);
    attr->setJVPName(jvpName);
  }

  // If differentiation is triggered by `[differentiable]`, associated function
  // should share linkage of original function.
  auto isAssocFnExported =
      invoker.getKind() ==
          DifferentiationInvoker::Kind::SILDifferentiableAttribute;

  // Try to look up VJP only if attribute specifies VJP name or if original
  // function is an external declaration. If VJP function cannot be found,
  // create an external VJP reference.
  StringRef vjpName;
  SILFunction *vjp = nullptr;
  if (attr->hasVJP()) {
    vjpName = attr->getVJPName();
  } else if (original->isExternalDeclaration()) {
    vjpName = original->getASTContext()
                  .getIdentifier("AD__" + original->getName().str() +
				                         "__vjp_" + attr->getIndices().mangle())
                  .str();
  }
  if (!vjpName.empty()) {
    vjp = module.lookUpFunction(vjpName);
    if (!vjp)
      vjp = declareExternalAssociatedFunction(
          original, attr, vjpName, AutoDiffAssociatedFunctionKind::VJP);
    attr->setVJPName(vjpName);
  }

  // If the JVP doesn't exist, need to synthesize it.
  auto vjpGenerated = false;
  if (!vjp) {
    // Diagnose:
    // - Functions with no return.
    // - Functions with unsupported control flow.
    if (diagnoseNoReturn(*this, original, invoker) ||
        diagnoseUnsupportedControlFlow(*this, original, invoker))
      return true;
    
    vjpGenerated = true;
    vjp = createEmptyVJP(*this, original, attr, isAssocFnExported);
    getGeneratedFunctions().push_back(vjp);
    VJPEmitter emitter(*this, original, attr, vjp, invoker);
    if (emitter.run()) {
      return true;
    }
  }
      
  // If the JVP doesn't exist, need to synthesize it.
  if (!jvp) {
    // Diagnose:
    // - Functions with no return.
    // - Functions with unsupported control flow.
    if (vjpGenerated && (diagnoseNoReturn(*this, original, invoker) ||
        diagnoseUnsupportedControlFlow(*this, original, invoker)))
      return true;

    jvp = createEmptyJVP(*this, original, attr, isAssocFnExported);
    getGeneratedFunctions().push_back(jvp);

    if (vjpGenerated) {
      JVPEmitter emitter(*this, original, attr, jvp, invoker);
      return emitter.run();
    } else {
      LLVM_DEBUG(getADDebugStream()
                 << "Generating empty JVP for original @"
                 << original->getName() << '\n');
      // Create empty body of JVP if the user defined their own custom VJP.
      // Return undef.
      auto *entry = jvp->createBasicBlock();
      createEntryArguments(jvp);
      auto diffConv = jvp->getConventions();
      SILBuilder builder(entry);
      auto loc = jvp->getLocation();
      builder.createReturn(loc, SILUndef::get(
          jvp->mapTypeIntoContext(diffConv.getSILResultType()),
          *jvp));
      LLVM_DEBUG(getADDebugStream() << "Generated empty JVP for "
                 << original->getName() << ":\n" << *jvp);
    }
  }

  return false;
}

//===----------------------------------------------------------------------===//
// Differentiation pass implementation
//===----------------------------------------------------------------------===//

/// The automatic differentiation pass.
namespace {
class Differentiation : public SILModuleTransform {
public:
  Differentiation() : SILModuleTransform() {}
  void run() override;
};
} // end anonymous namespace

SILFunction *
ADContext::getOrCreateSubsetParametersThunkForLinearMap(
    SILFunction *parentThunk, CanSILFunctionType linearMapType,
    CanSILFunctionType targetType, AutoDiffAssociatedFunctionKind kind,
    SILAutoDiffIndices desiredIndices, SILAutoDiffIndices actualIndices) {
  LLVM_DEBUG(getADDebugStream() << "Getting a subset parameters thunk for " <<
             linearMapType << " from " << actualIndices << " to " <<
             desiredIndices << '\n');

  SubstitutionMap interfaceSubs = parentThunk->getForwardingSubstitutionMap();
  GenericEnvironment *genericEnv = parentThunk->getGenericEnvironment();
  auto thunkType = buildThunkType(
      parentThunk, linearMapType, targetType, genericEnv, interfaceSubs,
      /*withoutActuallyEscaping*/ true,
      DifferentiationThunkKind::Reabstraction);

  // TODO: Use more principled mangling.
  std::string thunkName;
  switch (kind) {
    case AutoDiffAssociatedFunctionKind::JVP:
      thunkName = "differential";
      break;
    case AutoDiffAssociatedFunctionKind::VJP:
      thunkName = "pullback";
  }
  Mangle::ASTMangler mangler;
  auto fromInterfaceType =
      linearMapType->mapTypeOutOfContext()->getCanonicalType();
  auto toInterfaceType = targetType->mapTypeOutOfContext()->getCanonicalType();
  CanType dynamicSelfType;
  thunkName = "AD__" + mangler.mangleReabstractionThunkHelper(
      thunkType, fromInterfaceType, toInterfaceType, dynamicSelfType,
      module.getSwiftModule()) + "_" + desiredIndices.mangle() + "_" +
      thunkName;
  thunkName += "_thunk";

  auto loc = parentThunk->getLocation();
  SILOptFunctionBuilder fb(getTransform());
  auto *thunk = fb.getOrCreateSharedFunction(
      loc, thunkName, thunkType, IsBare, IsTransparent, IsSerialized,
      ProfileCounter(), IsThunk, IsNotDynamic);

  if (!thunk->empty())
    return thunk;

  thunk->setGenericEnvironment(genericEnv);
  thunk->setOwnershipEliminated();
  auto *entry = thunk->createBasicBlock();
  SILBuilder builder(entry);
  createEntryArguments(thunk);

  // Get arguments.
  SmallVector<SILValue, 4> arguments;
  SmallVector<AllocStackInst *, 4> localAllocations;

  // Build a `.zero` argument for the given `Differentiable`-conforming type.
  auto buildZeroArgument = [&](SILType zeroSILType) {
    auto zeroSILObjType = zeroSILType.getObjectType();
    auto zeroType = zeroSILType.getASTType();
    auto *swiftMod = getModule().getSwiftModule();
    auto tangentSpace = zeroType->getAutoDiffAssociatedTangentSpace(
      LookUpConformanceInModule(swiftMod));
    assert(tangentSpace && "No tangent space for this type");
    switch (tangentSpace->getKind()) {
    case VectorSpace::Kind::Vector: {
      auto *buf = builder.createAllocStack(loc, zeroSILObjType);
      localAllocations.push_back(buf);
      emitZeroIntoBuffer(builder, zeroType, buf, loc);
      if (zeroSILType.isAddress())
        arguments.push_back(buf);
      else {
        auto loq = getBufferLOQ(buf->getType().getASTType(), *thunk);
        auto *arg = builder.createLoad(loc, buf, loq);
        arguments.push_back(arg);
      }
      break;
    }
    case VectorSpace::Kind::Tuple: {
      llvm_unreachable(
          "Unimplemented: Handle zero initialization for tuples");
    }
    case VectorSpace::Kind::Function:
      llvm_unreachable(
          "Unimplemented: Emit thunks for abstracting zero initialization");
    }
  };

  // `actualIndices` and `desiredIndices` are with respect to the original
  // function. However, the differential parameters and pullback results may
  // already be w.r.t. a subset. We create a map between the original function's
  // actual parameter indices and the linear map's actual indices.
  // Example:
  //   Original: (T0, T1, T2) -> R
  //   Actual indices: 0, 2
  //   Original differential: (T0, T2) -> R
  //   Original pullback: R -> (T0, T2)
  //   Desired indices w.r.t. original: 2
  //   Desired indices w.r.t. linear map: 1
  SmallVector<unsigned, 4> actualParamIndicesMap(
      actualIndices.parameters->getCapacity(), UINT_MAX);
  {
    unsigned indexInBitVec = 0;
    for (auto index : actualIndices.parameters->getIndices()) {
      actualParamIndicesMap[index] = indexInBitVec;
      indexInBitVec++;
    }
  }
  auto mapOriginalParameterIndex = [&](unsigned index) -> unsigned {
    auto mappedIndex = actualParamIndicesMap[index];
    assert(mappedIndex < actualIndices.parameters->getCapacity());
    return mappedIndex;
  };

  switch (kind) {
  // Differential arguments are:
  // - All indirect results, followed by:
  // - An interleaving of:
  //   - Thunk arguments (when parameter index is in both desired and actual
  //     indices).
  //   - Zeros (when parameter is not in desired indices).
  case AutoDiffAssociatedFunctionKind::JVP: {
    // Forward all indirect results.
    arguments.append(thunk->getIndirectResults().begin(),
                     thunk->getIndirectResults().end());
    auto toArgIter = thunk->getArgumentsWithoutIndirectResults().begin();
    auto useNextArgument = [&]() {
      arguments.push_back(*toArgIter++);
    };
    // Iterate over actual indices.
    for (unsigned i : actualIndices.parameters->getIndices()) {
      // If index is desired, use next argument.
      if (desiredIndices.isWrtParameter(i)) {
        useNextArgument();
      }
      // Otherwise, construct and use a zero argument.
      else {
        auto zeroSILType =
            linearMapType->getParameters()[mapOriginalParameterIndex(i)]
                .getSILStorageType();
        buildZeroArgument(zeroSILType);
      }
    }
    break;
  }
  // Pullback arguments are:
  // - An interleaving of:
  //   - Thunk indirect results (when parameter index is in both desired and
  //     actual indices).
  //   - Zeros (when parameter is not in desired indices).
  // - All actual arguments.
  case AutoDiffAssociatedFunctionKind::VJP: {
    auto toIndirectResultsIter = thunk->getIndirectResults().begin();
    auto useNextResult = [&]() {
      arguments.push_back(*toIndirectResultsIter++);
    };
    // Iterate over actual indices.
    for (unsigned i : actualIndices.parameters->getIndices()) {
      auto resultInfo =
          linearMapType->getResults()[mapOriginalParameterIndex(i)];
      // Skip direct results. Only indirect results are relevant as arguments.
      if (resultInfo.isFormalDirect())
        continue;
      // If index is desired, use next indirect result.
      if (desiredIndices.isWrtParameter(i)) {
        useNextResult();
        continue;
      }
      // Otherwise, construct and use a zero indirect result.
      buildZeroArgument(resultInfo.getSILStorageType());
    }
    // Foward all actual non-indirect-result arguments.
    arguments.append(thunk->getArgumentsWithoutIndirectResults().begin(),
                     thunk->getArgumentsWithoutIndirectResults().end() - 1);
    break;
  }
  }

  // Get the linear map thunk argument and apply it.
  auto *linearMap = thunk->getArguments().back();
  auto *ai = builder.createApply(
      loc, linearMap, SubstitutionMap(), arguments, /*isNonThrowing*/ false);

  // If differential thunk, deallocate local allocations and directly return
  // `apply` result.
  if (kind == AutoDiffAssociatedFunctionKind::JVP) {
    for (auto *alloc : reversed(localAllocations))
      builder.createDeallocStack(loc, alloc);
    builder.createReturn(loc, ai);
    return thunk;
  }

  // If pullback thunk, return only the desired results and clean up the
  // undesired results.
  SmallVector<SILValue, 8> pullbackDirectResults;
  extractAllElements(ai, builder, pullbackDirectResults);
  SmallVector<SILValue, 8> allResults;
  collectAllActualResultsInTypeOrder(
      ai, pullbackDirectResults,
      ai->getIndirectSILResults(), allResults);

  SmallVector<SILValue, 8> results;
  for (unsigned i : actualIndices.parameters->getIndices()) {
    // If result is desired:
    // - Do nothing if result is indirect.
    //   (It was already forwarded to the `apply` instruction).
    // - Push it to `results` if result is direct.
    auto result = allResults[mapOriginalParameterIndex(i)];
    if (desiredIndices.isWrtParameter(i)) {
      if (result->getType().isAddress())
        continue;
      results.push_back(result);
    }
    // Otherwise, cleanup the unused results.
    else {
      emitCleanup(builder, loc, result);
    }
  }
  // Deallocate local allocations and return final direct result.
  for (auto *alloc : reversed(localAllocations))
    builder.createDeallocStack(loc, alloc);
  auto result = joinElements(results, builder, loc);
  builder.createReturn(loc, result);

  getGeneratedFunctions().push_back(thunk);
  return thunk;
}

std::pair<SILFunction *, SubstitutionMap>
ADContext::getOrCreateSubsetParametersThunkForAssociatedFunction(
    SILValue origFnOperand, SILValue assocFn,
    AutoDiffAssociatedFunctionKind kind, SILAutoDiffIndices desiredIndices,
    SILAutoDiffIndices actualIndices) {
  LLVM_DEBUG(getADDebugStream() << "Getting a subset parameters thunk for "
             "associated function " << assocFn << " of the original function "
             << origFnOperand << " from " << actualIndices << " to " <<
             desiredIndices << '\n');

  auto origFnType = origFnOperand->getType().castTo<SILFunctionType>();
  auto &module = getModule();
  auto lookupConformance = LookUpConformanceInModule(module.getSwiftModule());

  // Compute target type for thunking.
  auto assocFnType = assocFn->getType().castTo<SILFunctionType>();
  auto targetType = origFnType->getAutoDiffAssociatedFunctionType(
      desiredIndices.parameters, desiredIndices.source,
      /*differentiationOrder*/ 1, kind, module, lookupConformance);
  auto *caller = assocFn->getFunction();
  if (targetType->hasArchetype()) {
    auto substTargetType = caller->mapTypeIntoContext(
        targetType->mapTypeOutOfContext())->getCanonicalType();
    targetType = SILType::getPrimitiveObjectType(substTargetType)
        .castTo<SILFunctionType>();
  }
  assert(assocFnType->getNumParameters() == targetType->getNumParameters());
  assert(assocFnType->getNumResults() == targetType->getNumResults());

  // Build thunk type.
  SubstitutionMap interfaceSubs;
  GenericEnvironment *genericEnv = nullptr;
  auto thunkType = buildThunkType(
      assocFn->getFunction(), assocFnType, targetType, genericEnv,
      interfaceSubs, /*withoutActuallyEscaping*/ false,
      DifferentiationThunkKind::IndexSubset);

  // FIXME: The logic for resolving `assocRef` does not reapply function
  // conversions, which is problematic if `assocFn` is a `partial_apply`
  // instruction.
  StringRef origName;
  if (auto *origFnRef =
          peerThroughFunctionConversions<FunctionRefInst>(origFnOperand)) {
    origName = origFnRef->getReferencedFunctionOrNull()->getName();
  } else if (auto *origMethodInst =
                 peerThroughFunctionConversions<MethodInst>(origFnOperand)) {
    origName = origMethodInst->getMember().getAnyFunctionRef()
        ->getAbstractFunctionDecl()->getNameStr();
  }
  assert(!origName.empty() && "Original function name could not be resolved");
  // TODO: Use more principled mangling.
  std::string thunkName;
  switch (kind) {
    case AutoDiffAssociatedFunctionKind::JVP:
      thunkName = "jvp";
      break;
    case AutoDiffAssociatedFunctionKind::VJP:
      thunkName = "vjp";
  }
  Mangle::ASTMangler mangler;
  auto fromInterfaceType =
      assocFnType->mapTypeOutOfContext()->getCanonicalType();
  auto toInterfaceType = targetType->mapTypeOutOfContext()->getCanonicalType();
  CanType dynamicSelfType;
  thunkName = "AD__orig_" + origName.str() + "_" +
      mangler.mangleReabstractionThunkHelper(
          thunkType, fromInterfaceType, toInterfaceType, dynamicSelfType,
          module.getSwiftModule()) + "_" + desiredIndices.mangle() + "_" +
          thunkName;
  thunkName += "_thunk";

  auto loc = origFnOperand.getLoc();
  SILOptFunctionBuilder fb(getTransform());
  auto *thunk = fb.getOrCreateSharedFunction(
      loc, thunkName, thunkType, IsBare, IsTransparent, caller->isSerialized(),
      ProfileCounter(), IsThunk, IsNotDynamic);

  if (!thunk->empty())
    return {thunk, interfaceSubs};

  thunk->setOwnershipEliminated();
  thunk->setGenericEnvironment(genericEnv);
  auto *entry = thunk->createBasicBlock();
  SILBuilder builder(entry);
  createEntryArguments(thunk);

  SubstitutionMap assocSubstMap;
  if (auto *partialApply = dyn_cast<PartialApplyInst>(assocFn))
    assocSubstMap = partialApply->getSubstitutionMap();

  // FIXME: The logic for resolving `assocRef` does not reapply function
  // conversions, which is problematic if `assocFn` is a `partial_apply`
  // instruction.
  SILValue assocRef;
  if (auto *assocFnRef =
          peerThroughFunctionConversions<FunctionRefInst>(assocFn)) {
    auto *assoc = assocFnRef->getReferencedFunctionOrNull();
    assocRef = builder.createFunctionRef(loc, assoc);
  } else if (auto *assocMethodInst =
                 peerThroughFunctionConversions<WitnessMethodInst>(assocFn)) {
    assocRef = builder.createWitnessMethod(
        loc, assocMethodInst->getLookupType(),
        assocMethodInst->getConformance(), assocMethodInst->getMember(),
        thunk->mapTypeIntoContext(assocMethodInst->getType()));
  }
  assert(assocRef && "Expected associated function to be resolved");

  assocSubstMap = assocSubstMap.subst(thunk->getForwardingSubstitutionMap());
  assocFnType = assocRef->getType().castTo<SILFunctionType>();

  SmallVector<SILValue, 4> arguments;
  arguments.append(thunk->getArguments().begin(), thunk->getArguments().end());
  auto *apply = builder.createApply(
      loc, assocRef, assocSubstMap, arguments, /*isNonThrowing*/ false);

  // Extract all direct results.
  SmallVector<SILValue, 8> directResults;
  extractAllElements(apply, builder, directResults);
  auto originalDirectResults = ArrayRef<SILValue>(directResults).drop_back(1);
  auto originalDirectResult =
      joinElements(originalDirectResults, builder, apply->getLoc());
  auto linearMap = directResults.back();

  auto linearMapType = linearMap->getType().castTo<SILFunctionType>();
  auto linearMapTargetType = targetType->getResults().back().getSILStorageType()
      .castTo<SILFunctionType>();

  auto *innerThunk = getOrCreateSubsetParametersThunkForLinearMap(
      thunk, linearMapType, linearMapTargetType, kind,
      desiredIndices, actualIndices);

  auto *innerThunkFRI = builder.createFunctionRef(loc, innerThunk);
  auto *newDerivative = builder.createPartialApply(
      loc, innerThunkFRI, thunk->getForwardingSubstitutionMap(), {linearMap},
      ParameterConvention::Direct_Guaranteed);

  assert(origFnType->getResults().size() == 1);
  if (origFnType->getResults().front().isFormalDirect()) {
    auto result = joinElements(
        {originalDirectResult, newDerivative}, builder, loc);
    builder.createReturn(loc, result);
  } else {
    builder.createReturn(loc, newDerivative);
  }

  getGeneratedFunctions().push_back(thunk);
  return {thunk, interfaceSubs};
}

SILValue ADContext::promoteToDifferentiableFunction(
    AutoDiffFunctionInst *inst, SILBuilder &builder, SILLocation loc,
    DifferentiationInvoker invoker) {
  auto origFnOperand = inst->getOriginalFunction();
  auto origFnTy = origFnOperand->getType().castTo<SILFunctionType>();
  auto parameterIndices = inst->getParameterIndices();
  unsigned resultIndex = resultIndices[inst];
  unsigned differentiationOrder = inst->getDifferentiationOrder();

  // Handle curry thunk applications specially.
  if (auto *ai = dyn_cast<ApplyInst>(origFnOperand)) {
    if (auto *thunkRef = dyn_cast<FunctionRefInst>(ai->getCallee())) {
      SILAutoDiffIndices desiredIndices(resultIndex, parameterIndices);
      auto *thunk = thunkRef->getReferencedFunctionOrNull();
      auto newThunkName = "AD__" + thunk->getName().str() +
          "__cloned_curry_thunk_" + desiredIndices.mangle();

      auto thunkTy = thunk->getLoweredFunctionType();
      auto thunkResult = thunkTy->getSingleResult();
      if (auto resultFnTy = thunkResult.getType()->getAs<SILFunctionType>()) {
        // Construct new curry thunk type with `@differentiable` result.
        auto diffableResultFnTy = resultFnTy->getWithExtInfo(
            resultFnTy->getExtInfo()
                .withDifferentiabilityKind(DifferentiabilityKind::Normal));
        auto newThunkResult = thunkResult.getWithType(diffableResultFnTy);
        auto thunkType = SILFunctionType::get(
            thunkTy->getGenericSignature(), thunkTy->getExtInfo(),
            thunkTy->getCoroutineKind(), thunkTy->getCalleeConvention(),
            thunkTy->getParameters(), {}, {newThunkResult}, {},
            thunkTy->getASTContext());

        // Construct new curry think.
        SILOptFunctionBuilder fb(transform);
        auto *newThunk = fb.getOrCreateFunction(
            loc, newThunkName,
            getSpecializedLinkage(thunk, thunk->getLinkage()), thunkType,
            thunk->isBare(), thunk->isTransparent(), thunk->isSerialized(),
            thunk->isDynamicallyReplaceable(), ProfileCounter(),
            thunk->isThunk());
        if (newThunk->empty()) {
          newThunk->setOwnershipEliminated();
          SILFunctionCloner cloner(newThunk);
          cloner.cloneFunction(thunk);
        }

        auto *retInst =
            cast<ReturnInst>(newThunk->findReturnBB()->getTerminator());
        AutoDiffFunctionInst *adfi;
        {
          SILBuilder builder(retInst);
          adfi = createAutoDiffFunction(builder, loc, parameterIndices,
                                        differentiationOrder,
                                        retInst->getOperand());
          resultIndices[adfi] = resultIndex;
          builder.createReturn(loc, adfi);
        }
        retInst->eraseFromParent();

        getAutoDiffFunctionInsts().push_back(adfi);
        if (processAutoDiffFunctionInst(adfi))
          return nullptr;

        auto *newThunkRef = builder.createFunctionRef(loc, newThunk);
        SmallVector<SILValue, 8> arguments(ai->getArguments().begin(),
                                           ai->getArguments().end());
        auto *newApply = builder.createApply(
            ai->getLoc(), newThunkRef, ai->getSubstitutionMap(), arguments,
            ai->isNonThrowing());
        return newApply;
      }
    }
  }

  SILAutoDiffIndices desiredIndices(resultIndex, parameterIndices);
  SmallVector<SILValue, 2> assocFns;
  for (auto assocFnKind : {AutoDiffAssociatedFunctionKind::JVP,
                           AutoDiffAssociatedFunctionKind::VJP}) {
    auto assocFnAndIndices = emitAssociatedFunctionReference(
        *this, builder, desiredIndices, assocFnKind,
        origFnOperand, invoker);
    // Show an error at the operator, highlight the argument, and show a note
    // at the definition site of the argument.
    if (!assocFnAndIndices)
      return nullptr;

    auto assocFn = assocFnAndIndices->first;
    getGeneratedAssociatedFunctionReferences().push_back(assocFn);

    // If desired indices are a subset of actual indices, create a "subset
    // indices thunk".
    // - For JVPs: the thunked JVP returns a differential taking fewer
    //   parameters (using `.zero` for the dropped parameters).
    // - For VJPs: the thunked VJP returns a pullback that drops the unused
    //   tangent values.
    auto actualIndices = assocFnAndIndices->second;
    // NOTE: `desiredIndices` may come from a partially-applied function and
    // have smaller capacity than `actualIndices`. We expect this logic to go
    // away when we support `@differentiable` partial apply.
    // if (actualIndices != desiredIndices) { // TODO: Re-enable.
    if (actualIndices.source != desiredIndices.source ||
        !actualIndices.parameters->equals(
            desiredIndices.parameters->extendingCapacity(getASTContext(),
                actualIndices.parameters->getCapacity()))) {
      assert(actualIndices.parameters->isSupersetOf(desiredIndices.parameters));
      SILFunction *thunk;
      SubstitutionMap interfaceSubs;
      std::tie(thunk, interfaceSubs) =
          getOrCreateSubsetParametersThunkForAssociatedFunction(
              origFnOperand, assocFn, assocFnKind, desiredIndices,
              actualIndices);
      auto *thunkFRI = builder.createFunctionRef(loc, thunk);
      if (auto genSig =
              thunk->getLoweredFunctionType()->getGenericSignature()) {
        assocFn = builder.createPartialApply(
            loc, thunkFRI, interfaceSubs, {},
            ParameterConvention::Direct_Guaranteed);
      } else {
        assocFn = thunkFRI;
      }
    }
    auto expectedAssocFnTy = origFnTy->getAutoDiffAssociatedFunctionType(
        parameterIndices, resultIndex, differentiationOrder,
        assocFnKind, getModule(),
        LookUpConformanceInModule(getModule().getSwiftModule()));
    // If `assocFn` is `@convention(thin)` but is expected to be
    // `@convention(thick)`, emit a `thin_to_thick` instruction.
    if (expectedAssocFnTy->getRepresentation()
            == SILFunctionTypeRepresentation::Thick &&
        assocFn->getType().castTo<SILFunctionType>()->getRepresentation()
            == SILFunctionTypeRepresentation::Thin) {
      assocFn = builder.createThinToThickFunction(
          loc, assocFn, SILType::getPrimitiveObjectType(expectedAssocFnTy));
    }

    assocFns.push_back(assocFn);
  }

  auto *adfi = createAutoDiffFunction(
      builder, loc, parameterIndices, differentiationOrder, origFnOperand,
      assocFns);
  resultIndices[adfi] = resultIndex;
  getAutoDiffFunctionInsts().push_back(adfi);
  return adfi;
}

/// Fold `autodiff_function_extract` users of the given `autodiff_function`
/// instruction, directly replacing them with `autodiff_function` instruction
/// operands. If the `autodiff_function` instruction has no remaining uses,
/// delete the instruction itself after folding.
///
/// Folding can be disabled by the `SkipFoldingAutoDiffFunctionExtraction` flag
/// for SIL testing purposes.
// FIXME: This function is not correctly detecting the foldable pattern and
// needs to be rewritten.
void ADContext::foldAutoDiffFunctionExtraction(AutoDiffFunctionInst *source) {
  // Iterate through all `autodiff_function` instruction uses.
  for (auto use : source->getUses()) {
    auto *adfei = dyn_cast<AutoDiffFunctionExtractInst>(use->getUser());
    // If user is not an `autodiff_function_extract` instruction, set flag to
    // false.
    if (!adfei)
      continue;
    // Fold original function extractors.
    if (adfei->getExtractee() == AutoDiffFunctionExtractee::Original) {
      auto originalFnValue = source->getOriginalFunction();
      adfei->replaceAllUsesWith(originalFnValue);
      adfei->eraseFromParent();
      continue;
    }
    // Fold associated function extractors.
    auto assocFnValue = source->getAssociatedFunction(
        adfei->getDifferentiationOrder(), adfei->getAssociatedFunctionKind());
    adfei->replaceAllUsesWith(assocFnValue);
    adfei->eraseFromParent();
  }
  // If the `autodiff_function` instruction has no remaining uses, erase it.
  if (isInstructionTriviallyDead(source)) {
    SILBuilder builder(source);
    for (auto &assocFn : source->getAssociatedFunctions())
      emitCleanup(builder, source->getLoc(), assocFn.get());
    source->eraseFromParent();
  }
  // Mark `source` as processed so that it won't be reprocessed after deletion.
  processedAutoDiffFunctionInsts.insert(source);
}

bool ADContext::processAutoDiffFunctionInst(AutoDiffFunctionInst *adfi) {
  LLVM_DEBUG({
    auto &s = getADDebugStream() << "Processing AutoDiffFunctionInst:\n";
    adfi->printInContext(s);
  });

  if (adfi->getNumAssociatedFunctions() ==
      autodiff::getNumAutoDiffAssociatedFunctions(
          adfi->getDifferentiationOrder()))
    return false;
  assert(adfi->getNumAssociatedFunctions() == 0 &&
         "some functions are already filled in but not all of them");

  SILFunction *parent = adfi->getFunction();
  auto loc = parent->getLocation();
  SILBuilder builder(adfi);

  auto differentiableFnValue =
      promoteToDifferentiableFunction(adfi, builder, loc, adfi);
  if (!differentiableFnValue)
    return true;
  // Mark `adfi` as processed so that it won't be reprocessed after deletion.
  processedAutoDiffFunctionInsts.insert(adfi);
  // Replace all uses of `adfi`.
  adfi->replaceAllUsesWith(differentiableFnValue);
  adfi->eraseFromParent();
  // If the promoted `@differentiable` function-typed value is an
  // `autodiff_function` instruction, fold `autodiff_function_extract`
  // instructions. If `autodiff_function_extract` folding is disabled, return.
  if (!SkipFoldingAutoDiffFunctionExtraction)
    if (auto *newADFI = dyn_cast<AutoDiffFunctionInst>(differentiableFnValue))
      foldAutoDiffFunctionExtraction(newADFI);
  transform.invalidateAnalysis(
      parent, SILAnalysis::InvalidationKind::FunctionBody);
  return false;
}

/// AD pass entry.
void Differentiation::run() {
  auto &module = *getModule();
  auto &astCtx = module.getASTContext();
  debugDump(module);

  // A global differentiation context.
  ADContext context(*this);

  // Register all `@differentiable` attributes and `autodiff_function`
  // instructions in the module that trigger differentiation.
  for (SILFunction &f : module) {
    for (auto *diffAttr : f.getDifferentiableAttrs()) {
      DifferentiationInvoker invoker(diffAttr);
      assert(!context.getInvokers().count(diffAttr) &&
             "[differentiable] attribute already has an invoker");
      context.getInvokers().insert({diffAttr, invoker});
      continue;
    }
    for (SILBasicBlock &bb : f)
      for (SILInstruction &i : bb)
        if (auto *adfi = dyn_cast<AutoDiffFunctionInst>(&i))
          context.getAutoDiffFunctionInsts().push_back(adfi);
  }

  // If nothing has triggered differentiation, there's nothing to do.
  if (context.getInvokers().empty() &&
      context.getAutoDiffFunctionInsts().empty())
    return;

  // AD relies on stdlib (the Swift module). If it's not imported, it's an
  // internal error.
  if (!astCtx.getStdlibModule()) {
    astCtx.Diags.diagnose(SourceLoc(),
                          diag::autodiff_internal_swift_not_imported);
    return;
  }

  bool errorOccurred = false;

  // Process all `[differentiable]` attributes.
  for (auto invokerPair : context.getInvokers()) {
    auto *attr = invokerPair.first;
    auto *original = attr->getOriginal();
    auto invoker = invokerPair.second;
    errorOccurred |=
        context.processDifferentiableAttribute(original, attr, invoker);
  }

  // Iteratively process `autodiff_function` instruction worklist.
  while (!context.getAutoDiffFunctionInsts().empty()) {
    auto *adfi = context.getAutoDiffFunctionInsts().back();
    context.getAutoDiffFunctionInsts().pop_back();
    // Skip instructions that have been already been processed.
    if (context.getProcessedAutoDiffFunctionInsts().count(adfi)) continue;
    errorOccurred |= context.processAutoDiffFunctionInst(adfi);
  }

  // If any error occurred while processing `[differentiable]` attributes or
  // `autodiff_function` instructions, clean up.
  if (errorOccurred) {
    context.cleanUp();
    return;
  }

  LLVM_DEBUG(getADDebugStream() << "All differentiation finished\n");
}

//===----------------------------------------------------------------------===//
// Pass creation
//===----------------------------------------------------------------------===//

SILTransform *swift::createDifferentiation() {
  return new Differentiation;
}
