//===--- TFDifferentiation.cpp - SIL Automatic Differentiation --*- C++ -*-===//
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

#include "swift/AST/AutoDiff.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/LoopUtils.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/DenseSet.h"

using namespace swift;
using llvm::DenseMap;
using llvm::SmallDenseSet;
using llvm::SmallDenseMap;
using llvm::SmallSet;

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
static raw_ostream &getADDebugStream() {
  return llvm::dbgs() << "[AD] ";
}

/// Given a dumpable value, dumps it to `llvm::dbgs()`.
template<typename T>
static inline void debugDump(T &v) {
  DEBUG(llvm::dbgs() << "\n==== BEGIN DEBUG DUMP ====\n" << v <<
        "\n==== END DEBUG DUMP ====\n");
}

/// Given a set of AD indices, mangles it into a textual form.
static std::string mangleADIndices(const SILReverseAutoDiffIndices &indices) {
  std::string result = "src_" + llvm::utostr(indices.source) + "_wrt_";
  interleave(indices.parameters.set_bits(),
             [&](unsigned idx) { result += llvm::utostr(idx); },
             [&]{ result += '_'; });
  return result;
}

/// Mangles an AD configuration. The mangling rule looks like
///   "grad_src_<src_idx>_wrt_<param_idx0>_<param_idx1>_..._<options>"
/// ... where options mangle as the following:
///   "_s" : seedable
///   "_p" : preserving_result
///   "_d" : delayed
static std::string mangleADConfig(
    const SILReverseAutoDiffConfig &config) {
  std::string result = "grad_" + mangleADIndices(config.indices);
  if (config.isSeedable())
    result += "_s";
  if (config.isPreservingResult())
    result += "_p";
  if (config.isDelayed())
    result += "_d";
  return result;
}

/// Creates arguments in the entry block based on the function type.
static void createEntryArguments(SILFunction *f) {
  auto *entry = f->getEntryBlock();
  auto conv = f->getConventions();
  assert((entry->getNumArguments() == 0 || conv.getNumSILArguments() == 0) &&
         "Entry already has arguments?!");
  for (auto indResultTy : conv.getIndirectSILResultTypes())
    entry->createFunctionArgument(indResultTy.getAddressType());
  for (auto paramTy : conv.getParameterSILTypes())
    entry->createFunctionArgument(paramTy);
}

/// Looks up a function in the current module. If it exists, returns it.
/// Otherwise, attempt to link it from imported modules. Returns null if such
/// function name does not exist.
static SILFunction *lookupOrLinkFunction(StringRef name, SILModule &module) {
  DEBUG(getADDebugStream() << "Looking up function " << name << '\n');
  assert(!name.empty());
  if (auto *localFn = module.lookUpFunction(name))
    return localFn;
  return module.findFunction(name, SILLinkage::PublicExternal);
}

/// Given a type, returns its formal SIL parameter info.
static SILParameterInfo getFormalParameterInfo(
    CanType type, SILModule &module) {
  SILType silTy = SILType::getPrimitiveObjectType(type);
  ParameterConvention conv;
  if (SILModuleConventions::isPassedIndirectlyInSIL(silTy, module))
    conv = ParameterConvention::Indirect_In;
  else if (silTy.isTrivial(module))
    conv = ParameterConvention::Direct_Unowned;
  else
    conv = ParameterConvention::Direct_Guaranteed;
  return { type, conv };
}

/// Given a type, returns its formal SIL result info.
static SILResultInfo getFormalResultInfo(CanType type, SILModule &module) {
  SILType silTy = SILType::getPrimitiveObjectType(type);
  ResultConvention conv;
  if (SILModuleConventions::isPassedIndirectlyInSIL(silTy, module))
    conv = ResultConvention::Indirect;
  else if (silTy.isTrivial(module))
    conv = ResultConvention::Unowned;
  else
    conv = ResultConvention::Owned;
  return { type, conv };
}

/// Given a function, gather all of its formal results (both direct and
/// indirect) in an order defined by its result type. Note that "formal results"
/// refer to result values in the body of the function, not at call sites.
static void collectAllFormalResultsInTypeOrder(
    SILFunction &function, SmallVectorImpl<SILValue> &results) {
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
    results.push_back(resInfo.isFormalDirect()
                      ? dirResults[dirResIdx++] : indResults[indResIdx++]);
}

/// Given a function call site, gather all of its actual results (both direct
/// and indirect) in an order defined by its result type.
template<typename IndResRange>
static void collectAllActualResultsInTypeOrder(
    ApplyInst *ai, ArrayRef<SILValue> extractedDirectResults,
    IndResRange &&indirectResults, SmallVectorImpl<SILValue> &results) {
  auto callee = ai->getCallee();
  SILFunctionConventions calleeConvs(callee->getType().getAs<SILFunctionType>(),
                                     ai->getModule());
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : calleeConvs.getResults())
    results.push_back(resInfo.isFormalDirect()
      ? extractedDirectResults[dirResIdx++] : indirectResults[indResIdx++]);
}

/// Given a range of types, joins these into a single type. If there's exactly
/// one element type, returns that element type. Otherwise, creates a tuple type
/// of all element types.
template<typename TypeRange>
static CanType joinElementTypes(TypeRange &&range, const ASTContext &ctx) {
  if (range.size() == 1)
    return range.front();
  auto typeElts = map<SmallVector<TupleTypeElt, 8>>(range, [&](Type type) {
    return type;
  });
  return TupleType::get(typeElts, ctx);
}

/// Given a range of SIL values, retrives the canonical types of these values,
/// and joins these types into a single type.
template<typename SILValueRange>
static CanType joinElementTypesFromValues(SILValueRange &&range,
                                          const ASTContext &ctx) {
  if (range.size() == 1)
    return range.front()->getType().getASTType();
  SmallVector<TupleTypeElt, 8> elts;
  transform(range, elts.begin(),
            [&](SILValue val) { return val->getType().getASTType(); });
  return TupleType::get(elts, ctx)->getCanonicalType();
}

/// Looks through the definition of a function value. If the source that
/// produced this function value is `function_ref` and the function is visible
/// (either in the same module or is serialized), returns the instruction.
/// Otherwise, returns null.
static FunctionRefInst *findReferenceToVisibleFunction(SILValue value) {
  auto *inst = value->getDefiningInstruction();
  if (!inst) return nullptr;
  if (auto *fri = dyn_cast<FunctionRefInst>(inst)) {
    auto *fn = fri->getReferencedFunction();
    if (&fn->getModule() == &inst->getModule() ||
        fn->isSerialized() == IsSerialized)
      return fri;
  }
  if (auto *thinToThink = dyn_cast<ThinToThickFunctionInst>(inst))
    return findReferenceToVisibleFunction(thinToThink->getOperand());
  if (auto *convertFn = dyn_cast<ConvertFunctionInst>(inst))
    return findReferenceToVisibleFunction(convertFn->getOperand());
  return nullptr;
}

/// Given an operator name, such as "+", and a protocol, returns the
/// "+" operator with type `(Self, Self) -> Self`. If the operator does not
/// exist in the protocol, returns null.
static FuncDecl *findAssociativeOperatorDeclInProtocol(
    DeclName operatorName, ProtocolDecl *protocol) {
  assert(operatorName.isOperator());
  // Find the operator requirement in the `VectorNumeric` protocol
  // declaration and cache it.
  auto plusLookup = protocol->lookupDirect(operatorName);
  // Find the `+` with type siguature `(Self, Self) -> Self`.
  for (auto *decl : plusLookup) {
    auto *fd = dyn_cast<FuncDecl>(decl);
    if (!fd || !fd->isBinaryOperator()) continue;
    auto *paramList = fd->getParameterList(1);
    auto *protoSelfTy = fd->getProtocolSelfType();
    // Make sure parameters have `Self` type.
    for (auto *param : paramList->getArray())
      if (!param->getType()->isEqual(protoSelfTy))
        continue;
    // Make sure the result type is also `Self`.
    if (!fd->getResultInterfaceType()->isEqual(protoSelfTy))
      continue;
    // This is the function type we want: `(Self, Self) -> Self`.
    return fd;
  }
  // Not found.
  return nullptr;
}

/// Assuming the buffer is for indirect passing, returns the store ownership
/// qualifier for creating a `store` instruction into the buffer.
static StoreOwnershipQualifier getBufferSOQ(Type type, SILModule &module) {
  return module.Types.getTypeLowering(type).isTrivial()
    ? StoreOwnershipQualifier::Trivial : StoreOwnershipQualifier::Init;
}

/// Assuming the buffer is for indirect passing, returns the load ownership
/// qualified for creating a `load` instruction from the buffer.
static LoadOwnershipQualifier getBufferLOQ(Type type, SILModule &module) {
  return module.Types.getTypeLowering(type).isTrivial()
    ? LoadOwnershipQualifier::Trivial : LoadOwnershipQualifier::Take;
}

//===----------------------------------------------------------------------===//
// Auxiliary data structures
//===----------------------------------------------------------------------===//

namespace {
class DifferentiationTask;

/// The invoker of a differentiation task. It can be some user syntax, e.g.
/// `#gradient` expression, the differentiation pass, or nothing at all. This
/// will be used to emit informative diagnostics.
struct DifferentiationInvoker {
public:
  /// The kind of the invoker of a differentiation task.
  enum class Kind {
    // No known invoker. This is the case when the differentiation is requested
    // from SIL source via a `gradient` instruction **without** being linked to
    // a Swift AST node.
    GradientInst,

    // Invoked by the indirect application of differentiation. This case has an
    // associated differentiation task reference.
    IndirectDifferentiation,

    // Invoked by a differential operator, such as `#gradient`, in the Swift
    // source. This case has an associated differential operator, i.e. a
    // `ReverseAutoDiffExpr`.
    DifferentialOperator,

    // Invoked by a `@differentiable` attribute in the Swift source. This case
    // has an associated `@differentiable` attribute.
    DifferentiableAttribute,
  };

private:
  Kind kind;
  union Value {
    /// The instruction associated with the `SILSource` case.
    GradientInst *gradientInst;
    Value(GradientInst *inst) : gradientInst(inst) {}

    /// The parent differentiation task associated with the
    /// `IndirectDifferentiation` case.
    std::pair<ApplyInst *,
              DifferentiationTask *> indirectDifferentiation;
    Value(ApplyInst *applyInst, DifferentiationTask *parentTask)
      : indirectDifferentiation({applyInst, parentTask}) {}

    /// The differential operator associated with the `DifferentialOperator`
    /// case.
    ReverseAutoDiffExpr *differentialOperator;
    Value(ReverseAutoDiffExpr *expr) : differentialOperator(expr) {}

    /// The `@differentiable` attribute associated with the
    /// `DifferentiableAttribute` case.
    DifferentiableAttr *differentiableAttribute;
    Value(DifferentiableAttr *attr) : differentiableAttribute(attr) {}
  } value;

  /*implicit*/
  DifferentiationInvoker(Kind kind, Value value)
    : kind(kind), value(value) {}

public:
  DifferentiationInvoker(GradientInst *inst)
    : kind(Kind::GradientInst), value(inst) {}
  DifferentiationInvoker(ApplyInst *applyInst,
                         DifferentiationTask *task)
    : kind(Kind::IndirectDifferentiation), value(applyInst, task) {}
  DifferentiationInvoker(ReverseAutoDiffExpr *expr)
    : kind(Kind::DifferentialOperator), value(expr) {}
  DifferentiationInvoker(DifferentiableAttr *attr)
    : kind(Kind::DifferentiableAttribute), value(attr) {}

  Kind getKind() const { return kind; }

  GradientInst *getGradientInst() const {
    assert(kind == Kind::GradientInst);
    return value.gradientInst;
  }

  std::pair<ApplyInst *, DifferentiationTask *>
  getIndirectDifferentiation() const {
    assert(kind == Kind::IndirectDifferentiation);
    return value.indirectDifferentiation;
  }

  ReverseAutoDiffExpr *getDifferentialOperator() const {
    assert(kind == Kind::DifferentialOperator);
    return value.differentialOperator;
  }

  DifferentiableAttr *getDifferentiableAttribute() const {
    assert(kind == Kind::DifferentiableAttribute);
    return value.differentiableAttribute;
  }

  void print(llvm::raw_ostream &os) const;
};
  
/// A differentiation task, specifying the original function and the
/// `[reverse_differentiable]` attribute on the function. PrimalGen and
/// AdjointGen will synthesize the primal and the adjoint for this task, filling
/// the primal and adjoint fields in the attribute.
///
/// NOTE: A task instance manages a `[reverse_differentiable]` SIL attribute and
/// shall be the only one that modifies this attribute.
class DifferentiationTask {
  friend llvm::DenseMapInfo<DifferentiationTask>;
  friend class ADContext;

private:
  /// The original function to be differentiated.
  SILFunction *original;

  /// The `[reverse_differentiable]` attribute on the original function. Since
  /// attribute synthesis is part of differentiation, a
  /// `[reverse_differentiable]` attribute must be available when a
  /// `DifferentiationTask` is created. The AD configuration resides within the
  /// attribute. This is guaranteed to be non-null.
  SILReverseDifferentiableAttr *attr;

  /// The invoker of this differentiation task.
  DifferentiationInvoker invoker;

  /// Mapping from original `apply` instructions to their corresponding
  /// differentiation tasks, if it's active. This is filled during primal
  /// synthesis, so that adjoint synthesis does not need to recompute the
  /// original function and differentiation indices.
  DenseMap<ApplyInst *, DifferentiationTask *> associatedTasks;

  /// Cache for primal and adjoint.
  SILFunction *primal = nullptr;
  SILFunction *adjoint = nullptr;

protected:
  /// Create a differentiation task.
  ///
  /// @param original The original function to be differentiated.
  /// @param attr The [reverse_differentiable] attribute to take control of.
  /// @param invoker The invoker of this differentiation task.
  /// @param module The module where differentiation happens.
  explicit DifferentiationTask(SILFunction *original,
                               SILReverseDifferentiableAttr *&&attr,
                               SILModule &module,
                               DifferentiationInvoker invoker)
    : original(original), attr(attr), invoker(invoker) {
    if (attr->hasPrimal())
      primal = lookupOrLinkFunction(attr->getPrimalName(), module);
    if (attr->hasAdjoint())
      adjoint = lookupOrLinkFunction(attr->getAdjointName(), module);
  }

public:
  DifferentiationTask(const DifferentiationTask &) = delete;
  DifferentiationTask &operator=(const DifferentiationTask &) = delete;

  SILFunction *getOriginal() const { return original; }
  SILReverseDifferentiableAttr *getAttribute() const { return attr; }
  DifferentiationInvoker getInvoker() const { return invoker; }
  
  const SILReverseAutoDiffIndices &getIndices() const {
    return attr->getIndices();
  }

  SILFunction *getPrimal() const { return primal; }
  SILFunction *getAdjoint() const { return adjoint; }

  void setPrimal(SILFunction *fn) {
    assert(fn); primal = fn;
    attr->setPrimalName(fn->getName());
  }
  
  void setAdjoint(SILFunction *fn) {
    assert(fn); adjoint = fn;
    attr->setAdjointName(fn->getName());
  }

  DenseMap<ApplyInst *, DifferentiationTask *> &getAssociatedTasks() {
    return associatedTasks;
  }

  bool isEqual(const DifferentiationTask &other) const {
    return original == other.original && attr == other.attr;
  }

  SILReverseAutoDiffConfig getMasterConfig() const {
    return SILReverseAutoDiffConfig::getMaster(getIndices());
  }

  void print(llvm::raw_ostream &os) const;
};

static
inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     DifferentiationInvoker invoker) {
  invoker.print(os);
  return os;
}

void DifferentiationInvoker::print(llvm::raw_ostream &os) const {
  os << "(differentiation_invoker ";
  switch (kind) {
  case Kind::GradientInst:
    os << "gradient_inst=(" << *getGradientInst() << ")";
    break;
  case Kind::IndirectDifferentiation: {
    auto indDiff = getIndirectDifferentiation();
    os << "indirect_differentiation=(apply_inst=(" << *indDiff.first
       << ") task=" << indDiff.second << ')';
    break;
  }
  case Kind::DifferentialOperator:
    os << "differential_operator=(";
    getDifferentialOperator()->print(os);
    os << ")";
    break;
  case Kind::DifferentiableAttribute:
    os << "differentiable_attribute=(";
    getDifferentiableAttribute()->print(os);
    os << ")";
    break;
  }
  os << ')';
}

void DifferentiationTask::print(llvm::raw_ostream &os) const {
  os << "(differentiation_task original=@" << original->getName()
     << " attribute=";
  attr->print(os);
  os << " invoker=" << invoker << ")";
}

using GradientLookupKey = std::pair<SILFunction *,
                                    SILReverseAutoDiffConfig>;

//===----------------------------------------------------------------------===//
// ADContext - Per-module contextual information for the Differentiation pass.
//===----------------------------------------------------------------------===//

class ADContext {
private:
  /// The module where Differentiation is performed on.
  SILModule &module;

  /// AST context.
  ASTContext &astCtx = module.getASTContext();

  /// Shared pass manager.
  SILPassManager &passManager;

  /// A mapping from functions and AD configurations to gradient functions.
  ///
  /// NOTE: The parameter index array is hashed by reference, which is expected
  /// to point to [reverse_differentiable wrt ...]'s trailing index storage.
  DenseMap<GradientLookupKey, SILFunction *> gradientMap;

  /// Queue of differentiation tasks.
  SmallVector<std::unique_ptr<DifferentiationTask>, 32> differentiationTasks;
  /// Mapping from enqueued differentiation tasks to their indices in
  /// `differentiationTasks`.
  SmallDenseMap<
    std::pair<SILFunction *, SILReverseAutoDiffIndices>,
    unsigned> enqueuedTaskIndices;

  /// SIL loader.
  ///
  /// FIXME: Fix SILModule's deserialization so that we can drop the local
  /// cache and use `SILModule::lookUpWitnessTable` directly.
  const std::unique_ptr<SerializedSILLoader> silLoader =
    SerializedSILLoader::create(astCtx, &module, nullptr);

  /// The VectorNumeric protocol in the standard library.
  ProtocolDecl *vectorNumericProtocol =
    astCtx.getProtocol(KnownProtocolKind::VectorNumeric);
  /// The Numeric protocol in the standard library.
  ProtocolDecl *numericProtocol =
    astCtx.getProtocol(KnownProtocolKind::Numeric);
  /// The FloatingPoint protocol in the stanard library.
  ProtocolDecl *floatingPointProtocol =
    astCtx.getProtocol(KnownProtocolKind::FloatingPoint);

  /// Flag indicating whether an error occurred.
  bool errorOccurred = false;

  /// `VectorNumeric.+` declaration.
  FuncDecl *cachedVectorPlusFn = nullptr;
  /// `Numeric.+` declaration.
  FuncDecl *cachedNumericPlusFn = nullptr;

public:
  /// Construct an ADContext for the given module.
  explicit ADContext(SILModule &module, SILPassManager &passManager);

  SILModule &getModule() const { return module; }
  ASTContext &getASTContext() const { return module.getASTContext(); }
  SILPassManager &getPassManager() const { return passManager; }
  Lowering::TypeConverter &getTypeConverter() { return module.Types; }

  ArrayRef<std::unique_ptr<DifferentiationTask>>
  getDifferentiationTasks() const {
    return differentiationTasks;
  }

  /// Finds a witness table for the specified conformance in the current module.
  /// If it doesn't exist, then tries to find it in all imported modules and
  /// links it to the current module. Returns null if no witness table can be
  /// found.
  SILWitnessTable *
  lookupOrLinkWitnessTable(ProtocolConformanceRef confRef) {
    auto *conf = confRef.getConcrete();
    if (auto existingTable = module.lookUpWitnessTable(confRef))
      return existingTable;
    auto *decl =
      conf->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
    auto linkage = getSILLinkage(getDeclLinkage(decl), NotForDefinition);
    auto *newTable = module.createWitnessTableDeclaration(conf, linkage);
    newTable = silLoader->lookupWitnessTable(newTable);
    // Update linkage for witness methods.
    // FIXME: Figure out why witnesses have shared linkage by default.
    for (auto &entry : newTable->getEntries())
      if (entry.getKind() == SILWitnessTable::WitnessKind::Method)
        entry.getMethodWitness().Witness->setLinkage(linkage);
    return newTable;
  }

  ProtocolDecl *getVectorNumericProtocol() const {
    return vectorNumericProtocol;
  }

  ProtocolDecl *getNumericProtocol() const {
    return numericProtocol;
  }

  ProtocolDecl *getFloatingPointProtocol() const {
    return floatingPointProtocol;
  }

  FuncDecl *getVectorPlusDecl() {
    if (cachedVectorPlusFn)
      return cachedVectorPlusFn;
    return cachedVectorPlusFn = findAssociativeOperatorDeclInProtocol(
      astCtx.getIdentifier("+"), vectorNumericProtocol);
  }

  FuncDecl *getNumericPlusDecl() {
    if (cachedNumericPlusFn)
      return cachedNumericPlusFn;
    return cachedNumericPlusFn = findAssociativeOperatorDeclInProtocol(
      astCtx.getIdentifier("+"), numericProtocol);
  }

  /// Determines whether the given type conforms to VectorNumeric while the
  /// ScalarElement associated type conforms to FloatingPoint.
  bool supportsVectorDifferentiation(Type type) const;

  /// Determines whether the given type conforms to FloatingPoint.
  bool supportsScalarDifferentiation(Type type) const;

  /// Retrieves the function dependent type container in the current Swift
  /// module. If it does not exist, create one.
  SourceFile &getFunctionDependentTypeContainer() {
    // FIXME: To handle this more properly, we should make a DerivedFileUnit
    // class to contain all synthesized implicit type declarations.
    for (auto *file : module.getSwiftModule()->getFiles())
      if (auto *src = dyn_cast<SourceFile>(file))
        return *src;
    llvm_unreachable("No files?");
  }

  /// Creates a function-dependent struct, which is associated uniquely with the
  /// specified function and has the same generic signature as the function.
  StructDecl *createDependentStructForFunction(SILFunction *function);

  void insertGradient(const GradientLookupKey &key, SILFunction *gradient) {
    gradientMap.insert({key, gradient});
  }

  SILFunction *lookupGradient(const GradientLookupKey &key) const {
    auto lookup = gradientMap.find(key);
    return lookup == gradientMap.end() ? nullptr : lookup->getSecond();
  }

  SILFunction *lookupCanonicalGradient(const DifferentiationTask *task) const {
    return lookupGradient({task->original, task->getMasterConfig()});
  }

  /// Finds the `[reverse_differentiable]` attribute on the specified original
  /// function corresponding to the specified parameter indices. Returns nullptr
  /// if it does not exist.
  ///
  /// TODO: Currently we are doing a O(n) lookup. This could be improved by
  /// hashing on SILFunction's side or maintaining a dictionary in ADContext.
  /// In any case, this is not performance-critical.
  SILReverseDifferentiableAttr *
  lookupReverseDifferentiableAttr(
      SILFunction *original, const SILReverseAutoDiffIndices &indices) const {
    for (auto *attr : original->getReverseDifferentiableAttrs())
      if (attr->getIndices() == indices)
        return attr;
    return nullptr;
  }

  SILReverseDifferentiableAttr *
  createReverseDifferentiableAttr(
      SILFunction *original, const SILReverseAutoDiffIndices &indices) const {
    assert(!lookupReverseDifferentiableAttr(original, indices));
    auto *attr =
      SILReverseDifferentiableAttr::create(getModule(), indices,
                                           /*primalName*/ StringRef(),
                                           /*adjointName*/ StringRef());
    original->addReverseDifferentiableAttr(attr);
    return attr;
  }

  /// Finds or creates a `[reverse_differentiable]` attribute on the specified
  /// original function corresponding to the specified parameter indices.
  SILReverseDifferentiableAttr *
  getOrCreateReverseDifferentiableAttr(
      SILFunction *original, const SILReverseAutoDiffIndices &indices) {
    if (auto *attr = lookupReverseDifferentiableAttr(original, indices))
      return attr;
    return createReverseDifferentiableAttr(original, indices);
  }

  /// Finds a differentiation task on a function such that the task produces
  /// adjoints for the specified indices.
  DifferentiationTask *
  lookupDifferentiationTask(SILFunction *original,
                            const SILReverseAutoDiffIndices &indices) {
    auto *attr = lookupReverseDifferentiableAttr(original, indices);
    if (!attr) return nullptr;
    auto existing = enqueuedTaskIndices.find({original, indices});
    if (existing == enqueuedTaskIndices.end()) return nullptr;
    return differentiationTasks[existing->getSecond()].get();
  }
  
  /// Finds a differentiation task on a function such that the task produces
  /// adjoints for the least number of parameters that is a superset of
  /// the parameter indices in `indices`.
  DifferentiationTask *
  lookupMinimalDifferentiationTask(SILFunction *original,
                                   const SILReverseAutoDiffIndices &indices) {
    const llvm::SmallBitVector *supersetParamIndices;
    const auto &indexSet = indices.parameters;
    for (auto *rda : original->getReverseDifferentiableAttrs())
      if (!indexSet.test(indexSet & rda->getIndices().parameters))
        supersetParamIndices = &rda->getIndices().parameters;
    auto existing = enqueuedTaskIndices.find(
      {original, {indices.source, *supersetParamIndices}});
    if (existing == enqueuedTaskIndices.end()) return nullptr;
    return differentiationTasks[existing->getSecond()].get();
  }

  /// Register a differentiation task in the global worklist. This will ensure
  /// that a `[reverse_differentiable]` attribute will be generated for the
  /// specified indices, and that primal/adjoint synthesis will be run in the
  /// Differentiation pass.
  DifferentiationTask *
  registerDifferentiationTask(SILFunction *original,
                              const SILReverseAutoDiffIndices &indices,
                              DifferentiationInvoker invoker) {
    auto *attr = getOrCreateReverseDifferentiableAttr(original, indices);
    std::unique_ptr<DifferentiationTask> task(
      new DifferentiationTask(original, std::move(attr), module, invoker));
    differentiationTasks.push_back(std::move(task));
    return differentiationTasks.back().get();
  }

  DifferentiationTask *
  lookUpOrRegisterDifferentiationTask(SILFunction *original,
                                      const SILReverseAutoDiffIndices &indices,
                                      DifferentiationInvoker invoker) {
    if (auto *existingTask = lookupDifferentiationTask(original, indices))
      return existingTask;
    return registerDifferentiationTask(original, indices, invoker);
  }

  template<typename...T, typename...U>
  InFlightDiagnostic diagnose(SourceLoc loc, Diag<T...> diag,
                              U &&...args) const {
    return getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  /// Given an instruction and a differentiation task associated with the
  /// parent function, emits a "not differentiable" error based on the task. If
  /// the task is indirect, emits notes all the way up to the outermost task,
  /// and emits an error at the outer task. Otherwise, emits an error directly.
  void emitNondifferentiabilityError(
    SILInstruction *inst, const DifferentiationTask *task,
    Diag<> noteAtInnermostNode = diag::autodiff_expression_is_not_differentiable
  );

  /// Given a value and a differentiation task associated with the parent
  /// function, emits a "not differentiable" error based on the task. If the
  /// task is indirect, emits notes all the way up to the outermost task, and
  /// emits an error at the outer task. Otherwise, emits an error directly.
  void emitNondifferentiabilityError(
    SILValue value, const DifferentiationTask *task,
    Diag<> noteAtInnermostNode = diag::autodiff_expression_is_not_differentiable
  );

  void setErrorOccurred() { errorOccurred = true; }
  bool hasErrorOccurred() const { return errorOccurred; }
};
} // end anonymous namespace

ADContext::ADContext(SILModule &module, SILPassManager &passManager)
  : module(module), passManager(passManager) {}

void ADContext::emitNondifferentiabilityError(SILValue value,
                                              const DifferentiationTask *task,
                                              Diag<> noteAtInnermostNode) {
  emitNondifferentiabilityError(value->getDefiningInstruction(), task,
                                noteAtInnermostNode);
}

void ADContext::emitNondifferentiabilityError(SILInstruction *inst,
                                              const DifferentiationTask *task,
                                              Diag<> noteAtInnermostNode) {
  SWIFT_DEFER { setErrorOccurred(); };
  // Location of the instruction.
  auto srcLoc = inst->getLoc().getSourceLoc();
  if (srcLoc.isInvalid()) srcLoc = SourceLoc();
  auto invoker = task->getInvoker();
  DEBUG(getADDebugStream() << "Diagnosing non-differentiability for value \n\t"
        << *inst << "\n" << "while performing differentiation task\n\t" << task
        << '\n');
  switch (invoker.getKind()) {
  // For a gradient instruction that is not associated with any source
  // location, we emit a diagnostic without source location.
  case DifferentiationInvoker::Kind::GradientInst:
    diagnose(srcLoc, diag::autodiff_function_not_differentiable);
    return;

  // For indirect differentiation, emit a "not differentiable" note on the
  // expression first. Then emit an error at the source invoker of
  // differentiation, and a "when differentiating this" note at each indirect
  // invoker.
  case DifferentiationInvoker::Kind::IndirectDifferentiation: {
    // Emit a default note at the innermost differentiation invoker.
    diagnose(srcLoc, noteAtInnermostNode);
    // Iteratively retrieve the outermost task, starting with the parent of the
    // current node, until the task is no longer indirect.
    auto *outerTask = invoker.getIndirectDifferentiation().second;
    while (outerTask->getInvoker().getKind() ==
             DifferentiationInvoker::Kind::IndirectDifferentiation) {
      std::tie(inst, outerTask) =
        outerTask->getInvoker().getIndirectDifferentiation();
      auto applyLoc = inst->getLoc().getSourceLoc();
      if (applyLoc.isValid())
        diagnose(applyLoc, diag::autodiff_when_differentiating_function_call);
    }
    // Now we've reached a direct task, recurse once to emit an error.
    emitNondifferentiabilityError(inst, outerTask);
    return;
  }

  // For a differential operator, emit a "not differentiable" note on the
  // expression first. Then emit an error at the differential operator.
  case DifferentiationInvoker::Kind::DifferentialOperator: {
    auto *expr = invoker.getDifferentialOperator();
    diagnose(srcLoc, noteAtInnermostNode);
    diagnose(expr->getLoc(),
      diag::autodiff_differential_operator_applied_to_nondifferentiable)
        .highlight(expr->getOriginalExpr()->getSourceRange());
    return;
  }

  // For a `@differentiable` attribute, emit a "not differentiable" note on the
  // expression first. Then emit an error at the `@differentiable` attribute.
  case DifferentiationInvoker::Kind::DifferentiableAttribute: {
    auto *attr = invoker.getDifferentiableAttribute();
    diagnose(srcLoc, noteAtInnermostNode);
    diagnose(attr->getLocation(),
      diag::autodiff_differentiable_attr_applied_to_nondifferentiable)
        .highlight(attr->getRangeWithAt());
    return;
  }
  }
}

/// Determines whether the type supports vector differentiation. We say that a
/// type supports vector differentiation if it conforms to `VectorNumeric` and
/// the associated type `ScalarElement` conforms to `FloatingPoint`.
bool ADContext::supportsVectorDifferentiation(Type type) const {
  auto *swiftModule = module.getSwiftModule();
  // Look up conformance.
  auto maybeConf = swiftModule->lookupConformance(type, vectorNumericProtocol);
  if (!maybeConf) return false;
  auto conf = *maybeConf;
  // See if the `ScalarElement` associated type conforms to `FloatingPoint`.
  DeclName scalarDeclName(getASTContext().getIdentifier("ScalarElement"));
  auto lookup = vectorNumericProtocol->lookupDirect(scalarDeclName);
  auto scalarAssocTy =
    cast<AssociatedTypeDecl>(lookup[0])->getDeclaredInterfaceType();
  auto scalarTy = conf.getAssociatedType(type, scalarAssocTy);
  auto scalarConf =
    swiftModule->lookupConformance(scalarTy, floatingPointProtocol);
  return scalarConf.hasValue();
}

/// Determines whether the type supports scalar differentiation. We say that a
/// type supports scalar differentiation if it conforms to `FloatingPoint` and
/// the associated type `ScalarElement` conforms to `FloatingPoint`.
bool ADContext::supportsScalarDifferentiation(Type type) const {
  auto *swiftModule = module.getSwiftModule();
  auto fpConf = swiftModule->lookupConformance(type, floatingPointProtocol);
  return fpConf.hasValue();
}

//===----------------------------------------------------------------------===//
// Control flow canonicalization
//===----------------------------------------------------------------------===//

namespace {
class ControlFlowCanonicalization {
private:
  SILFunction &function;
  SILBuilder builder = SILBuilder(function);
  DominanceInfo &domInfo;
  SILLoopInfo &loopInfo;

public:
  explicit ControlFlowCanonicalization(SILFunction &function,
                                       DominanceInfo &domInfo,
                                       SILLoopInfo &loopInfo)
    : function(function), domInfo(domInfo), loopInfo(loopInfo) {}

  /// Run control flow canonicalization on the function.
  bool run();
};
}

bool ControlFlowCanonicalization::run() {
  DEBUG(getADDebugStream() << "Running control flow canonicalization on "
        "function " << function.getName() << '\n');
  bool changed = false;
  assert(!function.isNoReturnFunction() && !function.isExternalDeclaration());
  assert(function.findReturnBB().getNodePtr());
  // Canonicalize loops.
  canonicalizeAllLoops(&domInfo, &loopInfo);
  // TODO: Handle multiple loop exits.
  return changed;
}

//===----------------------------------------------------------------------===//
// Activity Analysis
//===----------------------------------------------------------------------===//

namespace {
class DifferentiableActivityInfo;

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
/// Laurent Hascoët. Automatic Differentiation by Program Transformation. 2017.
class DifferentiableActivityAnalysis
  : public FunctionAnalysisBase<DifferentiableActivityInfo> {
private:
  DominanceAnalysis *dominanceAnalysis = nullptr;

public:
  explicit DifferentiableActivityAnalysis()
    : FunctionAnalysisBase(AnalysisKind::DifferentiableActivity) {}

  static bool classof(const SILAnalysis *s) {
    return s->getKind() == AnalysisKind::DifferentiableActivity;
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind k) override {
    return k & InvalidationKind::Everything;
  }

  virtual
  DifferentiableActivityInfo *newFunctionAnalysis(SILFunction *f) override;

  virtual void initialize(SILPassManager *pm) override;
};
} // end anonymous namespace

namespace {
/// Result of activity analysis on a function. Accepts queries for whether a
/// value is "varied", "useful" or "active" against certain differentiation
/// indices.
class DifferentiableActivityInfo {
private:
  SILFunction &function;

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

  /// Perform analysis and populate sets.
  void analyze();

public:
  explicit DifferentiableActivityInfo(SILFunction &f);

  bool isIndependent(SILValue value,
                     const SILReverseAutoDiffIndices &indices) const;
  bool isDependent(SILValue value,
                   const SILReverseAutoDiffIndices &indices) const;
  bool isVaried(SILValue value,
                unsigned independentVariableIndex) const;
  bool isUseful(SILValue value,
                unsigned dependentVariableIndex) const;
  bool isVaried(SILValue value,
                const llvm::SmallBitVector &parameterIndices) const;
  bool isActive(SILValue value,
                const SILReverseAutoDiffIndices &indices) const;
};
} // end anonymous namespace

DifferentiableActivityInfo *
DifferentiableActivityAnalysis::newFunctionAnalysis(SILFunction *f) {
  assert(dominanceAnalysis && "Expect a valid dominance anaysis");
  return new DifferentiableActivityInfo(*f);
}

void DifferentiableActivityAnalysis::initialize(SILPassManager *pm) {
  dominanceAnalysis = pm->getAnalysis<DominanceAnalysis>();
}

SILAnalysis *swift::createDifferentiableActivityAnalysis(SILModule *m) {
  return new DifferentiableActivityAnalysis();
}

DifferentiableActivityInfo::
DifferentiableActivityInfo(SILFunction &f) : function(f) {
  analyze();
}

/// Recursively find all "varied" values relative to the given value.
///
/// NOTE: The given value will **not** be considered varied.
static void collectVariedValues(SILValue value,
                                SmallDenseSet<SILValue> &variedValues,
                                unsigned inputIndex,
                                SmallDenseSet<SILValue> &visited) {
  auto insertion = visited.insert(value);
  if (!insertion.second) return;
  for (auto use : value->getUses()) {
    auto *inst = use->getUser();
    // If there's a `store` of this value, we consider the destination varied.
    if (auto *storeInst = dyn_cast<StoreInst>(inst)) {
      SILValue buffer = storeInst->getDest();
      // If the def is `begin_access`, then its operand is the actual buffer.
      if (auto *def =
            dyn_cast_or_null<BeginAccessInst>(buffer->getDefiningInstruction()))
        buffer = def->getOperand();
      DEBUG(getADDebugStream() << "VARIED @ " << inputIndex << ":\n"
            << buffer << '\n');
      variedValues.insert(buffer);
      visited.insert(buffer);
      collectVariedValues(buffer, variedValues, inputIndex, visited);
      continue;
    }
    // For other instructions, consider their results varied.
    for (auto val : inst->getResults()) {
      DEBUG(getADDebugStream() << "VARIED @ " << inputIndex << ":\n"
            << val << '\n');
      variedValues.insert(val);
      // Recursively collect.
      collectVariedValues(val, variedValues, inputIndex, visited);
    }
  }
}

/// Recursively find all "useful" values relative to the given value.
///
/// NOTE: The given value will be considered useful.
static void collectUsefulValues(SILValue value,
                                SmallDenseSet<SILValue> &usefulValues,
                                unsigned outputIndex) {
  DEBUG(getADDebugStream() << "USEFUL @ " << outputIndex << ":\n"
        << value << '\n');
  usefulValues.insert(value);
  if (auto *def = value->getDefiningInstruction())
    for (auto &op : def->getAllOperands())
      collectUsefulValues(op.get(), usefulValues, outputIndex);
}

void DifferentiableActivityInfo::analyze() {
  DEBUG(getADDebugStream() << "Running activity analysis on @"
        << function.getName() << '\n');
  // Inputs are just function's arguments, count `n`.
  auto paramArgs = function.getArgumentsWithoutIndirectResults();
  for (auto valueAndIndex : enumerate(paramArgs)) {
    inputValues.push_back(valueAndIndex.first);
  }
  DEBUG({
    auto &s = getADDebugStream();
    s << "Inputs in @" << function.getName() << ":\n";
    for (auto val : inputValues) s << val << '\n';
  });
  // Outputs are indirect result buffers and return values, count `m`.
  collectAllFormalResultsInTypeOrder(function, outputValues);
  DEBUG({
    auto &s = getADDebugStream();
    s << "Outputs in @" << function.getName() << ":\n";
    for (auto val : outputValues) s << val << '\n';
  });
  // Initialize sets to store useful values and varied values.
  usefulValueSets.append(outputValues.size(), {});
  variedValueSets.append(inputValues.size(), {});
  // Mark varied values for each independent varible.
  SmallDenseSet<SILValue> visitedVariedValues;
  for (auto valAndIdx : enumerate(inputValues))
    collectVariedValues(valAndIdx.first, variedValueSets[valAndIdx.second],
                        valAndIdx.second, visitedVariedValues);
  // Mark useful values for each dependent variable.
  for (auto valAndIdx : enumerate(outputValues))
    collectUsefulValues(valAndIdx.first, usefulValueSets[valAndIdx.second],
                        valAndIdx.second);
}

bool DifferentiableActivityInfo::
isIndependent(SILValue value, const SILReverseAutoDiffIndices &indices) const {
  for (auto paramIdx : indices.parameters.set_bits())
    if (inputValues[paramIdx] == value)
      return true;
  return false;
}

bool DifferentiableActivityInfo::
isDependent(SILValue value, const SILReverseAutoDiffIndices &indices) const {
  return inputValues[indices.source] == value;
}

bool DifferentiableActivityInfo::
isVaried(SILValue value, unsigned independentVariableIndex) const {
  auto &set = variedValueSets[independentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::
isVaried(SILValue value, const llvm::SmallBitVector &parameterIndices) const {
  for (auto paramIdx : parameterIndices.set_bits())
    if (!isVaried(value, paramIdx))
      return false;
  return true;
}

bool DifferentiableActivityInfo::
isUseful(SILValue value, unsigned dependentVariableIndex) const {
  auto &set = usefulValueSets[dependentVariableIndex];
  return set.count(value);
}

bool DifferentiableActivityInfo::
isActive(SILValue value, const SILReverseAutoDiffIndices &indices) const {
  return isVaried(value, indices.parameters) && isUseful(value, indices.source);
}

static void dumpActivityInfo(SILValue value,
                             const SILReverseAutoDiffIndices &indices,
                             DifferentiableActivityInfo &activityInfo,
                             llvm::raw_ostream &s = llvm::dbgs()) {
  s << '[';
  if (activityInfo.isActive(value, indices))
    s << "ACTIVE";
  else if (activityInfo.isVaried(value, indices.parameters))
    s << "VARIED";
  else if (activityInfo.isUseful(value, indices.source))
    s << "USEFUL";
  s << "] " << value;
}

static void dumpActivityInfo(SILFunction &fn,
                             const SILReverseAutoDiffIndices &indices,
                             DifferentiableActivityInfo &activityInfo,
                             llvm::raw_ostream &s = llvm::dbgs()) {
  s << "Activity info for " << fn.getName() << " at " << indices << '\n';
  for (auto &bb : fn) {
    for (auto *arg : bb.getArguments())
      dumpActivityInfo(arg, indices, activityInfo, s);
    for (auto &inst : bb)
      for (auto res : inst.getResults())
        dumpActivityInfo(res, indices, activityInfo, s);
  }
}

//===----------------------------------------------------------------------===//
// Code emission utilities
//===----------------------------------------------------------------------===//

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

/// When a function value is used in an instruciton (usually `apply`), there's
/// some conversion instruction in between, e.g. `thin_to_thick_function`. Given
/// a new function value and an old function value, this helper function
/// recursively converts the new function just like how the old function is
/// converted.
static SILValue reapplyFunctionConversion(
    SILValue newFunc, SILValue oldFunc, SILValue oldConvertedFunc,
    SILBuilder &builder, SILLocation loc,
    std::function<SILValue(SILValue)> substituteOperand
      = [](SILValue v) { return v; }) {
  // If the old func is the new func, then there's no conversion.
  if (oldFunc == oldConvertedFunc)
    return newFunc;
  // Handle a few instruction cases.
  // thin_to_thick_function
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(oldConvertedFunc)) {
    auto innerNewFunc = reapplyFunctionConversion(
      newFunc, oldFunc, tttfi->getOperand(), builder, loc, substituteOperand);
    auto operandFnTy = innerNewFunc->getType().castTo<SILFunctionType>();
    auto thickTy =
      operandFnTy->getWithRepresentation(SILFunctionTypeRepresentation::Thick);
    auto silTy = SILType::getPrimitiveObjectType(thickTy);

    return builder.createThinToThickFunction(loc, innerNewFunc, silTy);
  }
  // partial_apply
  if (auto *pai = dyn_cast<PartialApplyInst>(oldConvertedFunc)) {
    SmallVector<SILValue, 8> newArgs;
    newArgs.reserve(pai->getNumArguments());
    for (auto arg : pai->getArguments())
      newArgs.push_back(substituteOperand(arg));
    auto innerNewFunc = reapplyFunctionConversion(newFunc, oldFunc,
                                                  pai->getCallee(), builder,
                                                  loc, substituteOperand);
    return builder.createPartialApply(
      loc, innerNewFunc, pai->getSubstitutionMap(), newArgs,
      pai->getOrigCalleeType()->getCalleeConvention());
  }
  llvm_unreachable("Unhandled function convertion instruction");
}

/// Convert an integer literal to a type that is expressible by integer literal.
static
void convertIntToIndirectExpressible(intmax_t value,
                                     NominalTypeDecl *targetTypeDecl,
                                     SILValue resultBuf,
                                     SILLocation loc,
                                     SILBuilder &builder,
                                     ADContext &context) {
  auto &module = builder.getModule();
  auto &astCtx = module.getASTContext();
  auto targetTy =
    targetTypeDecl->getDeclaredInterfaceType()->getCanonicalType();
  // Step 1. Initialize a value of type `<target type>.IntegerLiteralType` from
  // the given value.
  DeclName intLitTypeName(astCtx.Id_IntegerLiteralType);
  SmallVector<ValueDecl *, 1> intLitTypeLookupResults;
  targetTypeDecl->lookupQualified(targetTy, intLitTypeName, NL_OnlyTypes,
                                  /*typeResolver*/ nullptr,
                                  intLitTypeLookupResults);
  assert(intLitTypeLookupResults.size() == 1);
  auto intLitTypeAliasDecl = cast<TypeAliasDecl>(intLitTypeLookupResults[0]);
  // Now we have the IntegerLiteralType type.
  auto intLitTy =
    intLitTypeAliasDecl->getUnderlyingTypeLoc().getType()->getCanonicalType();
  auto *intLitTypeDecl = intLitTy->getAnyNominal();
  assert(intLitTypeDecl);
  // %1 = integer_literal $Builtin.Int2048, <value>
  auto builtinIntTy = SILType::getBuiltinIntegerType(2048, astCtx);
  auto *builtinInt = builder.createIntegerLiteral(loc, builtinIntTy, value);
  // %2 = metatype $@thin <target type>.IntegerLiteralType.Type
  auto intLitMetatypeTy = SILType::getPrimitiveObjectType(
    CanMetatypeType::get(intLitTy, MetatypeRepresentation::Thick));
  auto *intLitMetatype = builder.createMetatype(loc, intLitMetatypeTy);
  // ExpressibleByBuiltinIntegerLiteral
  auto *ebilProto =
    astCtx.getProtocol(KnownProtocolKind::ExpressibleByBuiltinIntegerLiteral);
  // `init(_builtinIntegerLiteral:)`
  DeclName builtinLitInitName(astCtx, DeclBaseName::createConstructor(), {
    astCtx.getIdentifier("_builtinIntegerLiteral")
  });
  auto *initBILDecl =
    cast<ConstructorDecl>(ebilProto->lookupDirect(builtinLitInitName)[0]);
  SILDeclRef initBILDeclRef(initBILDecl);
  auto initBILType = context.getTypeConverter().getConstantType(initBILDeclRef);
  // Look up `IntegerLiteralType : _ExpressibleByBuiltinIntegerLiteral`. This is
  // guaranteed to be a normal conformance.
  auto *ebilConf = astCtx.getConformance(intLitTy, ebilProto,
                                         intLitTypeDecl->getLoc(),
                                         intLitTypeDecl,
                                         ProtocolConformanceState::Complete);
  ProtocolConformanceRef ebilConfRef(ebilConf);
  // Link witness table.
  context.lookupOrLinkWitnessTable(ebilConfRef);
  // %3 = witness_method ...
  auto initBILFn = builder.createWitnessMethod(loc, intLitTy, ebilConfRef,
                                               initBILDeclRef, initBILType);
  // Get substitutions.
  auto intLitSubMap =
    SubstitutionMap::getProtocolSubstitutions(ebilProto, intLitTy, ebilConfRef);
  // Allocate result buffer.
  // %intLitBuf = alloc_stack $IntegerLiteralType
  auto *intLitBuf =
    builder.createAllocStack(loc, SILType::getPrimitiveObjectType(intLitTy));
  SWIFT_DEFER {
    // dealloc_stack %intLitBuf : $*IntegerLiteralType
    builder.createDeallocStack(loc, intLitBuf);
  };
  // %4 = apply %3 <...>(%intLitBuf, %1, %2)
  builder.createApply(loc, initBILFn, intLitSubMap,
                      { intLitBuf, builtinInt, intLitMetatype },
                      /*isNonThrowing*/ false);

  // Step 2. Initialize a value of type `<target type>` by calling
  // %5 = metatype $@thin <target type>.IntegerLiteralType.Type
  auto targetMetatypeTy = SILType::getPrimitiveObjectType(
    CanMetatypeType::get(targetTy, MetatypeRepresentation::Thick));
  auto *targetMetatype = builder.createMetatype(loc, targetMetatypeTy);
  // `ExpressibleByIntegerLiteral.init(integerLiteral: %4)`.
  auto *eilProto =
    astCtx.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral);
  DeclName intLitInitName(astCtx, DeclBaseName::createConstructor(), {
    astCtx.getIdentifier("integerLiteral")
  });
  auto *initILDecl =
    cast<ConstructorDecl>(eilProto->lookupDirect(intLitInitName)[0]);
  SILDeclRef initILDeclRef(initILDecl);
  auto initILType = context.getTypeConverter().getConstantType(initILDeclRef);
  // Lookup `<target type> : ExpressibleByIntegerLiteral` (could be specialized
  // or inherited).
  auto *parentModule = targetTypeDecl->getModuleContext();
  auto eilConf = *parentModule->lookupConformance(targetTy, eilProto);
  ProtocolConformanceRef eilConfRef(eilConf);
  context.lookupOrLinkWitnessTable(eilConfRef);
  // %6 = witness_method ...
  auto initILFn = builder.createWitnessMethod(loc, targetTy, eilConfRef,
                                              initILDeclRef, initILType);
  // Get substitutions.
  auto targetSubMap =
    SubstitutionMap::getProtocolSubstitutions(eilProto, targetTy, eilConfRef);
  // %7 = apply %6 <...>(%resultBuf, %intLitBuf, %5)
  builder.createApply(loc, initILFn, targetSubMap,
                      { resultBuf, intLitBuf, targetMetatype },
                      /*isNonThrowing*/ false);
}

/// Create a seed value.
///
/// NOTE: This will be reduced to only support scalar AD when vector AD supports
/// optional seeds, because a vector of 1s as seed doesn't make mathematical
/// sense in vector AD.
static void convertToIndirectSeed(intmax_t value, CanType type,
                                  SILValue seedBuf, SILLocation loc,
                                  SILBuilder &builder, ADContext &context) {
  // See if the type is a builtin float. If so, we don't do protocol
  // conformance-based conversion.
  if (auto fpType = type->getAs<BuiltinFloatType>()) {
    auto one = builder.createFloatLiteral(
      loc, SILType::getPrimitiveObjectType(type),
      APFloat(fpType->getAPFloatSemantics(), value));
    auto access = builder.createBeginAccess(loc, seedBuf, SILAccessKind::Init,
                                            SILAccessEnforcement::Static,
                                            /*noNestedConflict*/ true,
                                            /*fromBuiltin*/ false);
    builder.createStore(loc, one, seedBuf,
                        getBufferSOQ(type, context.getModule()));
    builder.createEndAccess(loc, access, /*aborted*/ false);
    return;
  }
  
  auto *targetTypeDecl = type->getAnyNominal();
  assert(targetTypeDecl && "Target type must be a nominal type");
  auto &astCtx = context.getASTContext();
  auto &module = context.getModule();
  auto &typeConv = context.getTypeConverter();
  // If it's scalar differentiation, just convert the literal to the requested
  // type.
  if (context.supportsScalarDifferentiation(type)) {
    convertIntToIndirectExpressible(value, targetTypeDecl, seedBuf,
                                    loc, builder, context);
    return;
  }
  // Otherwise it must be vector differentiation, call
  // `VectorNumeric.init(_:)`.
  assert(context.supportsVectorDifferentiation(type));
  // Create a scalar value from the specified integer literal.
  DeclName scalarDeclName(astCtx.getIdentifier("ScalarElement"));
  auto currencyDeclLookupResult =
    targetTypeDecl->lookupDirect(scalarDeclName);
  auto *scalarElemAlias = cast<TypeAliasDecl>(currencyDeclLookupResult[0]);
  auto scalarTy =
    scalarElemAlias->getDeclaredInterfaceType()->getCanonicalType();
  auto currencySubMap =
    type->getMemberSubstitutionMap(module.getSwiftModule(), scalarElemAlias);
  scalarTy = scalarTy.subst(currencySubMap)->getCanonicalType();
  auto *scalarTyDecl = scalarTy.getAnyNominal();
  assert(scalarTyDecl && "ScalarElement must be a nominal type");
  // %0 = ... : $<scalar type>
  auto scalarBuf =
    builder.createAllocStack(loc, SILType::getPrimitiveObjectType(scalarTy));
  convertIntToIndirectExpressible(value, scalarTyDecl, scalarBuf,
                                  loc, builder, context);
  auto scalarLOQ = getBufferLOQ(scalarTy, module);
  auto scalarVal = builder.createLoad(loc, scalarBuf, scalarLOQ);
  // dealloc_stack %0 : $*<scalar type>
  builder.createDeallocStack(loc, scalarBuf);
  // %1 = metatype $<scalar type>.Type
  auto metatypeTy = SILType::getPrimitiveObjectType(
    CanMetatypeType::get(type, MetatypeRepresentation::Thick));
  auto *metatype = builder.createMetatype(loc, metatypeTy);
  // Call `init(_:)` through `VectorNumeric` protocol.
  DeclName initName(astCtx, DeclBaseName::createConstructor(), { Identifier() });
  // Allocate buffer for passing the indirect scalar value.
  // %2 = alloc_stack $<scalar type>
  auto scalarValBuf =
    builder.createAllocStack(loc, typeConv.getLoweredType(scalarTy));
  SWIFT_DEFER {
    // dealloc_stack %2 : $<scalar type>
    builder.createDeallocStack(loc, scalarValBuf);
  };
  auto *bufAccess = builder.createBeginAccess(loc, scalarValBuf,
                                              SILAccessKind::Init,
                                              SILAccessEnforcement::Static,
                                              /*noNestedConflict*/ true,
                                              /*fromBuiltin*/ false);
  // store %0 : $<scalar type> to $*<scalar type>
  builder.createStore(loc, scalarVal, scalarValBuf,
                      getBufferSOQ(scalarTy, module));
  builder.createEndAccess(loc, bufAccess, /*aborted*/ false);
  auto *vecNumProto = context.getVectorNumericProtocol();
  auto *reqr =
    cast<ConstructorDecl>(vecNumProto->lookupDirect(initName).front());
  SILDeclRef reqrRef(reqr, SILDeclRef::Kind::Allocator);
  auto silInitTy = context.getTypeConverter().getConstantType(reqrRef);
  // Get scalar's conformance to `FloatingPoint`.
  auto conf = astCtx.getConformance(type, vecNumProto,
                                    targetTypeDecl->getLoc(), targetTypeDecl,
                                    ProtocolConformanceState::Complete);
  ProtocolConformanceRef confRef(conf);
  // $4 = witness_method ...
  auto initFnRef =
    builder.createWitnessMethod(loc, type, confRef, reqrRef, silInitTy);
  auto initSubMap =
    SubstitutionMap::getProtocolSubstitutions(vecNumProto, type, confRef);
  // %5 = apply %4(%3, %2, %1)
  builder.createApply(loc, initFnRef, initSubMap,
                      { seedBuf, scalarValBuf, metatype },
                      /*isNonThrowing*/ false);
}

//===----------------------------------------------------------------------===//
// PrimalGen - generates primal functions for each differentiation task in
// the SIL module.
//===----------------------------------------------------------------------===//

namespace {
class PrimalGen {
  friend class PrimalGenCloner;
private:
  /// The global AD context.
  ADContext &context;

public:
  explicit PrimalGen(ADContext &context) : context(context) {}

};
} // end anonymous namespace


//===----------------------------------------------------------------------===//
// AdjointGen - generates an adjoint function for each differentiation task
// in a SIL module.
//===----------------------------------------------------------------------===//

/// The adjoint generator for all gradient functions. Executed after PrimalGen.
namespace {

class AdjointGen {
  friend class AdjointEmitter;
private:
  /// The global AD context.
  ADContext &context;

public:
  explicit AdjointGen(ADContext &context) : context(context) {}

};
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// Differentiation pass implementation
//===----------------------------------------------------------------------===//

/// Given a `gradient` instruction, find the corresponding differential operator
/// used in the AST. If no differential operator is found, return nullptr.
static ReverseAutoDiffExpr *findDifferentialOperator(GradientInst *inst) {
  return inst->getLoc().getAsASTNode<ReverseAutoDiffExpr>();
}

// Retrieve or create an empty gradient function based on a `gradient`
// instruction and replace all users of the `gradient` instruction with the
// gradient function. Returns the gradient function.
static SILFunction *lookupOrSynthesizeGradient(
  ADContext &context, GradientInst *gradInst, SILFunction *original) {
  auto &module = original->getModule();
  auto &astCtx = module.getASTContext();
  auto origTy = original->getLoweredFunctionType();
  auto config = gradInst->getConfig();

  // Creates a gradient function based on the configuration.
  auto createGradFunction = [&](const SILReverseAutoDiffConfig &config) {
    auto gradType = origTy->getGradientType(config, module);
    std::string gradName =
      original->getName().str() + "__" + mangleADConfig(config);
    auto gradNameId = astCtx.getIdentifier(gradName);
    auto *gradFn = module.createFunction(original->getLinkage(),
                                         gradNameId.str(), gradType,
                                         original->getGenericEnvironment(),
                                         original->getLocation(),
                                         original->isBare(),
                                         original->isTransparent(),
                                         original->isSerialized());
    gradFn->setDebugScope(
      new (module) SILDebugScope(original->getLocation(), gradFn));
    return gradFn;
  };

  // Find the canonical gradient.
  SILFunction *canonicalGrad = nullptr;
  // The master AD config corresponds to the canonical gradient.
  auto masterConfig = config.getWithCanonicalOptions();
  // If the canonical gradient already exists, we'll simply use it. No
  // differentiation is needed.
  if (auto *existingGrad = context.lookupGradient({original, masterConfig}))
    canonicalGrad = existingGrad;
  // Otherwise, create a canonical gradient and enqueue a differentiation task.
  else {
    // Create a canonical gradient.
    canonicalGrad = createGradFunction(masterConfig);
    context.insertGradient({original, masterConfig}, canonicalGrad);
    // Enqueue a new differentiation task in the global context.
    if (auto *diffOp = findDifferentialOperator(gradInst))
      context.registerDifferentiationTask(original, config.indices, diffOp);
    else
      context.registerDifferentiationTask(original, config.indices, gradInst);
  }

  // If the requested gradient is not *both seedable and result-preserving*,
  // emit wrapper function, emit a call to the canonical gradient function
  // inside, and cache it. Otherwise, it's just the canonical gradient.
  SILFunction *gradFn = nullptr;
  if (config.isMaster())
    gradFn = canonicalGrad;
  else if (auto *existingGradFn = context.lookupGradient({original, config}))
    gradFn = existingGradFn;
  else {
    gradFn = createGradFunction(config);
    // Create entry basic block.
    auto *entry = gradFn->createBasicBlock();
    createEntryArguments(gradFn);
    // Build a call to the canonical gradient function.
    SILBuilder builder(entry);
    auto loc = gradFn->getLocation();
    SILFunctionConventions gradConv(gradFn->getLoweredFunctionType(), module),
                           origConv(origTy, module),
                           canGradConv(canonicalGrad->getLoweredFunctionType(),
                                       module);
    SmallVector<SILValue, 8> args;
    SmallVector<SILValue, 1> stackAllocsToCleanUp;
    // Prepare arguments.
    // The first few arguments are the original arguments.
    for (auto arg : gradFn->getArguments())
      args.push_back(arg);
    // If it's not seedable, we need to create a default seed.
    if (!config.isSeedable()) {
      auto seedTy = origTy->getSingleResult().getType();
      auto seedSILTy = SILType::getPrimitiveObjectType(seedTy);
      // Call `<seed type>.init(1)` to create a default
      // seed to feed into the canonical gradient.
      auto *seedBuf = builder.createAllocStack(loc, seedSILTy);
      convertToIndirectSeed(1, seedTy, seedBuf, loc, builder, context);
      // If seed is address only, we'll clean up the buffer after calling the
      // canonical gradient Otherwise, we just load the seed and deallocate the
      // buffer.
      if (seedSILTy.isAddressOnly(module)) {
        stackAllocsToCleanUp.push_back(seedBuf);
      } else {
        auto loq = seedSILTy.isTrivial(module)
          ? LoadOwnershipQualifier::Trivial : LoadOwnershipQualifier::Take;
        auto seedBufAccess = builder.createBeginAccess(
            loc, seedBuf, SILAccessKind::Read, SILAccessEnforcement::Static,
            /*noNestedConflict*/ false, /*fromBuiltin=*/ false);
        auto seed = builder.createLoad(loc, seedBufAccess, loq);
        builder.createEndAccess(loc, seedBufAccess, /*aborted*/ false);
        args.push_back(seed);
        builder.createDeallocStack(loc, seedBuf);
      }
    }
    // Call the canonical gradient function.
    // %0 = function_ref ...
    auto *canGradFnRef = builder.createFunctionRef(loc, canonicalGrad);
    SubstitutionMap subMap;
    if (auto *genEnv = gradFn->getGenericEnvironment())
      subMap = genEnv->getForwardingSubstitutionMap();
    // %1 = apply %0(...)
    auto *resultAndGrad = builder.createApply(loc, canGradFnRef, subMap,
                                              args, /*isNonThrowing*/ false);
    // Clean up stack allocations made by seed passing when seed is addr-only.
    for (auto alloc : stackAllocsToCleanUp)
      builder.createDeallocStack(loc, alloc);
    // If the config is result-preserving, or if all original results are
    // indirect, we can just return whatever direct results the canonical
    // gradient produces.
    if (config.isPreservingResult() || origConv.getNumDirectSILResults() == 0) {
      builder.createReturn(loc, resultAndGrad);
    }
    // Otherwise, return every tuple element of `resultAndGrad` except the
    // first. For this, we have to build a bunch of `tuple_extract`s and
    // re-gather them using `tuple`.
    else {
      unsigned numDirResults = canGradConv.getNumDirectSILResults();
      SILValue result;
      if (numDirResults == 2)
        result = builder.createTupleExtract(loc, resultAndGrad, 1);
      else {
        SmallVector<SILValue, 8> grads;
        for (unsigned i : range(1, numDirResults))
          grads.push_back(builder.createTupleExtract(loc, resultAndGrad, i));
        result = builder.createTuple(loc, gradConv.getSILResultType(), grads);
      }
      builder.createReturn(loc, result);
    }

    // Cache the newly created gradient wrapper (non-canonical).
    context.insertGradient({original, config}, gradFn);
  }

  return gradFn;
}

/// Finish the canonical gradient function.
///
/// For the following original function type:
///   (a1, a2, ..., an) -> r
///
/// The canonical gradient has the following type:
///   (a1, a2, ..., an, seed) -> (r, a1, a0, ..., an)
///
/// In the canonical gradient function, we simply call the primal and the
/// adjoint, and return a tuple of the original result and the gradient values.
static void fillCanonicalGradient(SILFunction &canGrad,
                                  const DifferentiationTask *task,
                                  ADContext &context) {
  assert(canGrad.empty() && "The gradient function must be empty");
  auto &module = context.getModule();
  auto canGradTy = canGrad.getLoweredFunctionType();
  auto loc = canGrad.getLocation();
  auto *primal = task->getPrimal();
  assert(primal && "Primal does not exist?");
  auto primalTy = primal->getLoweredFunctionType();
  auto *adjoint = task->getAdjoint();
  assert(adjoint && "Adjoint does not exist?");
  auto adjointTy = adjoint->getLoweredFunctionType();
  SILFunctionConventions primalConv(primalTy, module),
                         adjointConv(adjointTy, module),
                         canGradConv(canGradTy, module);
  // Create an entry basic block.
  auto *entry = canGrad.createBasicBlock();
  createEntryArguments(&canGrad);
  // Initialize arguments.
  SILBuilder builder(entry);
  // Call primal with original arguments.
  SmallVector<SILValue, 8> stackAllocsToCleanUp;
  SmallVector<SILValue, 8> primalArgs;
  // Add indirect results.
  for (auto indResInfo : primalTy->getIndirectFormalResults()) {
    auto objTy = SILType::getPrimitiveObjectType(indResInfo.getType());
    auto resultBuf = builder.createAllocStack(loc, objTy);
    stackAllocsToCleanUp.push_back(resultBuf);
    primalArgs.push_back(resultBuf);
  }
  // Add original parameters. These are the canonical gradient's parameter
  // arguments except the seed, which is the last argument.
  for (auto arg : canGrad.getArgumentsWithoutIndirectResults().drop_back())
    primalArgs.push_back(arg);
  // %0 = function_ref @primal
  auto *primalRef = builder.createFunctionRef(loc, primal);
  // %1 = apply %0(...)
  auto *primalApply = builder.createApply(
    loc, primalRef, canGrad.getForwardingSubstitutionMap(),
    primalArgs, /*isNonThrowing*/ false);
  // Collect the primal's direct results.
  SmallVector<SILValue, 8> primalResults;
  if (primalConv.getNumDirectSILResults() == 1)
    primalResults.push_back(primalApply);
  else {
    auto tupleSILTy = primalConv.getSILResultType();
    for (unsigned i : range(primalConv.getNumDirectSILResults())) {
      auto val = builder.createTupleExtract(loc, primalApply, i,
                                            tupleSILTy.getTupleElementType(i));
      primalResults.push_back(val);
    }
  }
  // Call adjoint with original arguments, the checkpoints value and the seed.
  SmallVector<SILValue, 8> adjointArgs;
  // Add indirect results and original parameters. These are the canonical
  // gradient's arguments except the seed, which is the last argument.
  for (auto arg : canGrad.getArguments().drop_back())
    adjointArgs.push_back(arg);
  // Add primal values and the original result (all returned by primal).
  unsigned indResIdx = 0, dirResIdx = 0;
  for (auto &resInfo : primalConv.getResults())
    adjointArgs.push_back(resInfo.isFormalDirect()
      ? primalResults[dirResIdx++] : primalArgs[indResIdx++]);
  // Add seed.
  adjointArgs.push_back(canGrad.getArguments().back());
  // %2 = function_ref @adjoint
  auto *adjRef = builder.createFunctionRef(loc, adjoint);
  // %3 = apply %2(...)
  auto *adjApply = builder.createApply(loc, adjRef,
                                       canGrad.getForwardingSubstitutionMap(),
                                       adjointArgs, /*isNonThrowing*/ false);
  // Clean up stack allocations.
  for (auto val : reversed(stackAllocsToCleanUp))
    builder.createDeallocStack(loc, val);
  // Return the original result and the adjoint result as a tuple. If either one
  // of the primal or the adjoint returns a tuple, join them in a flat tuple.
  SmallVector<SILValue, 8> directResults;
  // If the original result is a direct return, add it to the direct return list
  // of the canonical gradient.
  if (primalConv.getResults().back().isFormalDirect())
    directResults.push_back(*primalResults.rbegin());
  // Add the adjoint's results to the direct return list.
  if (adjointConv.getNumDirectSILResults() == 1)
    directResults.push_back(adjApply);
  else {
    auto tupleSILTy = adjApply->getType();
    for (unsigned i : range(adjointConv.getNumDirectSILResults())) {
      auto val = builder.createTupleExtract(loc, adjApply, i,
                                            tupleSILTy.getTupleElementType(i));
      directResults.push_back(val);
    }
  }
  // Return these results as a tuple.
  auto tupleRet =
    builder.createTuple(loc, canGradConv.getSILResultType(), directResults);
  builder.createReturn(loc, tupleRet);
}

/// The automatic differentiation pass.
namespace {
class Differentiation : public SILModuleTransform {
public:
  Differentiation() : SILModuleTransform() {}
  void run() override;
private:
  void processGradientInst(GradientInst *gi, ADContext &context);
};
} // end anonymous namespace

void Differentiation::processGradientInst(GradientInst *gi,
                                          ADContext &context) {
  SILFunction *parent = gi->getFunction();
  auto operand = gi->getOperand(0);
  SILFunction *gradFn = nullptr;
  // If it traces back to a `function_ref`, differentiate that.
  if (auto *originalFRI = findReferenceToVisibleFunction(operand)) {
    auto *original = originalFRI->getReferencedFunction();
    gradFn = lookupOrSynthesizeGradient(context, gi, original);
    
    // If `gradFn` is still null, errors must have occurred.
    if (!gradFn) return;
    
    // Replace the `gradient` instruction with the reference to the specified
    // function.
    SILBuilder builder(gi);
    auto loc = parent->getLocation();
    SILValue gradRef = builder.createFunctionRef(loc, gradFn);
    // Traverse from the `gradient` instruction to the original
    // `function_ref`. If there's any function convertion, do the same
    // convertion for the gradient.
    auto convertedGradFn = reapplyFunctionConversion(gradRef, originalFRI,
                                                     gi->getOriginal(),
                                                     builder, loc);
    // Replace uses of the `gradient` instruction with the converted (if
    // necessary) gradient function value.
    gi->replaceAllUsesWith(convertedGradFn);
  }
  // Differentiating opaque functions is not supported yet.
  else {
    if (auto loc = gi->getLoc()) {
      auto *expr = loc.castToASTNode<ReverseAutoDiffExpr>();
      context.diagnose(expr->getOriginalExpr()->getLoc(),
                       diag::autodiff_opaque_function_unsupported);
    }
    context.setErrorOccurred();
    return;
  }
  // We invalidate analyses on the parent function because the `gradient`
  // instruction is transformed.
  PM->invalidateAnalysis(parent, SILAnalysis::InvalidationKind::FunctionBody);
}

/// AD pass entry.
void Differentiation::run() {
  auto &module = *getModule();

  // Collect gradient instructions to process.
  SmallVector<GradientInst *, 16> gradInsts;
  // Handle each `gradient` instruction in the module.
  for (SILFunction &f : module)
    for (SILBasicBlock &bb : f)
      for (SILInstruction &i : bb)
        if (auto *gi = dyn_cast<GradientInst>(&i))
          gradInsts.push_back(gi);

  // If there's no `gradient` instruction, there's no AD to do.
  if (gradInsts.empty()) return;

  // AD relies on stdlib (the Swift module). If it's not imported, it's an
  // internal error.
  if (!module.getSwiftModule()->getASTContext().getStdlibModule()) {
    llvm::errs() <<
      "Internal error: AD depends on the Swift module but it's not imported.\n";
    return;
  }

  // A global differentiation context.
  ADContext context(module, *PM);

  // Lower each gradient instruction to a function reference and replaces its
  // uses with a function reference to its gradient.
  //
  // If the operand to the instruction traces back to a function reference that
  // the compiler can see into, then we do further processing, i.e. retrieving
  // or creating its gradient. Otherwise, it's differentiating an opaque
  // function whose body isn't visible to the compiler. We don't have
  // infrastructure support for this yet and currently it'll error out, but
  // we'll look into adding a new function convention so that the primal and the
  // adjoint can be passed along with the function.
  for (auto *gi : gradInsts)
    processGradientInst(gi, context);

  // TODO: Run primal generation.
  PrimalGen primalGen(context);
  // primalGen.run();

  // If there were any error, back out.
  if (context.hasErrorOccurred())
    return;

  // TODO: Run adjoint generation.
  AdjointGen adjointGen(context);
  // adjointGen.run();

  // If there were any error, back out.
  if (context.hasErrorOccurred())
    return;

  // Fill the body of each empty canonical gradient function corresponding to
  // each differentiation task.
  for (auto &task : context.getDifferentiationTasks()) {
    auto *canGradFn = context.lookupCanonicalGradient(task.get());
    assert(canGradFn && "Cannot find the canonical gradient function");
    fillCanonicalGradient(*canGradFn, task.get(), context);
  }

  // Remove all remaining `gradient` instructions.
  for (auto *gi : gradInsts)
    recursivelyDeleteTriviallyDeadInstructions(gi);
}

//===----------------------------------------------------------------------===//
// Pass creation
//===----------------------------------------------------------------------===//

SILTransform *swift::createDifferentiation() {
  return new Differentiation;
}
