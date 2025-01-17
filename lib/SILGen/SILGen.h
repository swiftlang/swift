//===--- SILGen.h - Implements Lowering of ASTs -> SIL ----------*- C++ -*-===//
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

#ifndef SILGEN_H
#define SILGEN_H

#include "ASTVisitor.h"
#include "Cleanup.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseMap.h"
#include <deque>

namespace swift {
  class SILBasicBlock;
  class ForeignAsyncConvention;
  class SymbolSource;

namespace Lowering {
  class TypeConverter;
  class SILGenFunction;
  class CalleeTypeInfo;

/// An enum to indicate whether a protocol method requirement is satisfied by
/// a free function, as for an operator requirement.
enum IsFreeFunctionWitness_t : bool {
  IsNotFreeFunctionWitness = false,
  IsFreeFunctionWitness = true,
};

/// An ASTVisitor for generating SIL from top-level declarations in a module.
class LLVM_LIBRARY_VISIBILITY SILGenModule : public ASTVisitor<SILGenModule> {
public:
  /// The Module being constructed.
  SILModule &M;
  
  /// The type converter for the module.
  TypeConverter &Types;
  
  /// The Swift module we are visiting.
  ModuleDecl *SwiftModule;

  /// Mapping from SILDeclRefs to emitted SILFunctions.
  llvm::DenseMap<SILDeclRef, SILFunction*> emittedFunctions;
  /// Mapping from ProtocolConformances to emitted SILWitnessTables.
  llvm::DenseMap<NormalProtocolConformance*, SILWitnessTable*> emittedWitnessTables;

  /// Mapping from SILDeclRefs to where the given function will be inserted
  /// when it's emitted. Used for non-externally visible symbols.
  llvm::DenseMap<SILDeclRef, SILDeclRef> delayedFunctions;

  /// Queue of delayed SILFunctions that need to be forced.
  std::deque<SILDeclRef> pendingForcedFunctions;

  /// Delayed SILFunctions that need to be forced.
  llvm::DenseSet<SILDeclRef> forcedFunctions;

  /// Mapping global VarDecls to their onceToken and onceFunc, respectively.
  llvm::DenseMap<VarDecl *, std::pair<SILGlobalVariable *,
                                      SILFunction *>> delayedGlobals;

  /// Bookkeeping to ensure that useConformancesFrom{ObjectiveC,}Type() is
  /// only called once for each unique type, as an optimization.
  llvm::DenseSet<TypeBase *> usedConformancesFromTypes;
  llvm::DenseSet<TypeBase *> usedConformancesFromObjectiveCTypes;

  /// Queue of delayed conformances that need to be emitted.
  std::deque<NormalProtocolConformance *> pendingConformances;

  /// Set of delayed conformances that have already been forced.
  llvm::DenseSet<NormalProtocolConformance *> forcedConformances;

  size_t anonymousSymbolCounter = 0;

  std::optional<SILDeclRef> StringToNSStringFn;
  std::optional<SILDeclRef> NSStringToStringFn;
  std::optional<SILDeclRef> ArrayToNSArrayFn;
  std::optional<SILDeclRef> NSArrayToArrayFn;
  std::optional<SILDeclRef> DictionaryToNSDictionaryFn;
  std::optional<SILDeclRef> NSDictionaryToDictionaryFn;
  std::optional<SILDeclRef> SetToNSSetFn;
  std::optional<SILDeclRef> NSSetToSetFn;
  std::optional<SILDeclRef> BoolToObjCBoolFn;
  std::optional<SILDeclRef> ObjCBoolToBoolFn;
  std::optional<SILDeclRef> BoolToDarwinBooleanFn;
  std::optional<SILDeclRef> DarwinBooleanToBoolFn;
  std::optional<SILDeclRef> NSErrorToErrorFn;
  std::optional<SILDeclRef> ErrorToNSErrorFn;
  std::optional<SILDeclRef> BoolToWindowsBoolFn;
  std::optional<SILDeclRef> WindowsBoolToBoolFn;

  std::optional<ProtocolDecl *> PointerProtocol;

  std::optional<ProtocolDecl *> ObjectiveCBridgeable;
  std::optional<FuncDecl *> BridgeToObjectiveCRequirement;
  std::optional<FuncDecl *> UnconditionallyBridgeFromObjectiveCRequirement;
  std::optional<AssociatedTypeDecl *> BridgedObjectiveCType;

  std::optional<ProtocolDecl *> BridgedStoredNSError;
  std::optional<VarDecl *> NSErrorRequirement;

  std::optional<ProtocolConformance *> NSErrorConformanceToError;

public:
  SILGenModule(SILModule &M, ModuleDecl *SM);

  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;

  ASTContext &getASTContext() { return M.getASTContext(); }

  /// Main entry point.
  void emitSourceFile(SourceFile *sf);

  /// Lazy entry point for Feature::LazyImmediate.
  void emitSymbolSource(SymbolSource Source);

  llvm::StringMap<std::pair<std::string, /*isWinner=*/bool>> FileIDsByFilePath;

  static DeclName getMagicFunctionName(SILDeclRef ref);
  static DeclName getMagicFunctionName(DeclContext *dc);
  
  /// Get the function for a SILDeclRef, or return nullptr if it hasn't been
  /// emitted yet.
  SILFunction *getEmittedFunction(SILDeclRef constant,
                                  ForDefinition_t forDefinition);

  /// Get the function for a SILDeclRef, creating it if necessary.
  SILFunction *getFunction(SILDeclRef constant,
                           ForDefinition_t forDefinition);

  /// Get the dynamic dispatch thunk for a SILDeclRef.
  SILFunction *getDynamicThunk(SILDeclRef constant,
                               CanSILFunctionType constantTy);

  /// Emit a vtable thunk for a derived method if its natural abstraction level
  /// diverges from the overridden base method. If no thunking is needed,
  /// returns a static reference to the derived method.
  std::optional<SILVTable::Entry>
  emitVTableMethod(ClassDecl *theClass, SILDeclRef derived, SILDeclRef base);

  /// True if a function has been emitted for a given SILDeclRef.
  bool hasFunction(SILDeclRef constant);

  /// Get or create the declaration of a reabstraction thunk with the
  /// given signature.
  SILFunction *getOrCreateReabstractionThunk(
                                           CanSILFunctionType thunkType,
                                           CanSILFunctionType fromType,
                                           CanSILFunctionType toType,
                                           CanType dynamicSelfType,
                                           CanType fromGlobalActor);
  
  /// Get or create the declaration of a completion handler block
  /// implementation function for an ObjC API that was imported
  /// as `async` in Swift.
  SILFunction *getOrCreateForeignAsyncCompletionHandlerImplFunction(
      CanSILFunctionType blockType, CanType blockStorageType,
      CanType continuationType, AbstractionPattern origFormalType,
      CanGenericSignature sig, CalleeTypeInfo &calleeInfo);

  /// Determine whether the given class has any instance variables that
  /// need to be destroyed.
  bool hasNonTrivialIVars(ClassDecl *cd);

  /// Given an original function and a user-specified custom derivative
  /// function, get or create a derivative thunk with the expected derivative
  /// function type computed from the original function.
  ///
  /// To achieve the expected derivative type, the thunk may perform
  /// self-reordering, reabstraction, or both.
  ///
  /// Self-reordering is done for canonicalizing the types of derivative
  /// functions for instance methods wrt `self`. We want users to define
  /// derivatives with the following AST function types:
  ///
  /// JVP:
  /// - Takes `Self` as first parameter.
  /// - Returns differential taking `Self.Tan` as first parameter.
  ///
  ///     (Self) -> (T, ...) -> (R, (Self.Tan, T.Tan, ...) -> R.Tan)
  ///
  /// VJP:
  /// - Takes `Self` as first parameter.
  /// - Returns pullback returning `Self.Tan` as first result.
  ///
  ///     (Self) -> (T, ...) -> (R, (R.Tan) -> (Self.Tan, T.Tan, ...))
  ///
  /// However, the curried `Self` parameter in the AST JVP/VJP function types
  /// becomes the *last* parameter in the flattened parameter list of their
  /// lowered SIL function types.
  ///
  /// JVP:
  /// - Takes `Self` as *last* parameter.
  /// - Returns differential taking `Self.Tan` as *first* parameter.
  ///
  ///     $(T, ..., Self) -> (R, (Self.Tan, T.Tan, ...) -> R.Tan)
  ///
  /// VJP:
  /// - Takes `Self` as *last* parameter.
  /// - Returns pullback returning `Self.Tan` as *first* result.
  ///
  ///     $(T, ..., Self) -> (R, (R.Tan) -> (Self.Tan, T.Tan, ...))
  ///
  /// This leads to a parameter ordering inconsistency, and would require the
  /// differentiation transform to handle "wrt `self` instance method
  /// derivatives" specially. However, canonicalization during SILGen makes the
  /// parameter ordering uniform for "instance method derivatives wrt self" and
  /// simplifies the transform rules.
  ///
  /// If `self` must be reordered, reorder it so that it appears as:
  /// - The last parameter in the returned differential.
  /// - The last result in the returned pullback.
  SILFunction *getOrCreateCustomDerivativeThunk(
      AbstractFunctionDecl *originalAFD, SILFunction *originalFn,
      SILFunction *customDerivativeFn, const AutoDiffConfig &config,
      AutoDiffDerivativeFunctionKind kind);

  /// Get or create a derivative function vtable entry thunk for the given
  /// SILDeclRef and derivative function type.
  SILFunction *getOrCreateDerivativeVTableThunk(
      SILDeclRef derivativeFnRef, CanSILFunctionType derivativeFnTy);

  /// Determine whether we need to emit an ivar destroyer for the given class.
  /// An ivar destroyer is needed if a superclass of this class may define a
  /// failing designated initializer.
  bool requiresIVarDestroyer(ClassDecl *cd);

  //===--------------------------------------------------------------------===//
  // Visitors for top-level forms
  //===--------------------------------------------------------------------===//

  /// Returns true if SILGen should be skipped for the given decl.
  bool shouldSkipDecl(Decl *d);

  void visit(Decl *D);

  // These are either not allowed at global scope or don't require
  // code emission.
  void visitImportDecl(ImportDecl *d) {}
  void visitEnumCaseDecl(EnumCaseDecl *d) {}
  void visitEnumElementDecl(EnumElementDecl *d) {}
  void visitOperatorDecl(OperatorDecl *d) {}
  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *d) {}
  void visitTypeAliasDecl(TypeAliasDecl *d) {}
  void visitOpaqueTypeDecl(OpaqueTypeDecl *d) {}
  void visitGenericTypeParamDecl(GenericTypeParamDecl *d) {}
  void visitAssociatedTypeDecl(AssociatedTypeDecl *d) {}
  void visitConstructorDecl(ConstructorDecl *d) {}
  void visitDestructorDecl(DestructorDecl *d) {}
  void visitModuleDecl(ModuleDecl *d) { }
  void visitMissingMemberDecl(MissingMemberDecl *d) {}

  // Emitted as part of its storage.
  void visitAccessorDecl(AccessorDecl *ad) {}

  void visitFuncDecl(FuncDecl *fd);
  void visitPatternBindingDecl(PatternBindingDecl *vd);
  void visitTopLevelCodeDecl(TopLevelCodeDecl *td) {}
  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD);
  void visitNominalTypeDecl(NominalTypeDecl *ntd);
  void visitExtensionDecl(ExtensionDecl *ed);
  void visitVarDecl(VarDecl *vd);
  void visitSubscriptDecl(SubscriptDecl *sd);
  void visitMissingDecl(MissingDecl *d);
  void visitMacroDecl(MacroDecl *d);
  void visitMacroExpansionDecl(MacroExpansionDecl *d);

  // Same as AbstractStorageDecl::visitEmittedAccessors, but skips over skipped
  // (unavailable) decls.
  void visitEmittedAccessors(AbstractStorageDecl *D,
                             llvm::function_ref<void(AccessorDecl *)>);

  void emitEntryPoint(SourceFile *SF);
  void emitEntryPoint(SourceFile *SF, SILFunction *TopLevel);

  void emitAbstractFuncDecl(AbstractFunctionDecl *AFD);

  /// Generates code for the given FuncDecl and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  void emitFunction(FuncDecl *fd);

  /// Emits the function definition for a given SILDeclRef.
  void emitFunctionDefinition(SILDeclRef constant, SILFunction *f);

  /// Emit a function now, if it's externally usable or has been referenced in
  /// the current TU, or remember how to emit it later if not.
  void emitOrDelayFunction(SILDeclRef constant);

  /// Generates code for the given closure expression and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(ce).
  SILFunction *emitClosure(AbstractClosureExpr *ce,
                           const FunctionTypeInfo &closureInfo);

  /// Generates code for the given ConstructorDecl and adds
  /// the SILFunction to the current SILModule under the name SILDeclRef(decl).
  void emitConstructor(ConstructorDecl *decl);

  /// Generates code for the given class's destructor and adds
  /// the SILFunction to the current SILModule under the name
  /// SILDeclRef(cd, Destructor).
  ///
  /// TODO: Rename to emitClassDestructor.
  void emitDestructor(ClassDecl *cd, DestructorDecl *dd);

  /// Generates code for the given move only nominal type's destructor and adds
  /// the SILFunction to the current SILModule under the name SILDeclRef(nd,
  /// dd).
  void emitMoveOnlyDestructor(NominalTypeDecl *nd, DestructorDecl *dd);

  /// Emits the default argument generator with the given expression.
  void emitDefaultArgGenerator(SILDeclRef constant, ParamDecl *param);

  /// Emits the stored property initializer for the given pattern.
  void emitStoredPropertyInitialization(PatternBindingDecl *pd, unsigned i);

  /// Emits the backing initializer for a property with an attached wrapper.
  void emitPropertyWrapperBackingInitializer(VarDecl *var);

  /// Emits argument generators, including default argument generators and
  /// property wrapper argument generators, for the given parameter list.
  void emitArgumentGenerators(SILDeclRef::Loc decl, ParameterList *paramList);
  
  /// Emits a thunk from a foreign function to the native Swift convention.
  void emitForeignToNativeThunk(SILDeclRef thunk);

  /// Emits a thunk from a Swift function to the native Swift convention.
  void emitNativeToForeignThunk(SILDeclRef thunk);

  /// Emits the distributed actor thunk for the decl if there is one associated
  /// with it.
  void emitDistributedThunkForDecl(AbstractFunctionDecl * afd);

  /// Returns true if the given declaration must be referenced through a
  /// back deployment thunk in a context with the given resilience expansion.
  bool requiresBackDeploymentThunk(ValueDecl *decl,
                                   ResilienceExpansion expansion);

  /// Emits a thunk that calls either the original function if it is available
  /// or otherwise calls a fallback variant of the function that was emitted
  /// into the client module.
  void emitBackDeploymentThunk(SILDeclRef thunk);

  void preEmitFunction(SILDeclRef constant, SILFunction *F, SILLocation L);
  void postEmitFunction(SILDeclRef constant, SILFunction *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);

  /// Emit the ObjC-compatible entry point for a method.
  void emitObjCMethodThunk(FuncDecl *method);
  
  /// Emit the ObjC-compatible getter and setter for a property.
  void emitObjCPropertyMethodThunks(AbstractStorageDecl *prop);

  /// Emit the ObjC-compatible entry point for a constructor.
  void emitObjCConstructorThunk(ConstructorDecl *constructor);

  /// Emit the ObjC-compatible entry point for a destructor (i.e., -dealloc).
  void emitObjCDestructorThunk(DestructorDecl *destructor);

  /// Get or emit the witness table for a protocol conformance.
  SILWitnessTable *getWitnessTable(NormalProtocolConformance *conformance);

  /// Emit a protocol witness entry point.
  SILFunction *
  emitProtocolWitness(ProtocolConformanceRef conformance, SILLinkage linkage,
                      SerializedKind_t serializedKind, SILDeclRef requirement,
                      SILDeclRef witnessRef, IsFreeFunctionWitness_t isFree,
                      Witness witness);

  /// Emit the default witness table for a resilient protocol.
  void emitDefaultWitnessTable(ProtocolDecl *protocol);

  /// Emit the self-conformance witness table for a protocol.
  void emitSelfConformanceWitnessTable(ProtocolDecl *protocol);

  /// Emit the lazy initializer function for a global pattern binding
  /// declaration.
  SILFunction *emitLazyGlobalInitializer(StringRef funcName,
                                         PatternBindingDecl *binding,
                                         unsigned pbdEntry);
  
  /// Emit the accessor for a global variable or stored static property.
  ///
  /// This ensures the lazy initializer has been run before returning the
  /// address of the variable.
  void emitGlobalAccessor(VarDecl *global,
                          SILGlobalVariable *onceToken,
                          SILFunction *onceFunc);

  /// True if the given function requires an entry point for ObjC method
  /// dispatch.
  bool requiresObjCMethodEntryPoint(FuncDecl *method);

  /// True if the given constructor requires an entry point for ObjC method
  /// dispatch.
  bool requiresObjCMethodEntryPoint(ConstructorDecl *constructor);

  /// Emit a global initialization.
  void emitGlobalInitialization(PatternBindingDecl *initializer, unsigned elt);

  /// Is the self method of the given nonmutating method passed indirectly?
  bool isNonMutatingSelfIndirect(SILDeclRef method);

  SILDeclRef getAccessorDeclRef(AccessorDecl *accessor,
                                ResilienceExpansion expansion);

  bool canStorageUseStoredKeyPathComponent(AbstractStorageDecl *decl,
                                           ResilienceExpansion expansion);

  KeyPathPatternComponent
  emitKeyPathComponentForDecl(SILLocation loc,
                              GenericEnvironment *genericEnv,
                              ResilienceExpansion expansion,
                              unsigned &baseOperand,
                              bool &needsGenericContext,
                              SubstitutionMap subs,
                              AbstractStorageDecl *storage,
                              ArrayRef<ProtocolConformanceRef> indexHashables,
                              CanType baseTy,
                              DeclContext *useDC,
                              bool forPropertyDescriptor);

  /// Emit all differentiability witnesses for the given function, visiting its
  /// `@differentiable` and `@derivative` attributes.
  void emitDifferentiabilityWitnessesForFunction(SILDeclRef constant,
                                                 SILFunction *F);

  /// Emit the differentiability witness for the given original function
  /// declaration and SIL function, autodiff configuration, and JVP and VJP
  /// functions (null if undefined).
  void emitDifferentiabilityWitness(AbstractFunctionDecl *originalAFD,
                                    SILFunction *originalFunction,
                                    DifferentiabilityKind diffKind,
                                    const AutoDiffConfig &config,
                                    SILFunction *jvp, SILFunction *vjp,
                                    const DeclAttribute *diffAttr);

  /// Emit a deinit table for a noncopyable type.
  void emitNonCopyableTypeDeinitTable(NominalTypeDecl *decl);

  /// Known functions for bridging.
  SILDeclRef getStringToNSStringFn();
  SILDeclRef getNSStringToStringFn();
  SILDeclRef getArrayToNSArrayFn();
  SILDeclRef getNSArrayToArrayFn();
  SILDeclRef getDictionaryToNSDictionaryFn();
  SILDeclRef getNSDictionaryToDictionaryFn();
  SILDeclRef getSetToNSSetFn();
  SILDeclRef getNSSetToSetFn();
  SILDeclRef getBoolToObjCBoolFn();
  SILDeclRef getObjCBoolToBoolFn();
  SILDeclRef getBoolToDarwinBooleanFn();
  SILDeclRef getDarwinBooleanToBoolFn();
  SILDeclRef getBoolToWindowsBoolFn();
  SILDeclRef getWindowsBoolToBoolFn();
  SILDeclRef getNSErrorToErrorFn();
  SILDeclRef getErrorToNSErrorFn();

#define FUNC_DECL(NAME, ID) \
  FuncDecl *get##NAME(SILLocation loc);
#include "swift/AST/KnownDecls.def"

#define KNOWN_SDK_FUNC_DECL(MODULE, NAME, ID) \
  FuncDecl *get##NAME(SILLocation loc);
#include "swift/AST/KnownSDKDecls.def"
  
  /// Retrieve the _ObjectiveCBridgeable protocol definition.
  ProtocolDecl *getObjectiveCBridgeable(SILLocation loc);

  /// Retrieve the _ObjectiveCBridgeable._bridgeToObjectiveC requirement.
  FuncDecl *getBridgeToObjectiveCRequirement(SILLocation loc);

  /// Retrieve the
  /// _ObjectiveCBridgeable._unconditionallyBridgeFromObjectiveC
  /// requirement.
  FuncDecl *getUnconditionallyBridgeFromObjectiveCRequirement(SILLocation loc);

  /// Retrieve the _ObjectiveCBridgeable._ObjectiveCType requirement.
  AssociatedTypeDecl *getBridgedObjectiveCTypeRequirement(SILLocation loc);

  /// Find the conformance of the given Swift type to the
  /// _ObjectiveCBridgeable protocol.
  ProtocolConformance *getConformanceToObjectiveCBridgeable(SILLocation loc,
                                                            Type type);

  /// Retrieve the _BridgedStoredNSError protocol definition.
  ProtocolDecl *getBridgedStoredNSError(SILLocation loc);

  /// Retrieve the _BridgedStoredNSError._nsError requirement.
  VarDecl *getNSErrorRequirement(SILLocation loc);

  /// Find the conformance of the given Swift type to the
  /// _BridgedStoredNSError protocol.
  ProtocolConformanceRef getConformanceToBridgedStoredNSError(SILLocation loc,
                                                              Type type);

  /// Retrieve the conformance of NSError to the Error protocol.
  ProtocolConformance *getNSErrorConformanceToError();

  /// Retrieve the _Concurrency._asyncLetStart intrinsic.
  FuncDecl *getAsyncLetStart();
  /// Retrieve the _Concurrency._asyncLetGet intrinsic.
  FuncDecl *getAsyncLetGet();
  /// Retrieve the _Concurrency._asyncLetGetThrowing intrinsic.
  FuncDecl *getAsyncLetGetThrowing();
  /// Retrieve the _Concurrency._asyncLetFinish intrinsic.
  FuncDecl *getFinishAsyncLet();

  /// Retrieve the _Concurrency._taskFutureGet intrinsic.
  FuncDecl *getTaskFutureGet();

  /// Retrieve the _Concurrency._taskFutureGetThrowing intrinsic.
  FuncDecl *getTaskFutureGetThrowing();

  /// Retrieve the _Concurrency._resumeUnsafeContinuation intrinsic.
  FuncDecl *getResumeUnsafeContinuation();
  /// Retrieve the _Concurrency._resumeUnsafeThrowingContinuation intrinsic.
  FuncDecl *getResumeUnsafeThrowingContinuation();
  /// Retrieve the _Concurrency._resumeUnsafeThrowingContinuationWithError intrinsic.
  FuncDecl *getResumeUnsafeThrowingContinuationWithError();
  /// Retrieve the _Concurrency._runTaskForBridgedAsyncMethod intrinsic.
  FuncDecl *getRunTaskForBridgedAsyncMethod();
  /// Retrieve the _Concurrency._checkExpectedExecutor intrinsic.
  FuncDecl *getCheckExpectedExecutor();

  /// Retrieve the _Concurrency._createCheckedContinuation intrinsic.
  FuncDecl *getCreateCheckedContinuation();
  /// Retrieve the _Concurrency._createCheckedThrowingContinuation intrinsic.
  FuncDecl *getCreateCheckedThrowingContinuation();
  /// Retrieve the _Concurrency._resumeCheckedContinuation intrinsic.
  FuncDecl *getResumeCheckedContinuation();
  /// Retrieve the _Concurrency._resumeCheckedThrowingContinuation intrinsic.
  FuncDecl *getResumeCheckedThrowingContinuation();
  /// Retrieve the _Concurrency._resumeCheckedThrowingContinuationWithError intrinsic.
  FuncDecl *getResumeCheckedThrowingContinuationWithError();

  /// Retrieve the _Concurrency._asyncMainDrainQueue intrinsic.
  FuncDecl *getAsyncMainDrainQueue();
  /// Retrieve the _Concurrency._swiftJobRun intrinsic.
  FuncDecl *getSwiftJobRun();
  /// Retrieve the _Concurrency._deinitOnExecutor intrinsic.
  FuncDecl *getDeinitOnExecutor();
  // Retrieve the _SwiftConcurrencyShims.exit intrinsic.
  FuncDecl *getExit();

  SILFunction *getKeyPathProjectionCoroutine(bool isReadAccess,
                                             KeyPathTypeKind typeKind);

  /// Report a diagnostic.
  template<typename...T, typename...U>
  InFlightDiagnostic diagnose(SourceLoc loc, Diag<T...> diag,
                U &&...args) {
    return M.getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template<typename...T, typename...U>
  InFlightDiagnostic diagnose(SILLocation loc, Diag<T...> diag,
                U &&...args) {
    return M.getASTContext().Diags.diagnose(loc.getSourceLoc(),
                                     diag, std::forward<U>(args)...);
  }

  /// Get or create SILGlobalVariable for a given global VarDecl.
  SILGlobalVariable *getSILGlobalVariable(VarDecl *gDecl,
                                          ForDefinition_t forDef);

  /// Emit all lazy conformances referenced from this function body.
  void emitLazyConformancesForFunction(SILFunction *F);

  /// Emit all lazy conformances referenced from this type's signature and
  /// stored properties (or in the case of enums, associated values).
  void emitLazyConformancesForType(NominalTypeDecl *NTD);

  /// Mark a protocol conformance as used, so we know we need to emit it if
  /// it's in our TU.
  void useConformance(ProtocolConformanceRef conformance);

  /// Mark protocol conformances from the given type as used.
  void useConformancesFromType(CanType type);

  /// Mark protocol conformances from the given set of substitutions as used.
  void useConformancesFromSubstitutions(SubstitutionMap subs);

  /// Mark _ObjectiveCBridgeable conformances as used for any imported types
  /// mentioned by the given type.
  void useConformancesFromObjectiveCType(CanType type);

  /// Make a note of a member reference expression, which allows us
  /// to ensure that the conformance above is emitted wherever it
  /// needs to be.
  void noteMemberRefExpr(MemberRefExpr *e);

  /// Map the substitutions for the original declaration to substitutions for
  /// the overridden declaration.
  static SubstitutionMap mapSubstitutionsForWitnessOverride(
                                               AbstractFunctionDecl *original,
                                               AbstractFunctionDecl *overridden,
                                               SubstitutionMap subs);

  /// Emit a property descriptor for the given storage decl if it needs one.
  void tryEmitPropertyDescriptor(AbstractStorageDecl *decl);

  /// Replace local archetypes captured from outer AST contexts with primary
  /// archetypes.
  void recontextualizeCapturedLocalArchetypes(
      SILFunction *F, GenericSignatureWithCapturedEnvironments sig);

private:
  /// The most recent declaration we considered for emission.
  SILDeclRef lastEmittedFunction;

  /// Emit the deallocator for a class that uses the objc allocator.
  void emitObjCAllocatorDestructor(ClassDecl *cd, DestructorDecl *dd);

  /// Emit the actual body of deallocator.
  /// If deinit is isolated, function should be an isolated deallocator, an
  /// actual deallocator is just a thunk that switches executors. If deinit is
  /// isolated, function should be the deallocator itself.
  void emitDeallocatorImpl(SILDeclRef constant, SILFunction *f);
};
 
} // end namespace Lowering
} // end namespace swift

#endif
