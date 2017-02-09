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
#include "SILGenProfiling.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseMap.h"
#include <deque>

namespace swift {
  class SILBasicBlock;

namespace Lowering {
  class TypeConverter;
  class SILGenFunction;

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
  
  /// TopLevelSGF - The SILGenFunction used to visit top-level code, or null if
  /// the current source file is not a script source file.
  SILGenFunction /*nullable*/ *TopLevelSGF;

  /// The profiler for instrumentation based profiling, or null if profiling is
  /// disabled.
  std::unique_ptr<SILGenProfiling> Profiler;

  /// Mapping from SILDeclRefs to emitted SILFunctions.
  llvm::DenseMap<SILDeclRef, SILFunction*> emittedFunctions;
  /// Mapping from ProtocolConformances to emitted SILWitnessTables.
  llvm::DenseMap<ProtocolConformance*, SILWitnessTable*> emittedWitnessTables;

  struct DelayedFunction {
    /// Insert the entity after the given function when it's emitted.
    SILDeclRef insertAfter;
    /// Code that generates the function.
    std::function<void (SILFunction *)> emitter;
  };

  /// Mapping from SILDeclRefs to delayed SILFunction generators for
  /// non-externally-visible symbols.
  llvm::DenseMap<SILDeclRef, DelayedFunction> delayedFunctions;

  /// Queue of delayed SILFunctions that need to be forced.
  std::deque<std::pair<SILDeclRef, DelayedFunction>> forcedFunctions;

  /// The most recent declaration we considered for emission.
  SILDeclRef lastEmittedFunction;

  /// Set of used conformances for which witness tables need to be emitted.
  llvm::DenseSet<NormalProtocolConformance *> usedConformances;

  struct DelayedWitnessTable {
    NormalProtocolConformance *insertAfter;
  };

  /// Set of conformances we delayed emitting witness tables for.
  llvm::DenseMap<NormalProtocolConformance *, DelayedWitnessTable>
    delayedConformances;

  /// Queue of delayed conformances that need to be forced.
  std::deque<std::pair<NormalProtocolConformance *, DelayedWitnessTable>>
    forcedConformances;

  /// The most recent conformance...
  NormalProtocolConformance *lastEmittedConformance = nullptr;

  SILFunction *emitTopLevelFunction(SILLocation Loc);
  
  size_t anonymousSymbolCounter = 0;
  
  /// If true, all functions and globals are made fragile. Currently only used
  /// for compiling the stdlib.
  bool makeModuleFragile;
  
  Optional<SILDeclRef> StringToNSStringFn;
  Optional<SILDeclRef> NSStringToStringFn;
  Optional<SILDeclRef> ArrayToNSArrayFn;
  Optional<SILDeclRef> NSArrayToArrayFn;
  Optional<SILDeclRef> DictionaryToNSDictionaryFn;
  Optional<SILDeclRef> NSDictionaryToDictionaryFn;
  Optional<SILDeclRef> SetToNSSetFn;
  Optional<SILDeclRef> NSSetToSetFn;
  Optional<SILDeclRef> BoolToObjCBoolFn;
  Optional<SILDeclRef> ObjCBoolToBoolFn;
  Optional<SILDeclRef> BoolToDarwinBooleanFn;
  Optional<SILDeclRef> DarwinBooleanToBoolFn;
  Optional<SILDeclRef> NSErrorToErrorFn;
  Optional<SILDeclRef> ErrorToNSErrorFn;

  Optional<ProtocolDecl*> PointerProtocol;

  Optional<ProtocolDecl*> ObjectiveCBridgeable;
  Optional<FuncDecl*> BridgeToObjectiveCRequirement;
  Optional<FuncDecl*> UnconditionallyBridgeFromObjectiveCRequirement;
  Optional<AssociatedTypeDecl*> BridgedObjectiveCType;

  Optional<ProtocolDecl*> BridgedStoredNSError;
  Optional<VarDecl*> NSErrorRequirement;

  Optional<ProtocolConformance *> NSErrorConformanceToError;

public:
  SILGenModule(SILModule &M, ModuleDecl *SM, bool makeModuleFragile);

  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;

  ASTContext &getASTContext() { return M.getASTContext(); }

  static DeclName getMagicFunctionName(SILDeclRef ref);
  static DeclName getMagicFunctionName(DeclContext *dc);
  
  /// Returns the type of a constant reference.
  SILType getConstantType(SILDeclRef constant);
  
  /// Returns the calling convention for a function.
  SILFunctionTypeRepresentation getDeclRefRepresentation(SILDeclRef constant) {
    return getConstantType(constant).getAs<SILFunctionType>()
      ->getRepresentation();
  }

  /// Get the function for a SILDeclRef, or return nullptr if it hasn't been
  /// emitted yet.
  SILFunction *getEmittedFunction(SILDeclRef constant,
                                  ForDefinition_t forDefinition);

  /// Get the function for a SILDeclRef, creating it if necessary.
  SILFunction *getFunction(SILDeclRef constant,
                           ForDefinition_t forDefinition);
  
  /// Get the dynamic dispatch thunk for a SILDeclRef.
  SILFunction *getDynamicThunk(SILDeclRef constant,
                               SILConstantInfo constantInfo);
  
  /// Emit a vtable thunk for a derived method if its natural abstraction level
  /// diverges from the overridden base method. If no thunking is needed,
  /// returns a static reference to the derived method.
  SILFunction *emitVTableMethod(SILDeclRef derived,
                                SILDeclRef base,
                                SILLinkage &implLinkage);

  /// True if a function has been emitted for a given SILDeclRef.
  bool hasFunction(SILDeclRef constant);
  
  /// Get the lowered type for a Swift type.
  SILType getLoweredType(Type t) {
    return Types.getTypeLowering(t).getLoweredType();
  }

  /// Translate a formal enum element decl into its lowered form.
  ///
  /// This just turns ImplicitlyUnwrappedOptional's cases into Optional's.
  EnumElementDecl *getLoweredEnumElementDecl(EnumElementDecl *element);
  
  /// Get or create the declaration of a reabstraction thunk with the
  /// given signature.
  SILFunction *getOrCreateReabstractionThunk(
                                           GenericEnvironment *genericEnv,
                                           CanSILFunctionType thunkType,
                                           CanSILFunctionType fromType,
                                           CanSILFunctionType toType,
                                           IsFragile_t Fragile);

  /// Determine whether the given class has any instance variables that
  /// need to be destroyed.
  bool hasNonTrivialIVars(ClassDecl *cd);
  
  /// Determine whether we need to emit an ivar destroyer for the given class.
  /// An ivar destroyer is needed if a superclass of this class may define a
  /// failing designated initializer.
  bool requiresIVarDestroyer(ClassDecl *cd);

  //===--------------------------------------------------------------------===//
  // Visitors for top-level forms
  //===--------------------------------------------------------------------===//

  // These are either not allowed at global scope or don't require
  // code emission.
  void visitImportDecl(ImportDecl *d) {}
  void visitEnumCaseDecl(EnumCaseDecl *d) {}
  void visitEnumElementDecl(EnumElementDecl *d) {}
  void visitOperatorDecl(OperatorDecl *d) {}
  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *d) {}
  void visitTypeAliasDecl(TypeAliasDecl *d) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *d) {}
  void visitSubscriptDecl(SubscriptDecl *d) {}
  void visitConstructorDecl(ConstructorDecl *d) {}
  void visitDestructorDecl(DestructorDecl *d) {}
  void visitModuleDecl(ModuleDecl *d) { }

  void visitFuncDecl(FuncDecl *fd);
  void visitPatternBindingDecl(PatternBindingDecl *vd);
  void visitTopLevelCodeDecl(TopLevelCodeDecl *td);
  void visitIfConfigDecl(IfConfigDecl *icd);
  void visitNominalTypeDecl(NominalTypeDecl *ntd);
  void visitExtensionDecl(ExtensionDecl *ed);
  void visitVarDecl(VarDecl *vd);

  void emitPropertyBehavior(VarDecl *vd);

  void emitAbstractFuncDecl(AbstractFunctionDecl *AFD);
  
  /// Generate code for a source file of the module.
  void emitSourceFile(SourceFile *sf, unsigned startElem);
  
  /// Generates code for the given FuncDecl and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  void emitFunction(FuncDecl *fd);
  
  /// \brief Generates code for the given closure expression and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(ce).
  SILFunction *emitClosure(AbstractClosureExpr *ce);
  /// Generates code for the given ConstructorDecl and adds
  /// the SILFunction to the current SILModule under the name SILDeclRef(decl).
  void emitConstructor(ConstructorDecl *decl);

  /// Generates code for the given class's destructor and adds
  /// the SILFunction to the current SILModule under the name
  /// SILDeclRef(cd, Destructor).
  void emitDestructor(ClassDecl *cd, DestructorDecl *dd);

  /// Generates the enum constructor for the given
  /// EnumElementDecl under the name SILDeclRef(decl).
  void emitEnumConstructor(EnumElementDecl *decl);

  /// Emits the default argument generator with the given expression.
  void emitDefaultArgGenerator(SILDeclRef constant, Expr *arg);

  /// Emits the stored property initializer for the given pattern.
  void emitStoredPropertyInitialization(PatternBindingDecl *pd, unsigned i);

  /// Emits the default argument generator for the given function.
  void emitDefaultArgGenerators(SILDeclRef::Loc decl,
                                ArrayRef<ParameterList*> paramLists);

  /// Emits the curry thunk between two uncurry levels of a function.
  void emitCurryThunk(ValueDecl *fd,
                      SILDeclRef entryPoint,
                      SILDeclRef nextEntryPoint);
  
  /// Emits a thunk from a foreign function to the native Swift convention.
  void emitForeignToNativeThunk(SILDeclRef thunk);

  /// Emits a thunk from a Swift function to the native Swift convention.
  void emitNativeToForeignThunk(SILDeclRef thunk);

  template<typename T>
  void preEmitFunction(SILDeclRef constant, T *astNode,
                       SILFunction *F,
                       SILLocation L);
  void postEmitFunction(SILDeclRef constant, SILFunction *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);
  
  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalDefinition(Decl *d);

  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalWitnessTable(ProtocolConformance *d);

  /// Emit the ObjC-compatible entry point for a method.
  void emitObjCMethodThunk(FuncDecl *method);
  
  /// Emit the ObjC-compatible getter and setter for a property.
  void emitObjCPropertyMethodThunks(AbstractStorageDecl *prop);

  /// Emit the ObjC-compatible entry point for a constructor.
  void emitObjCConstructorThunk(ConstructorDecl *constructor);

  /// Emit the ObjC-compatible entry point for a destructor (i.e., -dealloc).
  void emitObjCDestructorThunk(DestructorDecl *destructor);

  /// Get or emit the witness table for a protocol conformance.
  SILWitnessTable *getWitnessTable(ProtocolConformance *conformance);
  
  /// Emit a protocol witness entry point.
  SILFunction *emitProtocolWitness(ProtocolConformance *conformance,
                                   SILLinkage linkage,
                                   SILDeclRef requirement,
                                   SILDeclRef witnessRef,
                                   IsFreeFunctionWitness_t isFree,
                                   Witness witness);

  /// Emit the default witness table for a resilient protocol.
  void emitDefaultWitnessTable(ProtocolDecl *protocol);

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

  void emitGlobalGetter(VarDecl *global,
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
  SILDeclRef getNSErrorToErrorFn();
  SILDeclRef getErrorToNSErrorFn();

#define FUNC_DECL(NAME, ID) \
  FuncDecl *get##NAME(SILLocation loc);
#include "swift/AST/KnownDecls.def"
  
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
  Optional<ProtocolConformanceRef>
  getConformanceToBridgedStoredNSError(SILLocation loc, Type type);

  /// Retrieve the conformance of NSError to the Error protocol.
  ProtocolConformance *getNSErrorConformanceToError();

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

  /// Mark a protocol conformance as used, so we know we need to emit it if
  /// it's in our TU.
  void useConformance(ProtocolConformanceRef conformance);

  /// Mark protocol conformances from the given set of substitutions as used.
  void useConformancesFromSubstitutions(SubstitutionList subs);

  /// Emit a `mark_function_escape` instruction for top-level code when a
  /// function or closure at top level refers to script globals.
  void emitMarkFunctionEscapeForTopLevelCodeGlobals(SILLocation loc,
                                                const CaptureInfo &captureInfo);

  /// Get the substitutions necessary to invoke a non-member (global or local)
  /// property.
  SubstitutionList
  getNonMemberVarDeclSubstitutions(VarDecl *var);

private:
  /// Emit the deallocator for a class that uses the objc allocator.
  void emitObjCAllocatorDestructor(ClassDecl *cd, DestructorDecl *dd);
};
 
} // end namespace Lowering
} // end namespace swift

#endif
