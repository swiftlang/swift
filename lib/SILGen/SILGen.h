//===--- SILGen.h - Implements Lowering of ASTs -> SIL ----------*- C++ -*-===//
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
  Module *SwiftModule;
  
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
  
  SILFunction *emitTopLevelFunction(SILLocation Loc);
  
  size_t anonymousSymbolCounter = 0;
  
  /// If true, all functions and global's are made fragile. Currently only used
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

  Optional<ProtocolDecl*> PointerProtocol;
  
public:
  SILGenModule(SILModule &M, Module *SM, bool makeModuleFragile);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;

  ASTContext &getASTContext() { return M.getASTContext(); }

  static DeclName getMagicFunctionName(SILDeclRef ref);
  static DeclName getMagicFunctionName(DeclContext *dc);
  
  /// Returns the type of a constant reference.
  SILType getConstantType(SILDeclRef constant);
  
  /// Returns the calling convention for a function.
  AbstractCC getConstantCC(SILDeclRef constant) {
    return getConstantType(constant).getAbstractCC();
  }
  
  /// Get the function for a SILDeclRef.
  SILFunction *getFunction(SILDeclRef constant,
                           ForDefinition_t forDefinition);
  
  /// Get the dynamic dispatch thunk for a SILDeclRef.
  SILFunction *getDynamicThunk(SILDeclRef constant,
                               SILConstantInfo constantInfo);
  
  /// Emit a vtable thunk for a derived method if its natural abstraction level
  /// diverges from the overridden base method. If no thunking is needed,
  /// returns a static reference to the derived method.
  SILFunction *emitVTableMethod(SILDeclRef derived,
                                SILDeclRef base);

  /// True if a function has been emitted for a given SILDeclRef.
  bool hasFunction(SILDeclRef constant);
  
  /// Get the lowered type for a Swift type.
  SILType getLoweredType(Type t) {
    return Types.getTypeLowering(t).getLoweredType();
  }
  
  /// Get or create the declaration of a reabstraction thunk with the
  /// given signature.
  SILFunction *getOrCreateReabstractionThunk(
                                           GenericParamList *thunkContextParams,
                                           CanSILFunctionType thunkType,
                                           CanSILFunctionType fromType,
                                           CanSILFunctionType toType,
                                           IsFragile_t Fragile);
  
  //===--------------------------------------------------------------------===//
  // Visitors for top-level forms
  //===--------------------------------------------------------------------===//

  // These are either not allowed at global scope or don't require
  // code emission.
  void visitImportDecl(ImportDecl *d) {}
  void visitEnumCaseDecl(EnumCaseDecl *d) {}
  void visitEnumElementDecl(EnumElementDecl *d) {}
  void visitOperatorDecl(OperatorDecl *d) {}
  void visitTypeAliasDecl(TypeAliasDecl *d) {}
  void visitAbstractTypeParamDecl(AbstractTypeParamDecl *d) {}
  void visitSubscriptDecl(SubscriptDecl *d) {}
  void visitConstructorDecl(ConstructorDecl *d) {}
  void visitDestructorDecl(DestructorDecl *d) {}

  void visitFuncDecl(FuncDecl *fd);
  void visitPatternBindingDecl(PatternBindingDecl *vd);
  void visitTopLevelCodeDecl(TopLevelCodeDecl *td);
  void visitIfConfigDecl(IfConfigDecl *icd);
  void visitNominalTypeDecl(NominalTypeDecl *ntd);
  void visitExtensionDecl(ExtensionDecl *ed);
  void visitVarDecl(VarDecl *vd);

  void emitAbstractFuncDecl(AbstractFunctionDecl *AFD);
  
  /// Generate code for a source file of the module.
  void emitSourceFile(SourceFile *sf, unsigned startElem);
  
  /// Generates code for the given FuncDecl and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  void emitFunction(FuncDecl *fd);
  
  /// \brief Generates code for the given closure expression and adds the
  /// SILFunction to the current SILModule under the nane SILDeclRef(ce).
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
  
  /// Emits the default argument generator for the given function.
  void emitDefaultArgGenerators(SILDeclRef::Loc decl,
                                ArrayRef<Pattern*> patterns);

  /// Emits the curry thunk between two uncurry levels of a function.
  void emitCurryThunk(SILDeclRef entryPoint,
                      SILDeclRef nextEntryPoint,
                      FuncDecl *fd);
  
  /// Emits a thunk from a foreign function to the native Swift convention.
  void emitForeignToNativeThunk(SILDeclRef thunk);

  /// Emits a thunk from a Swift function to the native Swift convention.
  void emitNativeToForeignThunk(SILDeclRef thunk);

  template<typename T>
  SILFunction *preEmitFunction(SILDeclRef constant, T *astNode,
                               SILLocation L);
  void postEmitFunction(SILDeclRef constant, SILFunction *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);
  
  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalDefinition(Decl *d);
  
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
                                   SILDeclRef witness,
                                   IsFreeFunctionWitness_t isFree,
                                   ArrayRef<Substitution> witnessSubs);

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
  
  /// True if calling the given method or property should use ObjC dispatch.
  bool requiresObjCDispatch(ValueDecl *vd);
  
  /// True if super-calling the given method from a subclass should use ObjC
  /// dispatch.
  bool requiresObjCSuperDispatch(ValueDecl *vd);

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

private:
  /// Emit the deallocator for a class that uses the objc allocator.
  void emitObjCAllocatorDestructor(ClassDecl *cd, DestructorDecl *dd);
};
 
} // end namespace Lowering
} // end namespace swift

#endif
