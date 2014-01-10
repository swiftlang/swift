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
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/Optional.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class SILBasicBlock;

namespace Lowering {
  class LogicalPathComponent;
  class LValue;
  class RValue;
  class RValueSource;
  class ManagedValue;
  class TypeConverter;
  class SILGenFunction;
  class Initialization;
  class RValueSource;
  class TemporaryInitialization;

/// An enum to indicate whether a protocol method requirement is satisfied by
/// a free function, as for an operator requirement.
enum IsFreeFunctionWitness_t : bool {
  IsNotFreeFunctionWitness = false,
  IsFreeFunctionWitness = true,
};
  
/// An enum to indicate whether a protocol method requirement is satisfied by a
/// method with an abstraction difference in the @inout-ness of its self
/// parameter.
enum HasInOutSelfAbstractionDifference_t : bool {
  DoesNotHaveInOutSelfAbstractionDifference = false,
  HasInOutSelfAbstractionDifference = true,
};

enum ForDefinition_t : bool {
  NotForDefinition = false,
  ForDefinition = true
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
  
  /// Mapping from SILDeclRefs to emitted SILFunctions.
  llvm::DenseMap<SILDeclRef, SILFunction*> emittedFunctions;
  /// Mapping from ProtocolConformances to emitted SILWitnessTables.
  llvm::DenseMap<ProtocolConformance*, SILWitnessTable*> emittedWitnessTables;
  
  SILFunction *emitTopLevelFunction(SILLocation Loc);
  
  size_t anonymousSymbolCounter = 0;
  
  Optional<SILDeclRef> StringToNSStringFn;
  Optional<SILDeclRef> NSStringToStringFn;
  Optional<SILDeclRef> BoolToObjCBoolFn;
  Optional<SILDeclRef> ObjCBoolToBoolFn;
  Optional<SILDeclRef> StringDefaultInitFn;
  
public:
  SILGenModule(SILModule &M, Module *SM);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;

  ASTContext &getASTContext() { return M.getASTContext(); }
  
  /// Returns the type of a constant reference.
  SILType getConstantType(SILDeclRef constant);
  
  /// Returns the calling convention for a function.
  AbstractCC getConstantCC(SILDeclRef constant) {
    return getConstantType(constant).getAbstractCC();
  }
  
  /// Determine the linkage of a constant.
  SILLinkage getConstantLinkage(SILDeclRef constant,
                                ForDefinition_t forDefinition);
  
  /// Get the function for a SILDeclRef.
  SILFunction *getFunction(SILDeclRef constant,
                           ForDefinition_t forDefinition);

  /// True if a function has been emitted for a given SILDeclRef.
  bool hasFunction(SILDeclRef constant);
  
  /// Get the lowered type for a Swift type.
  SILType getLoweredType(Type t) {
    return Types.getTypeLowering(t).getLoweredType();
  }
  
  /// Get or create the declaration of a reabstraction thunk with the
  /// given signature.
  SILFunction *getOrCreateReabstractionThunk(SILLocation loc,
                                             CanSILFunctionType thunkType,
                                             CanSILFunctionType fromType,
                                             CanSILFunctionType toType);

  //===--------------------------------------------------------------------===//
  // Visitors for top-level forms
  //===--------------------------------------------------------------------===//
  // FIXME: visit other decls
  void visitFuncDecl(FuncDecl *fd);
  void visitPatternBindingDecl(PatternBindingDecl *vd);
  void visitTopLevelCodeDecl(TopLevelCodeDecl *td);
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
  void emitClosure(AbstractClosureExpr *ce);
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
  
  /// Emits a thunk from a foreign function to the native Swift conventions.
  void emitForeignThunk(SILDeclRef thunk);
  
  template<typename T>
  SILFunction *preEmitFunction(SILDeclRef constant, T *astNode, SILLocation L);
  void postEmitFunction(SILDeclRef constant, SILFunction *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);
  
  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalDefinition(Decl *d);
  
  /// Emit the ObjC-compatible entry point for a method.
  void emitObjCMethodThunk(FuncDecl *method);
  
  /// Emit the ObjC-compatible getter and setter for a property.
  void emitObjCPropertyMethodThunks(VarDecl *prop);

  /// Emit the ObjC-compatible entry point for a constructor.
  void emitObjCConstructorThunk(ConstructorDecl *constructor);

  /// Emit the ObjC-compatible getter and setter for a subscript
  /// declaration.
  void emitObjCSubscriptMethodThunks(SubscriptDecl *subscript);
  
  /// Get or emit the witness table for a protocol conformance.
  /// Return null if the conformance does not have a witness table directly
  /// associated with itself.
  SILWitnessTable *getWitnessTable(ProtocolConformance *conformance);
  
  /// Emit a protocol witness entry point.
  SILFunction *emitProtocolWitness(ProtocolConformance *conformance,
                                   SILDeclRef requirement,
                                   SILDeclRef witness,
                                   IsFreeFunctionWitness_t isFree,
                                   ArrayRef<Substitution> witnessSubs);

  /// Emit the lazy initializer function for a global pattern binding
  /// declaration.
  SILFunction *emitLazyGlobalInitializer(StringRef funcName,
                                         PatternBindingDecl *binding);
  
  /// Emit the accessor for a global variable or stored static property.
  ///
  /// This ensures the lazy initializer has been run before returning the
  /// address of the variable.
  void emitGlobalAccessor(VarDecl *global,
                          FuncDecl *builtinOnceDecl,
                          SILGlobalVariable *onceToken,
                          SILFunction *onceFunc);
  
  /// True if the given function requires an entry point for ObjC method
  /// dispatch.
  bool requiresObjCMethodEntryPoint(FuncDecl *method);

  /// True if the given constructor requires an entry point for ObjC method
  /// dispatch.
  bool requiresObjCMethodEntryPoint(ConstructorDecl *constructor);

  /// True if the given property requires entry points for ObjC property method
  /// dispatch.
  bool requiresObjCPropertyEntryPoints(VarDecl *property);

  /// True if the given subscript requires entry points for ObjC
  /// subscript operations
  bool requiresObjCSubscriptEntryPoints(SubscriptDecl *subscript);
  
  /// True if calling the given method should use ObjC dispatch.
  bool requiresObjCDispatch(ValueDecl *vd);
  
  /// True if super-calling the given method from a subclass should use ObjC
  /// dispatch.
  bool requiresObjCSuperDispatch(ValueDecl *vd);

  /// Emit a global initialization.
  void emitGlobalInitialization(PatternBindingDecl *initializer);
  
  /// Known functions for bridging.
  SILDeclRef getStringToNSStringFn();
  SILDeclRef getNSStringToStringFn();
  SILDeclRef getBoolToObjCBoolFn();
  SILDeclRef getObjCBoolToBoolFn();
  SILDeclRef getStringDefaultInitFn();
  
  /// Report a diagnostic.
  template<typename...T, typename...U>
  void diagnose(SourceLoc loc, Diag<T...> diag,
                U &&...args) {
    M.getASTContext().Diags.diagnose(loc, diag, std::forward<U>(args)...);
  }

  template<typename...T, typename...U>
  void diagnose(SILLocation loc, Diag<T...> diag,
                U &&...args) {
    M.getASTContext().Diags.diagnose(loc.getSourceLoc(),
                                     diag, std::forward<U>(args)...);
  }
};
  
/// Materialize - Represents a temporary allocation.
struct Materialize {
  /// The address of the allocation.
  SILValue address;

  /// The cleanup to dispose of the value before deallocating the buffer.
  /// This cleanup can be killed by calling the consume method.
  CleanupHandle valueCleanup;
  
  /// Load and claim ownership of the value in the buffer. Does not deallocate
  /// the buffer.
  ManagedValue claim(SILGenFunction &gen, SILLocation loc);
};

} // end namespace Lowering
} // end namespace swift

#endif
