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

#ifndef SILGen_H
#define SILGen_H

#include "ASTVisitor.h"
#include "Cleanup.h"
#include "Condition.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/Optional.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {
  class SILBasicBlock;

namespace Lowering {
  class Condition;
  class LogicalPathComponent;
  class LValue;
  class RValue;
  class ManagedValue;
  class TypeConverter;
  class SILGenFunction;
  class Initialization;
  class OwnershipConventions;

/// SILGenModule - an ASTVisitor for generating SIL from top-level declarations
/// in a translation unit.
class LLVM_LIBRARY_VISIBILITY SILGenModule : public ASTVisitor<SILGenModule> {
public:
  /// The Module being constructed.
  SILModule &M;
  
  /// The type converter for the module.
  TypeConverter &Types;
  
  /// TopLevelSGF - The SILGenFunction used to visit top-level code, or null if
  /// the module is not a main module.
  SILGenFunction /*nullable*/ *TopLevelSGF;
  
  /// Mapping from SILDeclRefs to emitted SILFunctions.
  llvm::DenseMap<SILDeclRef, SILFunction*> emittedFunctions;
  
  SILFunction *emitTopLevelFunction();
  
  size_t anonymousFunctionCounter = 0;
  
  Optional<SILDeclRef> StringToNSStringFn;
  Optional<SILDeclRef> NSStringToStringFn;
  Optional<SILDeclRef> BoolToObjCBoolFn;
  Optional<SILDeclRef> ObjCBoolToBoolFn;
  
public:
  SILGenModule(SILModule &M);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;
  
  /// Returns the type of a constant reference.
  SILType getConstantType(SILDeclRef constant);
  
  /// Returns the calling convention for a function.
  AbstractCC getConstantCC(SILDeclRef constant) {
    return getConstantType(constant).getAbstractCC();
  }
  
  /// Determine the linkage of a constant.
  SILLinkage getConstantLinkage(SILDeclRef constant);
  
  /// Get the function for a SILDeclRef.
  SILFunction *getFunction(SILDeclRef constant);

  /// True if a function has been emitted for a given SILDeclRef.
  bool hasFunction(SILDeclRef constant);
  
  /// Get the lowered type for a Swift type.
  SILType getLoweredType(Type t) {
    return Types.getTypeLowering(t).getLoweredType();
  }
  
  /// Generate the mangled symbol name for a SILDeclRef.
  void mangleConstant(SILDeclRef constant,
                      SILFunction *f);

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

  /// Generates code for the given FuncExpr and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  void emitFunction(SILDeclRef::Loc decl, FuncExpr *fe);
  /// \brief Generates code for the given closure expression and adds the 
  /// SILFunction to the current SILModule under the nane SILDeclRef(ce).
  void emitClosure(PipeClosureExpr *ce);
  /// Generates code for the given ClosureExpr and adds the
  /// SILFunction to the current SILModule under the name SILDeclRef(ce).
  void emitClosure(ClosureExpr *ce);
  /// Generates code for the given ConstructorDecl and adds
  /// the SILFunction to the current SILModule under the name SILDeclRef(decl).
  void emitConstructor(ConstructorDecl *decl);
  /// Generates code for the given class's destructor and adds
  /// the SILFunction to the current SILModule under the name
  /// SILDeclRef(cd, Destructor). If a DestructorDecl is provided, it will be
  /// used, otherwise only the implicit destruction behavior will be emitted.
  void emitDestructor(ClassDecl *cd, DestructorDecl /*nullable*/ *dd);
  /// Generates the union constructor for the given
  /// UnionElementDecl under the name SILDeclRef(decl).
  void emitUnionConstructor(UnionElementDecl *decl);

  /// Emits the default argument generator with the given expression.
  void emitDefaultArgGenerator(SILDeclRef constant, Expr *arg);
  
  /// Emits the default argument generator for the given function.
  void emitDefaultArgGenerators(SILDeclRef::Loc decl, 
                                ArrayRef<Pattern*> patterns);

  /// emitCurryThunk - Emits the curry thunk between two uncurry levels of a
  /// function.
  void emitCurryThunk(SILDeclRef entryPoint,
                      SILDeclRef nextEntryPoint,
                      FuncExpr *fe);
  
  template<typename T>
  SILFunction *preEmitFunction(SILDeclRef constant, T *astNode,
                               SILLocation L = SILLocation());
  void postEmitFunction(SILDeclRef constant, SILFunction *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);
  
  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalDefinition(Decl *d);
  
  /// Emit the ObjC-compatible entry point for a method.
  void emitObjCMethodThunk(FuncDecl *method);
  
  /// Emit the ObjC-compatible getter and setter for an instance variable or
  /// property.
  void emitObjCPropertyMethodThunks(VarDecl *prop);
  
  /// True if the given function requires an entry point for ObjC method
  /// dispatch.
  bool requiresObjCMethodEntryPoint(FuncDecl *method);
  
  /// True if the given property requires entry points for ObjC property method
  /// dispatch.
  bool requiresObjCPropertyEntryPoints(VarDecl *property);
  
  /// True if calling the given method should use ObjC dispatch.
  bool requiresObjCDispatch(ValueDecl *vd);
  
  /// True if super-calling the given method from a subclass should use ObjC
  /// dispatch.
  bool requiresObjCSuperDispatch(ValueDecl *vd);

  /// Known functions for bridging.
  SILDeclRef getStringToNSStringFn();
  SILDeclRef getNSStringToStringFn();
  SILDeclRef getBoolToObjCBoolFn();
  SILDeclRef getObjCBoolToBoolFn();
  
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
  CleanupsDepth valueCleanup;
  
  /// Load and claim ownership of the value in the buffer. Does not deallocate
  /// the buffer.
  ManagedValue claim(SILGenFunction &gen, SILLocation loc);
};
  
/// SGFContext - Internal context information for the SILGenFunction visitor.
struct SGFContext {
private:
  using State =
    llvm::PointerIntPair<Initialization *, 1, bool>;
  
  State state;
public:
  SGFContext() = default;
  
  /// Creates an emitInto context that will store the result of the visited expr
  /// into the given Initialization.
  SGFContext(Initialization *emitInto)
    : state(emitInto, false)
  {}
  
  /// Creates a load expr context, in which a temporary lvalue that would
  /// normally be materialized can be left as an rvalue to avoid a useless
  /// immediately-consumed allocation.
  SGFContext(bool isChildOfLoadExpr)
    : state(nullptr, isChildOfLoadExpr)
  {}

  /// Returns a pointer to the Initialization that the current expression should
  /// store its result to, or null if the expression should allocate temporary
  /// storage for its result.
  Initialization *getEmitInto() const {
    return state.getPointer();
  }
  
  /// Returns true if the current expression is a child of a LoadExpr, and
  /// should thus avoid emitting a temporary materialization if possible.
  bool isChildOfLoadExpr() const {
    return state.getInt();
  }
};

class SwitchContext;
  
/// SILGenFunction - an ASTVisitor for producing SIL from function bodies.
class LLVM_LIBRARY_VISIBILITY SILGenFunction
  : public ASTVisitor<SILGenFunction>
{ // style violation because Xcode <rdar://problem/13065676>
public:
  /// The SILGenModule this function belongs to.
  SILGenModule &SGM;
    
  /// The SILFunction being constructed.
  SILFunction &F;

  ASTContext &getASTContext() const { return SGM.M.getASTContext(); }

  /// This is used to keep track of all SILInstructions inserted by \c B.
  SmallVector<SILInstruction*, 32> InsertedInstrs;
  size_t LastInsnWithoutScope;
  
  /// B - The SILBuilder used to construct the SILFunction.  It is what maintains
  /// the notion of the current block being emitted into.
  SILBuilder B;
    
  /// IndirectReturnAddress - For a function with an indirect return, holds a
  /// value representing the address to initialize with the return value. Null
  /// for a function that returns by value.
  SILValue IndirectReturnAddress;
  
  std::vector<JumpDest> BreakDestStack;
  std::vector<JumpDest> ContinueDestStack;
  std::vector<SwitchContext *> SwitchStack;
  /// Keep track of our current nested scope.
  std::vector<SILDebugScope*> DebugScopeStack;

  /// The cleanup depth and epilog BB for "return" instructions.
  JumpDest ReturnDest;
  /// True if a non-void return is required in this function.
  bool NeedsReturn;
  
  /// \brief Location information for the return instruction.
  ///
  /// FIXME: Currently this is only ever the location of the function body, or
  /// of the first ReturnStmt in the function body. We probably want richer
  /// return location information.
  SILLocation ReturnLoc;

  /// \brief The SIL location corresponding to the AST node being processed.
  SILLocation CurrentSILLoc;

  /// Cleanups - This records information about the currently active cleanups.
  CleanupManager Cleanups;

  /// A pending writeback.
  struct Writeback {
    SILLocation loc;
    std::unique_ptr<LogicalPathComponent> component;
    SILValue base;
    Materialize temp;
    
    // Instantiate the unique_ptr destructor in a scope where
    // LogicalPathComponent is defined.
    ~Writeback();
    Writeback(Writeback&&) = default;
    Writeback &operator=(Writeback&&) = default;
    
    Writeback() = default;
    Writeback(SILLocation loc, std::unique_ptr<LogicalPathComponent> &&comp,
              SILValue base, Materialize temp);
  };

  /// The stack of pending writebacks.
  std::vector<Writeback> WritebackStack;
  bool InWritebackScope = false;
  
  void pushWritebackIfInScope(SILLocation loc,
                              const LogicalPathComponent &component,
                              SILValue base,
                              Materialize temp);

  /// VarLoc - representation of an emitted local variable.
  struct VarLoc {
    /// box - the retainable box for the variable, or invalid if no box was
    /// made for the value.
    SILValue box;
    /// address - the address at which the variable is stored.
    SILValue address;
  };
    
  /// VarLocs - Entries in this map are generated when a PatternBindingDecl is
  /// emitted. The map is queried to produce the lvalue for a DeclRefExpr to
  /// a local variable.
  llvm::DenseMap<ValueDecl*, VarLoc> VarLocs;
    
  /// LocalConstants - Entries in this map are generated when a local constant
  /// declaration that requires local context, such as a func closure, is
  /// emitted. This map is then queried to produce the value for a DeclRefExpr
  /// to a local constant.
  llvm::DenseMap<SILDeclRef, SILValue> LocalConstants;
  
  /// True if 'return' without an operand or falling off the end of the current
  /// function is valid.
  bool allowsVoidReturn() const {
    return ReturnDest.getBlock()->bbarg_empty();
  }
  
  /// This location, when set, is used as an override location for magic
  /// identifier expansion (e.g. __FILE__).  This allows default argument
  /// expansion to report the location of the call, instead of the location
  /// of the original expr.
  SourceLoc overrideLocationForMagicIdentifiers;
  
  SILGenFunction(SILGenModule &SGM, SILFunction &F);
  ~SILGenFunction();
  
  /// Return a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Cleanups.getCleanupsDepth();
  }
  
  SILFunction &getFunction() { return F; }
  SILBuilder &getBuilder() { return B; }
  
  const TypeLowering &getTypeLowering(Type t, unsigned uncurryLevel = 0) {
    return SGM.Types.getTypeLowering(t, uncurryLevel);
  }
  SILType getLoweredType(Type t, unsigned uncurryLevel = 0) {
    return getTypeLowering(t, uncurryLevel).getLoweredType();
  }
  SILType getLoweredLoadableType(Type t, unsigned uncurryLevel = 0) {
    return SGM.Types.getLoweredLoadableType(t, uncurryLevel);
  }
  const TypeLowering &getTypeLowering(SILType type) {
    return SGM.Types.getTypeLowering(type);
  }

  SourceManager &getSourceManager() { return SGM.M.getASTContext().SourceMgr; }

  /// enterDebugScope - Push a new debug scope and set its parent pointer.
  void enterDebugScope(SILDebugScope *DS) {
    if (DebugScopeStack.size())
      DS->setParent(DebugScopeStack.back());
    else
      DS->setParent(F.getDebugScope());
    DebugScopeStack.push_back(DS);
    setDebugScopeForInsertedInstrs(DS->Parent);
  }

  /// enterDebugScope - return to the previous debug scope.
  void leaveDebugScope() {
    assert(DebugScopeStack.size());
    setDebugScopeForInsertedInstrs(DebugScopeStack.back());
    DebugScopeStack.pop_back();
  }

  /// Set the debug scope for all SILInstructions that where emitted
  /// from when we entered the last scope up to the current one.
  void setDebugScopeForInsertedInstrs(SILDebugScope *DS) {
    while (LastInsnWithoutScope < InsertedInstrs.size()) {
      InsertedInstrs[LastInsnWithoutScope]->setDebugScope(DS);
      ++LastInsnWithoutScope;
    }
  }

  //===--------------------------------------------------------------------===//
  // Entry points for codegen
  //===--------------------------------------------------------------------===//
  
  /// emitFunction - Generates code for a FuncExpr.
  void emitFunction(FuncExpr *fe);
  /// \brief Emits code for a PipeClosureExpr.
  void emitClosure(PipeClosureExpr *ce);
  /// emitClosure - Generates code for a ClosureExpr. This is akin
  /// to visiting the body as if wrapped in a ReturnStmt.
  void emitClosure(ClosureExpr *ce);
  /// emitDestructor - Generates code for a class destroying destructor. This
  /// emits the body code from the DestructorDecl (if any),
  /// implicitly releases the elements of the class, and calls the base
  /// class destructor.
  void emitDestructor(ClassDecl *cd, DestructorDecl *dd);
  
  /// Generates code for a struct constructor.
  /// This allocates the new 'self' value, emits the
  /// body code, then returns the final initialized 'self'.
  void emitValueConstructor(ConstructorDecl *ctor);
  /// Generates code for a union case constructor.
  /// This allocates the new 'self' value, injects the union case,
  /// then returns the final initialized 'self'.
  void emitUnionConstructor(UnionElementDecl *element);
  /// Generates code for a class constructor's
  /// allocating entry point. This allocates the new 'self' value, passes it to
  /// the initializer entry point, then returns the initialized 'self'.
  void emitClassConstructorAllocator(ConstructorDecl *ctor);
  /// Generates code for a class constructor's
  /// initializing entry point. This takes 'self' and the constructor arguments
  /// as parameters and executes the constructor body to initialize 'self'.
  void emitClassConstructorInitializer(ConstructorDecl *ctor);
  /// Generates code for a curry thunk from one uncurry level
  /// of a function to another.
  void emitCurryThunk(FuncExpr *fe, SILDeclRef fromLevel, SILDeclRef toLevel);

  // Generate a nullary function that returns the given value.
  void emitGeneratorFunction(SILDeclRef function, Expr *value);

  /// Generate an ObjC-compatible thunk for a method.
  void emitObjCMethodThunk(SILDeclRef thunk);
  
  /// Generate an ObjC-compatible getter for a property.
  void emitObjCPropertyGetter(SILDeclRef getter);
  
  /// Generate an ObjC-compatible setter for a property.
  void emitObjCPropertySetter(SILDeclRef setter);

  //===--------------------------------------------------------------------===//
  // Control flow
  //===--------------------------------------------------------------------===//
  
  /// emitCondition - Emit a boolean expression as a control-flow condition.
  ///
  /// \param Loc - The statement being lowered, for source information on
  ///        the branch.
  /// \param E - The expression to be evaluated as a condition.
  /// \param hasFalseCode - true if the false branch doesn't just lead
  ///        to the fallthrough.
  /// \param invertValue - true if this routine should invert the value before
  ///        testing true/false.
  /// \param contArgs - the types of the arguments to the continuation BB.
  ///        Matching argument values must be passed to exitTrue and exitFalse
  ///        of the resulting Condition object.
  Condition emitCondition(SILLocation Loc, Expr *E,
                          bool hasFalseCode = true, bool invertValue = false,
                          ArrayRef<SILType> contArgs = {});
  
  
  //===--------------------------------------------------------------------===//
  // Memory management
  //===--------------------------------------------------------------------===//
  
  /// emitProlog - Generates prolog code to allocate and clean up mutable
  /// storage for closure captures and local arguments.
  void emitProlog(CapturingExpr *ce, ArrayRef<Pattern*> paramPatterns,
                  Type resultType);
  void emitProlog(ArrayRef<Pattern*> paramPatterns,
                  Type resultType);

  /// \brief Create (but do not emit) the epilog branch, and save the
  /// current cleanups depth as the destination for return statement branches.
  ///
  /// \param returnType  If non-null, the epilog block will be created with an
  ///                    argument of this type to receive the return value for
  ///                    the function.
  /// \param L           The SILLocation which should be accosocated with
  ///                    cleanup instructions.
  void prepareEpilog(Type returnType, CleanupLocation L);
  
  /// \brief Branch to and emit the epilog basic block. This will fuse
  /// the epilog to the current basic block if the epilog bb has no predecessor.
  /// The insertion point will be moved into the epilog block if it is
  /// reachable.
  ///
  /// \param TopLevelLoc The location of the top level AST node for which we are
  ///            constructing the epilog, such as a CapturingExpr.
  /// \returns Nothing if the epilog block is unreachable. Otherwise, returns
  ///          the epilog block's return value argument, or a null SILValue if
  ///          the epilog doesn't take a return value. Also returns the location
  ///          of the return instrcution if the epilog block is supposed to host
  ///          the ReturnLocation (This happens in case the predecessor block is
  ///          merged with the epilog block.)
  std::pair<Optional<SILValue>,
            Optional<SILLocation>> emitEpilogBB(SILLocation TopLevelLoc);
  
  /// \brief Emits a standard epilog which runs top-level cleanups then returns
  /// the function return value, if any.
  ///
  /// \param TopLevelLoc The location of the top-level expression during whose
  ///        evaluation the epilog is being produced, for example, the
  ///        CapturingExpr.
  /// \param IsAutoGen Flags if the prolog is auto-generated.
  void emitEpilog(SILLocation TopLevelLoc, bool IsAutoGen = false);
  
  /// emitDestructorProlog - Generates prolog code for a destructor. Unlike
  /// a normal function, the destructor does not consume a reference to its
  /// argument. Returns the 'self' argument SILValue.
  SILValue emitDestructorProlog(ClassDecl *CD, DestructorDecl *DD);
  
  /// Emits a temporary allocation that will be deallocated automatically at the
  /// end of the current scope. Returns the address of the allocation.
  SILValue emitTemporaryAllocation(SILLocation loc, SILType ty);
  
  /// Prepares a buffer to receive the result of an expression, either using the
  /// 'emit into' initialization buffer if available, or allocating a temporary
  /// allocation if not.
  SILValue getBufferForExprResult(SILLocation loc, SILType ty, SGFContext C);
  
  //===--------------------------------------------------------------------===//
  // Recursive entry points
  //===--------------------------------------------------------------------===//

  using ASTVisitorType::visit;
  
  //===--------------------------------------------------------------------===//
  // Statements
  //===--------------------------------------------------------------------===//
  
  void visitBraceStmt(BraceStmt *S);
  
  void visitReturnStmt(ReturnStmt *S);
  
  void visitIfStmt(IfStmt *S);
  
  void visitWhileStmt(WhileStmt *S);
  
  void visitDoWhileStmt(DoWhileStmt *S);
  
  void visitForStmt(ForStmt *S);
  
  void visitForEachStmt(ForEachStmt *S);
  
  void visitBreakStmt(BreakStmt *S);
  
  void visitContinueStmt(ContinueStmt *S);
  
  void visitFallthroughStmt(FallthroughStmt *S);
  
  void visitSwitchStmt(SwitchStmt *S);

  void visitCaseStmt(CaseStmt *S);
  
  //===--------------------------------------------------------------------===//
  // Patterns
  //===--------------------------------------------------------------------===//

  void emitSwitchStmt(SwitchStmt *S);
  void emitSwitchFallthrough(FallthroughStmt *S);
  
  //===--------------------------------------------------------------------===//
  // Expressions
  //===--------------------------------------------------------------------===//
 
  RValue visit(Expr *E) = delete;
 
  /// Generate SIL for the given expression, storing the final result into the
  /// specified Initialization buffer(s). This avoids an allocation and copy if
  /// the result would be allocated into temporary memory normally.
  void emitExprInto(Expr *E, Initialization *I);

  RValue emitRValue(Expr *E, SGFContext C = SGFContext());
  
  ManagedValue emitArrayInjectionCall(ManagedValue ObjectPtr,
                                      SILValue BasePtr,
                                      SILValue Length,
                                      Expr *ArrayInjectionFunction,
                                      SILLocation Loc);
                        
  /// Emit the empty tuple value by emitting
  SILValue emitEmptyTuple(SILLocation loc);
  /// "Emit" an RValue representing an empty tuple.
  RValue emitEmptyTupleRValue(SILLocation loc);

  /// Returns a reference to a constant in global context. For local func decls
  /// this returns the function constant with unapplied closure context.
  SILValue emitGlobalFunctionRef(SILLocation loc, SILDeclRef constant);
  /// Returns a reference to a constant in local context. This will return a
  /// closure object reference if the constant refers to a local func decl.
  /// In rvalue contexts, emitFunctionRef should be used instead, which retains
  /// a local constant and returns a ManagedValue with a cleanup.
  SILValue emitUnmanagedFunctionRef(SILLocation loc, SILDeclRef constant);
  /// Returns a reference to a constant in local context. This will return a
  /// retained closure object reference if the constant refers to a local func
  /// decl.
  ManagedValue emitFunctionRef(SILLocation loc, SILDeclRef constant);

  ManagedValue emitReferenceToDecl(SILLocation loc,
                               ValueDecl *decl,
                               Type declType = Type(),
                               unsigned uncurryLevel
                                 = SILDeclRef::ConstructAtNaturalUncurryLevel);

  ManagedValue emitClosureForCapturingExpr(SILLocation loc,
                                           SILDeclRef function,
                                           ArrayRef<Substitution> forwardSubs,
                                           CapturingExpr *body);
  
  Materialize emitMaterialize(SILLocation loc, ManagedValue v);
  ManagedValue emitGetProperty(SILLocation loc,
                               SILDeclRef getter,
                               ArrayRef<Substitution> substitutions,
                               RValue &&optionalSelfValue,
                               RValue &&optionalSubscripts,
                               Type resultType,
                               SGFContext C);
  void emitSetProperty(SILLocation loc,
                       SILDeclRef setter,
                       ArrayRef<Substitution> substitutions,
                       RValue &&optionalSelfValue,
                       RValue &&optionalSubscripts,
                       RValue &&value);

  ManagedValue emitManagedRetain(SILLocation loc, SILValue v);
  ManagedValue emitManagedRetain(SILLocation loc, SILValue v,
                                 const TypeLowering &lowering);
  
  ManagedValue emitManagedRValueWithCleanup(SILValue v);
  ManagedValue emitManagedRValueWithCleanup(SILValue v,
                                            const TypeLowering &lowering);

  void emitSemanticLoadInto(SILLocation loc, SILValue src,
                            const TypeLowering &srcLowering,
                            SILValue dest,
                            const TypeLowering &destLowering,
                            IsTake_t isTake, IsInitialization_t isInit);

  SILValue emitSemanticLoad(SILLocation loc, SILValue src,
                            const TypeLowering &srcLowering,
                            const TypeLowering &rvalueLowering,
                            IsTake_t isTake);

  void emitSemanticInitialize(SILLocation loc, SILValue value,
                              SILValue dest,
                              const TypeLowering &destTL);

  void emitSemanticAssignment(SILLocation loc, SILValue value,
                              SILValue dest,
                              const TypeLowering &destTL,
                              bool canBeDefinitiveInit);
  
  ManagedValue emitLoad(SILLocation loc, SILValue addr,
                        const TypeLowering &rvalueTL,
                        SGFContext C, IsTake_t isTake);
  
  void emitAssignToLValue(SILLocation loc, RValue &&src,
                          LValue const &dest);
  ManagedValue emitAddressOfLValue(SILLocation loc, LValue const &src);
  ManagedValue emitLoadOfLValue(SILLocation loc, const LValue &src,
                                SGFContext C);
  ManagedValue emitMethodRef(SILLocation loc,
                             SILValue selfValue,
                             SILDeclRef methodConstant,
                             ArrayRef<Substitution> innerSubstitutions);
  
  SILValue emitMetatypeOfValue(SILLocation loc, SILValue base);
  
  void emitReturnExpr(SILLocation loc, Expr *ret);
  
  /// Convert a value with a specialized representation (such as a thin function
  /// reference, or a function reference with a foreign calling convention) to
  /// the generalized representation of its Swift type, which can then be stored
  /// to a variable or passed as an argument or return value.
  SILValue emitGeneralizedValue(SILLocation loc, SILValue thinFn);
  
  /// Convert a native Swift value to a value that can be passed as an argument
  /// to or returned as the result of a function with the given calling
  /// convention.
  ManagedValue emitNativeToBridgedValue(SILLocation loc, ManagedValue v,
                                        AbstractCC destCC,
                                        CanType bridgedTy);
  
  /// Convert a value received as the result or argument of a function with
  /// the given calling convention to a native Swift value of the given type.
  ManagedValue emitBridgedToNativeValue(SILLocation loc, ManagedValue v,
                                        AbstractCC srcCC,
                                        CanType nativeTy);

  //
  // Helpers for emitting ApplyExpr chains.
  //
  
  RValue emitApplyExpr(ApplyExpr *e, SGFContext c);

  ManagedValue emitApply(SILLocation Loc, ManagedValue Fn,
                         ArrayRef<ManagedValue> Args,
                         CanType NativeResultTy,
                         OwnershipConventions const &Ownership,
                         bool ForceInline = false,
                         SGFContext C = SGFContext());

  ManagedValue emitApplyOfLibraryIntrinsic(SILLocation loc,
                                           FuncDecl *fn,
                                           ArrayRef<ManagedValue> args,
                                           CanType resultType,
                                           SGFContext ctx);

  /// \brief Emit the cast instruction appropriate to the kind of checked cast.
  ///
  /// \param loc          The AST location associated with the operation.
  /// \param originalMV   The value to cast.
  /// \param origTy       The original AST-level type.
  /// \param castTy       The destination type.
  /// \param kind         The semantics of the cast.
  /// \param mode         Whether to emit an unconditional or conditional cast.
  /// \param useCastValue If true, the cleanup on the original value will be
  ///                     disabled, and the callee will be expected to take
  ///                     ownership of the returned value. If false, the original
  ///                     value's cleanup is left intact, and an unowned reference
  ///                     or address is returned.
  SILValue emitCheckedCast(SILLocation loc,
                           ManagedValue originalMV,
                           Type origTy,
                           Type castTy,
                           CheckedCastKind kind,
                           CheckedCastMode mode,
                           bool useCastValue);

  //===--------------------------------------------------------------------===//
  // Declarations
  //===--------------------------------------------------------------------===//
  
  void visitDecl(Decl *D) {
    D->dump();
    llvm_unreachable("Not yet implemented");
  }

  void visitNominalTypeDecl(NominalTypeDecl *D);
  void visitFuncDecl(FuncDecl *D);
  void visitPatternBindingDecl(PatternBindingDecl *D);
    
  void visitTypeAliasDecl(TypeAliasDecl *D) {
    // No lowering support needed.
  }

  void visitGenericTypeParamDecl(GenericTypeParamDecl *D) {
    // No lowering support needed.
  }
  void visitAssociatedTypeDecl(AssociatedTypeDecl *D) {
    // No lowering support needed.
  }

  void visitVarDecl(VarDecl *D) {
    // We handle these in pattern binding.
  }
  
  /// Emit the allocation for a local variable. Returns the address of the
  /// value. Does not register a cleanup.
  void emitLocalVariable(VarDecl *D);
  
  /// Emit the allocation for a local variable, provides an Initialization
  /// that can be used to initialize it, and registers cleanups in the active
  /// scope.
  std::unique_ptr<Initialization> emitLocalVariableWithCleanup(VarDecl *D);
  
  /// Destroy and deallocate an initialized local variable.
  void destroyLocalVariable(SILLocation L, VarDecl *D);
  
  /// Deallocate an uninitialized local variable.
  void deallocateUninitializedLocalVariable(SILLocation L, VarDecl *D);

  /// Enter a cleanup to deallocate a stack variable.
  CleanupsDepth enterDeallocStackCleanup(SILLocation loc, SILValue address);
  
  /// Evaluate an Expr as an lvalue.
  LValue emitLValue(Expr *E);
  
  /// Evaluate an Expr as an lvalue, and take the materialized address as an
  /// rvalue.
  RValue emitLValueAsRValue(Expr *E);
  
  /// Build an identity substitution map for the given set of archetypes.
  ArrayRef<Substitution>
  buildForwardingSubstitutions(ArrayRef<ArchetypeType *> params);
  
  /// Return forwarding substitutions for the archetypes in the current
  /// function.
  ArrayRef<Substitution> getForwardingSubstitutions();
};
  
} // end namespace Lowering
} // end namespace swift

#endif
