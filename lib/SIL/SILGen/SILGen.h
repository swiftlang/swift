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
#include "Scope.h"
#include "swift/SIL/Function.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {
  class BasicBlock;
  
namespace Lowering {
  class Condition;
  class LValue;
  class ManagedValue;
  class TypeConverter;
  class SILGenFunction;
  class Initialization;
  struct Writeback;

/// SILGenModule - an ASTVisitor for generating SIL from top-level declarations
/// in a translation unit.
class LLVM_LIBRARY_VISIBILITY SILGenModule : public ASTVisitor<SILGenModule> {
public:
  /// The Module being constructed.
  SILModule &M;
  
  /// Types - This creates and manages TypeLoweringInfo objects containing extra
  /// information about types needed for SIL generation.
  TypeConverter Types;
  
  /// TopLevelSGF - The SILGenFunction used to visit top-level code, or null if
  /// the module is not a main module.
  SILGenFunction /*nullable*/ *TopLevelSGF;
  
public:
  SILGenModule(SILModule &M);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;
  
  /// Returns the type of a constant reference.
  SILType getConstantType(SILConstant constant);
  
  /// Get the lowered type for a Swift type.
  SILType getLoweredType(Type t) {
    return Types.getTypeLoweringInfo(t).getLoweredType();
  }

  //===--------------------------------------------------------------------===//
  // Visitors for top-level forms
  //===--------------------------------------------------------------------===//
  // FIXME: visit other decls
  void visitFuncDecl(FuncDecl *fd);
  void visitPatternBindingDecl(PatternBindingDecl *vd);
  void visitTopLevelCodeDecl(TopLevelCodeDecl *td);
  void visitNominalTypeDecl(NominalTypeDecl *ntd);

  /// emitFunction - Generates code for the given FuncExpr and adds the
  /// Function to the current SILModule under the name SILConstant(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  Function *emitFunction(SILConstant::Loc decl, FuncExpr *fe);
  /// emitClosure - Generates code for the given ClosureExpr and adds the
  /// Function to the current SILModule under the name SILConstant(ce).
  Function *emitClosure(ClosureExpr *ce);
  /// emitConstructor - Generates code for the given ConstructorDecl and adds
  /// the Function to the current SILModule under the name SILConstant(decl).
  Function *emitConstructor(ConstructorDecl *decl);
  /// emitDestructor - Generates code for the given class's destructor and adds
  /// the Function to the current SILModule under the name
  /// SILConstant(cd, Destructor). If a DestructorDecl is provided, it will be
  /// used, otherwise only the implicit destruction behavior will be emitted.
  Function *emitDestructor(ClassDecl *cd,
                           DestructorDecl /*nullable*/ *dd);
  
  template<typename T>
  Function *preEmitFunction(SILConstant constant, T *astNode);
  void postEmitFunction(SILConstant constant, Function *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);
  
  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalDefinition(Decl *d);
};
  
/// SILGenType - an ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class LLVM_LIBRARY_VISIBILITY SILGenType : public ASTVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  NominalTypeDecl *theType;
  DestructorDecl *explicitDestructor;
  
public:
  SILGenType(SILGenModule &SGM, NominalTypeDecl *theType);
  ~SILGenType();

  /// Emit SIL functions for all the members of the type.
  void emitType();
  
  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitNominalTypeDecl(NominalTypeDecl *ntd);
  void visitFuncDecl(FuncDecl *fd);
  void visitConstructorDecl(ConstructorDecl *cd);
  void visitDestructorDecl(DestructorDecl *dd);
  // FIXME: other special members?
  
  // no-ops. We don't deal with the layout of types here.
  void visitPatternBindingDecl(PatternBindingDecl *) {}
  void visitVarDecl(VarDecl *) {}
  
};

/// Materialize - Represents a temporary allocation.
struct Materialize {
  /// The address of the allocation.
  Value address;
  /// The cleanup to dispose of the value before deallocating the buffer.
  /// This cleanup can be killed by calling the consume method.
  CleanupsDepth valueCleanup;
  
  /// Load the value out of the temporary buffer and deactivate its value
  /// cleanup. Returns the loaded value, which becomes the caller's
  /// responsibility to release.
  ManagedValue consume(SILGenFunction &gen, SILLocation loc);
};
  
/// SGFContext - Internal context information for the SILGenFunction visitor.
struct SGFContext {
private:
  using EmitIntoOrArgumentVectorPointer =
    PointerUnion<Initialization*, SmallVectorImpl<Value>*>;
  
  using State =
    llvm::PointerIntPair<EmitIntoOrArgumentVectorPointer, 1, bool>;
  
  State state;
public:
  SGFContext() = default;
  
  /// Creates an emitInto context that will store the result of the visited expr
  /// into the given Initialization.
  SGFContext(Initialization *emitInto)
    : state(EmitIntoOrArgumentVectorPointer(emitInto), false)
  {}
  
  /// Creates an argument tuple context that will recursively append tuple
  /// elements to the given argument vector.
  SGFContext(SmallVectorImpl<Value> &argumentVector)
    : state(EmitIntoOrArgumentVectorPointer(&argumentVector), false)
  {}
  
  /// Creates a load expr context, in which a temporary lvalue that would
  /// normally be materialized can be left as an rvalue to avoid a useless
  /// immediately-consumed allocation.
  SGFContext(bool isChildOfLoadExpr)
    : state(EmitIntoOrArgumentVectorPointer(), isChildOfLoadExpr)
  {}

  /// Returns a pointer to the Initialization that the current expression should
  /// store its result to, or null if the expression should allocate temporary
  /// storage for its result.
  Initialization *getEmitInto() const {
    return state.getPointer().dyn_cast<Initialization*>();
  }
  
  /// Returns a pointer to the argument vector the current tuple expression
  /// should destructure its result elements into, or null if the expression
  /// should return a tuple value.
  SmallVectorImpl<Value> *getArgumentVector() const {
    return state.getPointer().dyn_cast<SmallVectorImpl<Value>*>();
  }
  
  /// Returns true if the current expression is a child of a LoadExpr, and
  /// should thus avoid emitting a temporary materialization if possible.
  bool isChildOfLoadExpr() const {
    return state.getInt();
  }
};
  
/// SILGenFunction - an ASTVisitor for producing SIL from function bodies.
class LLVM_LIBRARY_VISIBILITY SILGenFunction
  : public ASTVisitor<SILGenFunction,
                      /*ExprRetTy=*/ ManagedValue,
                      /*StmtRetTy=*/ void,
                      /*DeclRetTy=*/ void,
                      /*PatternRetTy=*/ void,
                      /*Args...=*/ SGFContext> {
public:
  /// The SILGenModule this function belongs to.
  SILGenModule &SGM;
    
  /// The Function being constructed.
  Function &F;
  
  /// B - The SILBuilder used to construct the Function.  It is what maintains
  /// the notion of the current block being emitted into.
  SILBuilder B;
    
  /// IndirectReturnAddress - For a function with an indirect return, holds a
  /// value representing the address to initialize with the return value. Null
  /// for a function that returns by value.
  Value IndirectReturnAddress;
  
  std::vector<JumpDest> BreakDestStack;
  std::vector<JumpDest> ContinueDestStack;

  /// Cleanups - This records information about the currently active cleanups.
  CleanupManager Cleanups;

  /// VarLoc - representation of an emitted local variable.
  struct VarLoc {
    /// box - the retainable box for the variable, or invalid if no box was
    /// made for the value.
    Value box;
    /// address - the address at which the variable is stored.
    Value address;
  };
    
  /// VarLocs - Entries in this map are generated when a PatternBindingDecl is
  /// emitted. The map is queried to produce the lvalue for a DeclRefExpr to
  /// a local variable.
  llvm::DenseMap<ValueDecl*, VarLoc> VarLocs;
    
  /// LocalConstants - Entries in this map are generated when a local constant
  /// declaration that requires local context, such as a func closure, is
  /// emitted. This map is then queried to produce the value for a DeclRefExpr
  /// to a local constant.
  llvm::DenseMap<SILConstant, Value> LocalConstants;
  
  /// True if 'return' without an operand or falling off the end of the current
  /// function is valid.
  bool hasVoidReturn;
  
  /// In a constructor or destructor context, returning from the body doesn't
  /// return from the current SIL function but instead branches to an epilog
  /// block that executes implicit behavior. epilogBB points to the epilog
  /// block that 'return' jumps to in those contexts, or 'null' if returning
  /// can return normally from the function.
  BasicBlock *epilogBB;

public:
  SILGenFunction(SILGenModule &SGM, Function &F, bool hasVoidReturn);
  ~SILGenFunction();
  
  /// Return a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Cleanups.getCleanupsDepth();
  }
  
  Function &getFunction() { return F; }
  SILBuilder &getBuilder() { return B; }
  
  TypeLoweringInfo const &getTypeLoweringInfo(Type t,
                                              unsigned uncurryLevel = 0) {
    return SGM.Types.getTypeLoweringInfo(t, uncurryLevel);
  }
  SILType getLoweredType(Type t, unsigned uncurryLevel = 0) {
    return getTypeLoweringInfo(t, uncurryLevel).getLoweredType();
  }
  SILType getLoweredLoadableType(Type t, unsigned uncurryLevel = 0) {
    TypeLoweringInfo const &ti = getTypeLoweringInfo(t, uncurryLevel);
    assert(ti.isLoadable() && "unexpected address-only type");
    return ti.getLoweredType();
  }

  //===--------------------------------------------------------------------===//
  // Entry points for codegen
  //===--------------------------------------------------------------------===//
  
  /// emitFunction - Generates code for a FuncExpr.
  void emitFunction(FuncExpr *fe);
  /// emitClosure - Generates code for a ClosureExpr. This is akin
  /// to visiting the body as if wrapped in a ReturnStmt.
  void emitClosure(ClosureExpr *ce);
  /// emitDestructor - Generates code for a class destructor. This emits the
  /// body code from the DestructorDecl (if any),
  /// implicitly releases the elements of the class, and calls the base
  /// class destructor or deallocates the instance if this is a root class.
  void emitDestructor(ClassDecl *cd, DestructorDecl *dd);
  /// emitValueConstructor - Generates code for a struct constructor.
  /// This allocates the new 'this' value, emits the
  /// body code, then returns the final initialized 'this'.
  void emitValueConstructor(ConstructorDecl *ctor);
  /// emitClassConstructorAllocator - Generates code for a class constructor's
  /// allocating entry point. This allocates the new 'this' value, passes it to
  /// the initializer entry point, then returns the initialized 'this'.
  void emitClassConstructorAllocator(ConstructorDecl *ctor);
  /// emitClassConstructorInitializer - Generates code for a class constructor's
  /// initializing entry point. This takes 'this' and the constructor arguments
  /// as parameters and executes the constructor body to initialize 'this'.
  void emitClassConstructorInitializer(ConstructorDecl *ctor);

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
  
  
  /// emitBranch - Emit a branch to the given jump destination, threading out
  /// through any cleanups we might need to run.
  void emitBranch(JumpDest D);
  
  /// emitEpilogBB - Branch to and emit the epilog basic block. This will fuse
  /// the epilog to the current basic block if the epilog bb has no predecessor.
  /// Returns true if the epilog is reachable, false if it is unreachable.
  bool emitEpilogBB(SILLocation loc);
  
  //===--------------------------------------------------------------------===//
  // Memory management
  //===--------------------------------------------------------------------===//
  
  /// emitProlog - Generates prolog code to allocate and clean up mutable
  /// storage for closure captures and local arguments.
  void emitProlog(CapturingExpr *ce, ArrayRef<Pattern*> paramPatterns,
                  Type resultType);
  void emitProlog(ArrayRef<Pattern*> paramPatterns,
                  Type resultType);
  
  /// emitDestructorProlog - Generates prolog code for a destructor. Unlike
  /// a normal function, the destructor does not consume a reference to its
  /// argument. Returns the 'this' argument Value.
  Value emitDestructorProlog(ClassDecl *CD, DestructorDecl *DD);
  
  /// emitRetainRValue - Emits the instructions necessary to increase the
  /// retain count of a temporary, in order to use it as part of another
  /// independent temporary.
  /// - For trivial loadable types, this is a no-op.
  /// - For reference types, 'v' is retained.
  /// - For loadable types with reference type members, the reference type
  ///   members are all retained.
  /// - FIXME For address-only types, this currently crashes. It should instead
  ///   allocate and return a copy made using copy_addr.
  void emitRetainRValue(SILLocation loc, Value v);
    
  /// emitReleaseRValue - Emits the instructions necessary to clean up a
  /// temporary value.
  /// - For trivial loadable types, this is a no-op.
  /// - For reference types, 'v' is released.
  /// - For loadable types with reference type members, the reference type
  ///   members are all released.
  /// - For address-only types, the value at the address is destroyed using
  ///   a destroy_addr instruction.
  void emitReleaseRValue(SILLocation loc, Value v);

  /// Emits a temporary allocation that will be deallocated automatically at the
  /// end of the current scope. Returns the address of the allocation.
  Value emitTemporaryAllocation(SILLocation loc, SILType ty);
  
  /// Prepares a buffer to receive the result of an expression, either using the
  /// 'emit into' initialization buffer if available, or allocating a temporary
  /// allocation if not.
  Value getBufferForExprResult(SILLocation loc, SILType ty, SGFContext C);
  
  /// Emits a reassignment to a physical address.
  void emitAssignPhysicalAddress(SILLocation loc,
                                 ManagedValue src,
                                 Value addr);
  
  //===--------------------------------------------------------------------===//
  // Recursive entry points
  //===--------------------------------------------------------------------===//

  using ASTVisitorType::visit;
  
  ManagedValue visit(Expr *E);
  void visit(Stmt *S) { visit(S, SGFContext()); }
  void visit(Decl *D) { visit(D, SGFContext()); }
  
  //===--------------------------------------------------------------------===//
  // Statements
  //===--------------------------------------------------------------------===//
  
  void visitBraceStmt(BraceStmt *S, SGFContext C);
  
  void visitAssignStmt(AssignStmt *S, SGFContext C);

  void visitReturnStmt(ReturnStmt *S, SGFContext C);
  
  void visitIfStmt(IfStmt *S, SGFContext C);
  
  void visitWhileStmt(WhileStmt *S, SGFContext C);
  
  void visitDoWhileStmt(DoWhileStmt *S, SGFContext C);
  
  void visitForStmt(ForStmt *S, SGFContext C);
  
  void visitForEachStmt(ForEachStmt *S, SGFContext C);
  
  void visitBreakStmt(BreakStmt *S, SGFContext C);
  
  void visitContinueStmt(ContinueStmt *S, SGFContext C);
  
  void visitSwitchStmt(SwitchStmt *S, SGFContext C);

  void visitCaseStmt(CaseStmt *S, SGFContext C);
  
  //===--------------------------------------------------------------------===//
  // Expressions
  //===--------------------------------------------------------------------===//
  
  /// Generate SIL for the given expression, storing the final result into the
  /// specified Initialization buffer(s). This avoids an allocation and copy if
  /// the result would be allocated into temporary memory normally.*
  ///
  /// *FIXME: does not actually avoid copy yet!
  void emitExprInto(Expr *E, Initialization *I);
  
  ManagedValue visitApplyExpr(ApplyExpr *E, SGFContext C);

  ManagedValue visitDeclRefExpr(DeclRefExpr *E, SGFContext C);
  ManagedValue visitSuperRefExpr(SuperRefExpr *E, SGFContext C);
  ManagedValue visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E,
                                                SGFContext C);
  
  ManagedValue visitIntegerLiteralExpr(IntegerLiteralExpr *E, SGFContext C);
  ManagedValue visitFloatLiteralExpr(FloatLiteralExpr *E, SGFContext C);
  ManagedValue visitCharacterLiteralExpr(CharacterLiteralExpr *E, SGFContext C);
  ManagedValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
  ManagedValue visitLoadExpr(LoadExpr *E, SGFContext C);
  ManagedValue visitMaterializeExpr(MaterializeExpr *E, SGFContext C);
  ManagedValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
  ManagedValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                           SGFContext C);
  ManagedValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
  ManagedValue visitRequalifyExpr(RequalifyExpr *E, SGFContext C);
  ManagedValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                           SGFContext C);
  ManagedValue visitErasureExpr(ErasureExpr *E, SGFContext C);
  ManagedValue visitCoerceExpr(CoerceExpr *E, SGFContext C);
  ManagedValue visitUncheckedDowncastExpr(UncheckedDowncastExpr *E, SGFContext C);
  ManagedValue visitUncheckedSuperToArchetypeExpr(
                                UncheckedSuperToArchetypeExpr *E, SGFContext C);
  ManagedValue visitIsSubtypeExpr(IsSubtypeExpr *E, SGFContext C);
  ManagedValue visitSuperIsArchetypeExpr(SuperIsArchetypeExpr *E, SGFContext C);
  ManagedValue visitParenExpr(ParenExpr *E, SGFContext C);
  ManagedValue visitTupleExpr(TupleExpr *E, SGFContext C);
  ManagedValue visitScalarToTupleExpr(ScalarToTupleExpr *E, SGFContext C);
  ManagedValue visitSpecializeExpr(SpecializeExpr *E, SGFContext C);
  ManagedValue visitAddressOfExpr(AddressOfExpr *E, SGFContext C);
  ManagedValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
  ManagedValue visitGenericMemberRefExpr(GenericMemberRefExpr *E, SGFContext C);
  ManagedValue visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                           SGFContext C);
  ManagedValue visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E,
                                             SGFContext C);
  ManagedValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                             SGFContext C);
  ManagedValue visitModuleExpr(ModuleExpr *E, SGFContext C);
  ManagedValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
  ManagedValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
  ManagedValue visitGenericSubscriptExpr(GenericSubscriptExpr *E, SGFContext C);
  ManagedValue visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E,
                                           SGFContext C);
  ManagedValue visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E,
                                             SGFContext C);
  ManagedValue visitTupleShuffleExpr(TupleShuffleExpr *E, SGFContext C);
  ManagedValue visitNewArrayExpr(NewArrayExpr *E, SGFContext C);
  ManagedValue visitMetatypeExpr(MetatypeExpr *E, SGFContext C);
  ManagedValue visitFuncExpr(FuncExpr *E, SGFContext C);
  ManagedValue visitClosureExpr(ClosureExpr *E, SGFContext C);
  ManagedValue visitInterpolatedStringLiteralExpr(
                                              InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
  ManagedValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
  ManagedValue visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E,
                                                SGFContext C);
  ManagedValue visitBridgeToBlockExpr(BridgeToBlockExpr *E, SGFContext C);
  ManagedValue visitIfExpr(IfExpr *E, SGFContext C);

  void emitApplyArgumentValue(SILLocation loc,
                              ManagedValue argValue,
                              llvm::SmallVectorImpl<Value> &argsV);
  void emitApplyArguments(Expr *argsExpr,
                          llvm::SmallVectorImpl<Value> &args);

  ManagedValue emitApply(SILLocation Loc, Value Fn, ArrayRef<Value> Args);
  ManagedValue emitArrayInjectionCall(Value ObjectPtr,
                                      Value BasePtr,
                                      Value Length,
                                      Expr *ArrayInjectionFunction);
  ManagedValue emitTupleShuffleOfExprs(Expr *E,
                                       ArrayRef<Expr *> InExprs,
                                       ArrayRef<int> ElementMapping,
                                       Expr *VarargsInjectionFunction,
                                       SGFContext C);
  ManagedValue emitTupleShuffle(Expr *E,
                                ArrayRef<Value> InElts,
                                ArrayRef<int> ElementMapping,
                                Expr *VarargsInjectionFunction);
  ManagedValue emitTuple(Expr *E,
                         ArrayRef<Expr *> Elements,
                         Type VarargsBaseTy,
                         ArrayRef<Value> VariadicElements,
                         Expr *VarargsInjectionFunction,
                         SGFContext C);

  /// Returns a reference to a constant in global context. For local func decls
  /// this returns the function constant with unapplied closure context.
  Value emitGlobalConstantRef(SILLocation loc, SILConstant constant);
  /// Returns a reference to a constant in local context. This will return a
  /// closure object reference if the constant refers to a local func decl.
  /// In rvalue contexts, emitConstantRef should be used instead, which retains
  /// a local constant and returns a ManagedValue with a cleanup.
  Value emitUnmanagedConstantRef(SILLocation loc, SILConstant constant);
  /// Returns a reference to a constant in local context. This will return a
  /// retained closure object reference if the constant refers to a local func
  /// decl.
  ManagedValue emitConstantRef(SILLocation loc, SILConstant constant);

  ManagedValue emitReferenceToDecl(SILLocation loc,
                                   ValueDecl *decl);

  ManagedValue emitClosureForCapturingExpr(SILLocation loc,
                                           SILConstant function,
                                           CapturingExpr *body);
  
  Materialize emitMaterialize(SILLocation loc, ManagedValue v);
  Materialize emitGetProperty(SILLocation loc,
                              ManagedValue getter,
                              Optional<ManagedValue> thisValue,
                              Optional<ArrayRef<Value>> subscripts);
  void emitSetProperty(SILLocation loc,
                       ManagedValue setter,
                       Optional<ManagedValue> thisValue,
                       Optional<ArrayRef<Value>> subscripts,
                       ManagedValue value);
  
  ManagedValue emitManagedRValueWithCleanup(Value v);
  
  void emitAssignToLValue(SILLocation loc, ManagedValue src,
                          LValue const &dest);
  ManagedValue emitMaterializedLoadFromLValue(SILLocation loc,
                                              LValue const &src);
  ManagedValue emitSpecializedPropertyConstantRef(Expr *expr, Expr *baseExpr,
                                            Expr /*nullable*/ *subscriptExpr,
                                            SILConstant constant,
                                            ArrayRef<Substitution> substs);
  ManagedValue emitMethodRef(SILLocation loc,
                             Value thisValue,
                             SILConstant methodConstant);
  
  Value emitThickenFunction(SILLocation loc, Value thinFn);
  void emitStore(SILLocation loc, ManagedValue src, Value destAddr);
  
  //
  // Helpers for emitting ApplyExpr chains.
  //
  
  Value emitArchetypeMethod(ArchetypeMemberRefExpr *e, Value archetype);
  Value emitProtocolMethod(ExistentialMemberRefExpr *e, Value existential);
  
  ManagedValue emitApplyExpr(ApplyExpr *e);

  //===--------------------------------------------------------------------===//
  // Declarations
  //===--------------------------------------------------------------------===//
  
  void visitDecl(Decl *D, SGFContext C) {
    D->dump();
    llvm_unreachable("Not yet implemented");
  }

  void visitNominalTypeDecl(NominalTypeDecl *D, SGFContext C);
  void visitFuncDecl(FuncDecl *D, SGFContext C);
  void visitPatternBindingDecl(PatternBindingDecl *D, SGFContext C);
    
  void visitTypeAliasDecl(TypeAliasDecl *D, SGFContext C) {
    // No lowering support needed.
  }
    
  void visitVarDecl(VarDecl *D, SGFContext C) {
    // We handle these in pattern binding.
  }
};
  
/// SILGenLValue - An ASTVisitor for building logical lvalues.
/// Used to visit the left-hand sides of AssignStmts and [byref] arguments of
/// ApplyExprs.
class LLVM_LIBRARY_VISIBILITY SILGenLValue
  : public ExprVisitor<SILGenLValue, LValue>
{
  SILGenFunction &gen;
public:
  SILGenLValue(SILGenFunction &gen) : gen(gen) {}
  
  LValue visitRec(Expr *e);
  
  /// Dummy handler to log unimplemented nodes.
  LValue visitExpr(Expr *e);

  // Nodes that form the root of lvalue paths

  LValue visitDeclRefExpr(DeclRefExpr *e);
  LValue visitMaterializeExpr(MaterializeExpr *e);

  // Nodes that make up components of lvalue paths
  
  LValue visitMemberRefExpr(MemberRefExpr *e);
  LValue visitGenericMemberRefExpr(GenericMemberRefExpr *e);
  LValue visitSubscriptExpr(SubscriptExpr *e);
  LValue visitGenericSubscriptExpr(GenericSubscriptExpr *e);
  LValue visitTupleElementExpr(TupleElementExpr *e);
  
  // Expressions that wrap lvalues
  
  LValue visitAddressOfExpr(AddressOfExpr *e);
  LValue visitParenExpr(ParenExpr *e);
  LValue visitRequalifyExpr(RequalifyExpr *e); // FIXME kill lvalue qualifiers
  LValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e);
};
  
} // end namespace Lowering
} // end namespace swift

#endif
