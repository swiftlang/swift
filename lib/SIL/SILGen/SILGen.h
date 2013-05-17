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
  class LValue;
  class RValue;
  class ManagedValue;
  class TypeConverter;
  class SILGenFunction;
  class Initialization;
  class OwnershipConventions;
  struct Writeback;

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
  
  /// Mapping from SILConstants to emitted SILFunctions.
  llvm::DenseMap<SILConstant, SILFunction*> emittedFunctions;
  
  SILFunction *emitTopLevelFunction();
  
  size_t anonymousFunctionCounter = 0;
  
public:
  SILGenModule(SILModule &M);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;
  
  /// Returns the type of a constant reference.
  SILType getConstantType(SILConstant constant);
  
  /// Returns the calling convention for a function.
  AbstractCC getConstantCC(SILConstant constant) {
    return getConstantType(constant).getFunctionTypeInfo()->getAbstractCC();
  }
  
  /// Determine the linkage of a constant.
  SILLinkage getConstantLinkage(SILConstant constant);
  
  /// Get the function for a SILConstant.
  SILFunction *getFunction(SILConstant constant);
  
  /// Get the lowered type for a Swift type.
  SILType getLoweredType(Type t) {
    return Types.getTypeLoweringInfo(t).getLoweredType();
  }
  
  /// Generate the mangled symbol name for a SILConstant.
  void mangleConstant(SILConstant constant,
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

  /// emitFunction - Generates code for the given FuncExpr and adds the
  /// SILFunction to the current SILModule under the name SILConstant(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  void emitFunction(SILConstant::Loc decl, FuncExpr *fe);
  /// \brief Generates code for the given closure expression and adds the 
  /// SILFunction to the current SILModule under the nane SILConstant(ce).
  void emitClosure(PipeClosureExpr *ce);
  /// emitClosure - Generates code for the given ClosureExpr and adds the
  /// SILFunction to the current SILModule under the name SILConstant(ce).
  void emitClosure(ClosureExpr *ce);
  /// emitConstructor - Generates code for the given ConstructorDecl and adds
  /// the SILFunction to the current SILModule under the name SILConstant(decl).
  void emitConstructor(ConstructorDecl *decl);
  /// emitDestructor - Generates code for the given class's destructor and adds
  /// the SILFunction to the current SILModule under the name
  /// SILConstant(cd, Destructor). If a DestructorDecl is provided, it will be
  /// used, otherwise only the implicit destruction behavior will be emitted.
  void emitDestructor(ClassDecl *cd, DestructorDecl /*nullable*/ *dd);
  
  /// emitCurryThunk - Emits the curry thunk between two uncurry levels of a
  /// function.
  void emitCurryThunk(SILConstant entryPoint,
                      SILConstant nextEntryPoint,
                      FuncExpr *fe);
  
  template<typename T>
  SILFunction *preEmitFunction(SILConstant constant, T *astNode);
  void postEmitFunction(SILConstant constant, SILFunction *F);
  
  /// Add a global variable to the SILModule.
  void addGlobalVariable(VarDecl *global);
  
  /// Emit SIL related to a Clang-imported declaration.
  void emitExternalDefinition(Decl *d);
  
  /// Emit the ObjC-compatible entry point for a method.
  void emitObjCMethodThunk(FuncDecl *method);
  
  /// Emit the ObjC-compatible getter and setter for an instance variable or
  /// property.
  void emitObjCPropertyMethodThunks(VarDecl *prop);
};
  
/// Materialize - Represents a temporary allocation.
struct Materialize {
  /// The address of the allocation.
  SILValue address;
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
  
/// SILGenFunction - an ASTVisitor for producing SIL from function bodies.
class LLVM_LIBRARY_VISIBILITY SILGenFunction
  : public ASTVisitor<SILGenFunction,
                      /*ExprRetTy=*/ RValue,
                      /*StmtRetTy=*/ void,
                      /*DeclRetTy=*/ void,
                      /*PatternRetTy=*/ void,
                      /*Args...=*/ SGFContext>
{ // style violation because Xcode <rdar://problem/13065676>
public:
  /// The SILGenModule this function belongs to.
  SILGenModule &SGM;
    
  /// The SILFunction being constructed.
  SILFunction &F;
  
  /// B - The SILBuilder used to construct the SILFunction.  It is what maintains
  /// the notion of the current block being emitted into.
  SILBuilder B;
    
  /// IndirectReturnAddress - For a function with an indirect return, holds a
  /// value representing the address to initialize with the return value. Null
  /// for a function that returns by value.
  SILValue IndirectReturnAddress;
  
  std::vector<JumpDest> BreakDestStack;
  std::vector<JumpDest> ContinueDestStack;
  std::vector<FallthroughDest> FallthroughDestStack;

  /// Cleanups - This records information about the currently active cleanups.
  CleanupManager Cleanups;

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
  llvm::DenseMap<SILConstant, SILValue> LocalConstants;
  
  /// True if 'return' without an operand or falling off the end of the current
  /// function is valid.
  bool hasVoidReturn;
  
  /// In a constructor or destructor context, returning from the body doesn't
  /// return from the current SIL function but instead branches to an epilog
  /// block that executes implicit behavior. epilogBB points to the epilog
  /// block that 'return' jumps to in those contexts, or 'null' if returning
  /// can return normally from the function.
  SILBasicBlock *epilogBB;

public:
  SILGenFunction(SILGenModule &SGM, SILFunction &F, bool hasVoidReturn);
  ~SILGenFunction();
  
  /// Return a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Cleanups.getCleanupsDepth();
  }
  
  SILFunction &getFunction() { return F; }
  SILBuilder &getBuilder() { return B; }
  
  const TypeLoweringInfo &getTypeLoweringInfo(Type t,
                                      AbstractCC cc = AbstractCC::Freestanding,
                                      unsigned uncurryLevel = 0) {
    return SGM.Types.getTypeLoweringInfo(t, cc, uncurryLevel);
  }
  SILType getLoweredType(Type t,
                         AbstractCC cc = AbstractCC::Freestanding,
                         unsigned uncurryLevel = 0) {
    return getTypeLoweringInfo(t, cc, uncurryLevel).getLoweredType();
  }
  SILType getLoweredLoadableType(Type t,
                                 AbstractCC cc = AbstractCC::Freestanding,
                                 unsigned uncurryLevel = 0) {
    const TypeLoweringInfo &ti = getTypeLoweringInfo(t, cc, uncurryLevel);
    assert(ti.isLoadable() && "unexpected address-only type");
    return ti.getLoweredType();
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
  /// emitCurryThunk - Generates code for a curry thunk from one uncurry level
  /// of a function to another.
  void emitCurryThunk(FuncExpr *fe, SILConstant fromLevel, SILConstant toLevel);


  /// Generate an ObjC-compatible thunk for a method.
  void emitObjCMethodThunk(SILConstant thunk);
  
  /// Generate an ObjC-compatible getter for a property.
  void emitObjCPropertyGetter(SILConstant getter);
  
  /// Generate an ObjC-compatible setter for a property.
  void emitObjCPropertySetter(SILConstant setter);

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
  /// argument. Returns the 'this' argument SILValue.
  SILValue emitDestructorProlog(ClassDecl *CD, DestructorDecl *DD);
  
  /// emitRetainRValue - Emits the instructions necessary to increase the
  /// retain count of a temporary, in order to use it as part of another
  /// independent temporary.
  /// - For trivial loadable types, this is a no-op.
  /// - For reference types, 'v' is retained.
  /// - For loadable types with reference type members, the reference type
  ///   members are all retained.
  /// - FIXME For address-only types, this currently crashes. It should instead
  ///   allocate and return a copy made using copy_addr.
  void emitRetainRValue(SILLocation loc, SILValue v);
    
  /// emitReleaseRValue - Emits the instructions necessary to clean up a
  /// temporary value.
  /// - For trivial loadable types, this is a no-op.
  /// - For reference types, 'v' is released.
  /// - For loadable types with reference type members, the reference type
  ///   members are all released.
  /// - For address-only types, the value at the address is destroyed using
  ///   a destroy_addr instruction.
  void emitReleaseRValue(SILLocation loc, SILValue v);

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
  
  RValue visit(Expr *E);
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
  
  void visitFallthroughStmt(FallthroughStmt *S, SGFContext C);
  
  void visitSwitchStmt(SwitchStmt *S, SGFContext C);

  void visitCaseStmt(CaseStmt *S, SGFContext C);
  
  //===--------------------------------------------------------------------===//
  // Expressions
  //===--------------------------------------------------------------------===//
  
  /// Generate SIL for the given expression, storing the final result into the
  /// specified Initialization buffer(s). This avoids an allocation and copy if
  /// the result would be allocated into temporary memory normally.
  void emitExprInto(Expr *E, Initialization *I);
  
  RValue visitApplyExpr(ApplyExpr *E, SGFContext C);

  RValue visitDeclRefExpr(DeclRefExpr *E, SGFContext C);
  RValue visitSuperRefExpr(SuperRefExpr *E, SGFContext C);
  RValue visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E,
                                                SGFContext C);
  
  RValue visitIntegerLiteralExpr(IntegerLiteralExpr *E, SGFContext C);
  RValue visitFloatLiteralExpr(FloatLiteralExpr *E, SGFContext C);
  RValue visitCharacterLiteralExpr(CharacterLiteralExpr *E, SGFContext C);
  RValue visitStringLiteralExpr(StringLiteralExpr *E, SGFContext C);
  RValue visitLoadExpr(LoadExpr *E, SGFContext C);
  RValue visitMaterializeExpr(MaterializeExpr *E, SGFContext C);
  RValue visitDerivedToBaseExpr(DerivedToBaseExpr *E, SGFContext C);
  RValue visitMetatypeConversionExpr(MetatypeConversionExpr *E,
                                           SGFContext C);
  RValue visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E, SGFContext C);
  RValue visitRequalifyExpr(RequalifyExpr *E, SGFContext C);
  RValue visitFunctionConversionExpr(FunctionConversionExpr *E,
                                           SGFContext C);
  RValue visitErasureExpr(ErasureExpr *E, SGFContext C);
  RValue visitCoerceExpr(CoerceExpr *E, SGFContext C);
  RValue visitUncheckedDowncastExpr(UncheckedDowncastExpr *E, SGFContext C);
  RValue visitUncheckedSuperToArchetypeExpr(
                                UncheckedSuperToArchetypeExpr *E, SGFContext C);
  RValue visitIsSubtypeExpr(IsSubtypeExpr *E, SGFContext C);
  RValue visitParenExpr(ParenExpr *E, SGFContext C);
  RValue visitTupleExpr(TupleExpr *E, SGFContext C);
  RValue visitScalarToTupleExpr(ScalarToTupleExpr *E, SGFContext C);
  RValue visitSpecializeExpr(SpecializeExpr *E, SGFContext C);
  RValue visitAddressOfExpr(AddressOfExpr *E, SGFContext C);
  RValue visitMemberRefExpr(MemberRefExpr *E, SGFContext C);
  RValue visitGenericMemberRefExpr(GenericMemberRefExpr *E, SGFContext C);
  RValue visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E,
                                           SGFContext C);
  RValue visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E,
                                             SGFContext C);
  RValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E,
                                             SGFContext C);
  RValue visitModuleExpr(ModuleExpr *E, SGFContext C);
  RValue visitTupleElementExpr(TupleElementExpr *E, SGFContext C);
  RValue visitSubscriptExpr(SubscriptExpr *E, SGFContext C);
  RValue visitGenericSubscriptExpr(GenericSubscriptExpr *E, SGFContext C);
  RValue visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E,
                                           SGFContext C);
  RValue visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E,
                                             SGFContext C);
  RValue visitTupleShuffleExpr(TupleShuffleExpr *E, SGFContext C);
  RValue visitNewArrayExpr(NewArrayExpr *E, SGFContext C);
  RValue visitMetatypeExpr(MetatypeExpr *E, SGFContext C);
  RValue visitFuncExpr(FuncExpr *E, SGFContext C);
  RValue visitPipeClosureExpr(PipeClosureExpr *E, SGFContext C);
  RValue visitClosureExpr(ClosureExpr *E, SGFContext C);
  RValue visitInterpolatedStringLiteralExpr(
                                              InterpolatedStringLiteralExpr *E,
                                              SGFContext C);
  RValue visitCollectionExpr(CollectionExpr *E, SGFContext C);
  RValue visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E,
                                                SGFContext C);
  RValue visitBridgeToBlockExpr(BridgeToBlockExpr *E, SGFContext C);
  RValue visitIfExpr(IfExpr *E, SGFContext C);
  RValue visitZeroValueExpr(ZeroValueExpr *E, SGFContext C);
  RValue visitDefaultValueExpr(DefaultValueExpr *E, SGFContext C);

  ManagedValue emitArrayInjectionCall(ManagedValue ObjectPtr,
                                      SILValue BasePtr,
                                      SILValue Length,
                                      Expr *ArrayInjectionFunction);
                        
  /// Emit the empty tuple value by emitting
  SILValue emitEmptyTuple(SILLocation loc);
  /// "Emit" an RValue representing an empty tuple.
  RValue emitEmptyTupleRValue(SILLocation loc);

  /// Returns a reference to a constant in global context. For local func decls
  /// this returns the function constant with unapplied closure context.
  SILValue emitGlobalFunctionRef(SILLocation loc, SILConstant constant);
  /// Returns a reference to a constant in local context. This will return a
  /// closure object reference if the constant refers to a local func decl.
  /// In rvalue contexts, emitFunctionRef should be used instead, which retains
  /// a local constant and returns a ManagedValue with a cleanup.
  SILValue emitUnmanagedFunctionRef(SILLocation loc, SILConstant constant);
  /// Returns a reference to a constant in local context. This will return a
  /// retained closure object reference if the constant refers to a local func
  /// decl.
  ManagedValue emitFunctionRef(SILLocation loc, SILConstant constant);

  ManagedValue emitReferenceToDecl(SILLocation loc,
                               ValueDecl *decl,
                               Type declType = Type(),
                               unsigned uncurryLevel
                                 = SILConstant::ConstructAtNaturalUncurryLevel);

  ManagedValue emitClosureForCapturingExpr(SILLocation loc,
                                           SILConstant function,
                                           CapturingExpr *body);
  
  Materialize emitMaterialize(SILLocation loc, ManagedValue v);
  Materialize emitGetProperty(SILLocation loc,
                              SILConstant getter,
                              ArrayRef<Substitution> substitutions,
                              RValue &&optionalthisValue,
                              RValue &&optionalSubscripts,
                              Type resultType);
  void emitSetProperty(SILLocation loc,
                       SILConstant setter,
                       ArrayRef<Substitution> substitutions,
                       RValue &&optionalThisValue,
                       RValue &&optionalSubscripts,
                       RValue &&value);
  
  ManagedValue emitManagedRValueWithCleanup(SILValue v);
  
  ManagedValue emitLoad(SILLocation loc, SILValue addr, SGFContext C,
                        bool isTake);
  
  void emitAssignToLValue(SILLocation loc, RValue &&src,
                          LValue const &dest);
  ManagedValue emitMaterializedLoadFromLValue(SILLocation loc,
                                              LValue const &src);
  ManagedValue emitSpecializedPropertyFunctionRef(
                                          SILLocation loc,
                                          SILConstant constant,
                                          ArrayRef<Substitution> substitutions,
                                          Type substPropertyType);

  ManagedValue emitMethodRef(SILLocation loc,
                             SILValue thisValue,
                             SILConstant methodConstant,
                             ArrayRef<Substitution> innerSubstitutions);
  void emitStore(SILLocation loc, ManagedValue src, SILValue destAddr);
  
  SILValue emitMetatypeOfValue(SILLocation loc, SILValue base);
  
  void emitReturnExpr(SILLocation loc, Expr *ret);
  
  /// Convert a value with a specialized representation (such as a thin function
  /// reference, or a function reference with a foreign calling convention) to
  /// the generalized representation of its Swift type, which can then be stored
  /// to a variable or passed as an argument or return value.
  SILValue emitGeneralizedValue(SILLocation loc, SILValue thinFn);
  
  //
  // Helpers for emitting ApplyExpr chains.
  //
  
  SILValue emitArchetypeMethod(ArchetypeMemberRefExpr *e, SILValue archetype);
  SILValue emitProtocolMethod(ExistentialMemberRefExpr *e, SILValue existential);
  
  RValue emitApplyExpr(ApplyExpr *e, SGFContext c);

  ManagedValue emitApply(SILLocation Loc, ManagedValue Fn,
                         ArrayRef<ManagedValue> Args,
                         OwnershipConventions const &Ownership,
                         SGFContext C = SGFContext());

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
  
  /// Emit the allocation for a local variable. Returns the address of the
  /// value. Does not register a cleanup.
  void emitLocalVariable(VarDecl *D);
  
  /// Emit the allocation for a local variable, provides an Initialization
  /// that can be used to initialize it, and registers cleanups in the active
  /// scope.
  std::unique_ptr<Initialization> emitLocalVariableWithCleanup(VarDecl *D);
  
  /// Destroy and deallocate an initialized local variable.
  void destroyLocalVariable(VarDecl *D);
  
  /// Deallocate an uninitialized local variable.
  void deallocateUninitializedLocalVariable(VarDecl *D);
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
