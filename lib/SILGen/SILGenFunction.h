//===--- SILGenFunction.h - Function Specific AST lower context -*- C++ -*-===//
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

#ifndef SILGENFUNCTION_H
#define SILGENFUNCTION_H

#include "SILGen.h"
#include "Condition.h"
#include "JumpDest.h"
#include "swift/AST/AnyFunctionRef.h"
#include "llvm/ADT/PointerIntPair.h"
#include "swift/SIL/SILBuilder.h"

namespace swift {
namespace Lowering {

/// SGFContext - Internal context information for the SILGenFunction visitor.
///
/// In general, emission methods which take an SGFContext indicate
/// that they've initialized the emit-into buffer (if they have) by
/// returning a null value of whatever type (typically an RValue or
/// ManagedValue).  Callers who propagate down an SGFContext that
/// might have a emit-into buffer must be aware of this.
struct SGFContext {
public:
  enum ChildOfLoad_t { ChildOfLoad };
  enum Ignored_t { Ignored };
  enum Ungeneralized_t { Ungeneralized };
  
private:
  enum class Kind {
    Normal,
    Ignored,
    ChildOfLoad,
    Ungeneralized
  };

private:
  using State = llvm::PointerIntPair<Initialization *, 2, Kind>;
  
  State state;
public:
  SGFContext() = default;
  
  /// Creates an emitInto context that will store the result of the visited expr
  /// into the given Initialization.
  explicit SGFContext(Initialization *emitInto)
    : state(emitInto, Kind::Normal)
  {}
  
  /// Creates a load expr context, in which a temporary lvalue that would
  /// normally be materialized can be left as an rvalue to avoid a useless
  /// immediately-consumed allocation.
  SGFContext(ChildOfLoad_t _)
    : state(nullptr, Kind::ChildOfLoad)
  {}

  /// Creates an ignored context, in which the value of the
  /// expression is being ignored.
  SGFContext(Ignored_t _)
    : state(nullptr, Kind::Ignored)
  {}

  /// Creates a context that is friendly to differences in abstraction.
  /// This permits generalization to be suppressed.
  SGFContext(Ungeneralized_t _)
    : state(nullptr, Kind::Ungeneralized)
  {}

  /// Returns a pointer to the Initialization that the current expression should
  /// store its result to, or null if the expression should allocate temporary
  /// storage for its result.
  Initialization *getEmitInto() const {
    return state.getPointer();
  }

  /// Does this context have a preferred address to emit into?
  bool hasAddressToEmitInto() const; // in Initialization.h
  
  /// Returns true if the current expression is a child of a LoadExpr, and
  /// should thus avoid emitting a temporary materialization if possible.
  bool isChildOfLoadExpr() const {
    return state.getInt() == Kind::ChildOfLoad;
  }

  /// Returns true if the current expression is in an ignored context.
  bool isIgnored() const {
    return state.getInt() == Kind::Ignored;
  }

  /// Returns true if the result of the expression does not need to
  /// generalized to match the representation of its formal type.
  bool isUngeneralized() const {
    return state.getInt() == Kind::Ungeneralized;
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
  std::vector<SwitchContext*> SwitchStack;
  /// Keep track of our current nested scope.
  std::vector<SILDebugScope*> DebugScopeStack;

  /// The cleanup depth and BB for when the operand of a
  /// BindOptionalExpr is a missing value.
  JumpDest BindOptionalFailureDest = JumpDest::invalid();

  /// The cleanup depth and epilog BB for "return" instructions.
  JumpDest ReturnDest;

  /// \brief True if a non-void return is required in this function.
  bool NeedsReturn : 1;
  
  /// \brief True if stored property accesses in this function should always be
  /// direct, even in @objc methods.
  bool AlwaysDirectStoredPropertyAccess : 1;
  
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

  /// VarLoc - representation of an emitted local variable.  Local variables can
  /// either have a singular constant value that is always returned (in which
  /// case VarLoc holds that value), or may be emitted into a box.  If they are
  /// emitted into a box, the retainable pointer is also stored.
  struct VarLoc {
    /// addressOrValue - the address at which the variable is stored, or the
    /// value of the decl if it is a constant.
    llvm::PointerIntPair<SILValue, 1, bool> addressOrValue;
  public:
    /// box - For a non-constant value, this is the retainable box for the
    /// variable.  It may be invalid if no box was made for the value (e.g.,
    /// because it was an @inout value, or constant).
    SILValue box;

    bool isConstant() const { return addressOrValue.getInt(); }
    bool isAddress() const { return !isConstant(); }

    SILValue getAddress() const {
      assert(isAddress() && "Can't get the address of a constant");
      return addressOrValue.getPointer();
    }

    SILValue getConstant() const {
      assert(isConstant() && "Emit a load to get the value of an address");
      return addressOrValue.getPointer();
    }

    static VarLoc getConstant(SILValue Val) {
      VarLoc Result;
      Result.addressOrValue.setPointerAndInt(Val, true);
      return Result;
    }
    static VarLoc getAddress(SILValue Addr, SILValue Box = SILValue()) {
      VarLoc Result;
      Result.addressOrValue.setPointerAndInt(Addr, false);
      Result.box = Box;
      return Result;
    }
  };
    
  /// VarLocs - Entries in this map are generated when a PatternBindingDecl is
  /// emitted. The map is queried to produce the lvalue for a DeclRefExpr to
  /// a local variable.
  llvm::DenseMap<ValueDecl*, VarLoc> VarLocs;
    
  /// LocalFunctions - Entries in this map are generated when a local function
  /// declaration that requires local context, such as a func closure, is
  /// emitted. This map is then queried to produce the value for a DeclRefExpr
  /// to a local constant.
  llvm::DenseMap<SILDeclRef, SILValue> LocalFunctions;

  /// Mapping from active opaque value expressions to their values,
  /// along with a bit for each indicating whether it has been consumed yet.
  llvm::DenseMap<OpaqueValueExpr *, std::pair<SILValue, bool> > OpaqueValues;

  /// RAII object that introduces a temporary binding for an opaque value.
  ///
  /// The value bound should have already been retained. Each time the
  /// opaque value expression is referenced, it will be retained/released
  /// separately. When this RAII object goes out of scope, the value will be
  /// destroyed.
  class OpaqueValueRAII {
    SILGenFunction &Self;
    OpaqueValueExpr *OpaqueValue;

    OpaqueValueRAII(const OpaqueValueRAII &) = delete;
    OpaqueValueRAII &operator=(const OpaqueValueRAII &) = delete;

  public:
    OpaqueValueRAII(SILGenFunction &self, OpaqueValueExpr *opaqueValue,
                    SILValue value)
      : Self(self), OpaqueValue(opaqueValue)
    {
      assert(Self.OpaqueValues.count(OpaqueValue) == 0 &&
             "Opaque value already has a binding");
      Self.OpaqueValues[OpaqueValue] = std::make_pair(value, false);
    }

    ~OpaqueValueRAII();
  };

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
  CleanupHandle getTopCleanup() const {
    return Cleanups.getTopCleanup();
  }
  
  SILFunction &getFunction() { return F; }
  SILBuilder &getBuilder() { return B; }
  
  const TypeLowering &getTypeLowering(AbstractionPattern orig, Type subst,
                                      unsigned uncurryLevel = 0) {
    return SGM.Types.getTypeLowering(orig, subst, uncurryLevel);
  }
  const TypeLowering &getTypeLowering(Type t, unsigned uncurryLevel = 0) {
    return SGM.Types.getTypeLowering(t, uncurryLevel);
  }
  SILType getLoweredType(AbstractionPattern orig, Type subst,
                         unsigned uncurryLevel = 0) {
    return SGM.Types.getLoweredType(orig, subst, uncurryLevel);
  }
  SILType getLoweredType(Type t, unsigned uncurryLevel = 0) {
    return SGM.Types.getLoweredType(t, uncurryLevel);
  }
  SILType getLoweredLoadableType(Type t, unsigned uncurryLevel = 0) {
    return SGM.Types.getLoweredLoadableType(t, uncurryLevel);
  }

  const TypeLowering &getTypeLowering(SILType type) {
    return SGM.Types.getTypeLowering(type);
  }

  SILConstantInfo getConstantInfo(SILDeclRef constant) {
    return SGM.Types.getConstantInfo(constant);
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
      InsertedInstrs[LastInsnWithoutScope++]->setDebugScope(DS);
    }
  }

  //===--------------------------------------------------------------------===//
  // Entry points for codegen
  //===--------------------------------------------------------------------===//
  
  /// \brief Generates code for a FuncDecl.
  void emitFunction(FuncDecl *fd);
  /// \brief Emits code for a ClosureExpr.
  void emitClosure(AbstractClosureExpr *ce);
  /// emitDestructor - Generates code for a class destroying destructor. This
  /// emits the body code from the DestructorDecl (if any),
  /// implicitly releases the elements of the class, and calls the base
  /// class destructor.
  void emitDestructor(ClassDecl *cd, DestructorDecl *dd);
  
  /// Generates code for a struct constructor.
  /// This allocates the new 'self' value, emits the
  /// body code, then returns the final initialized 'self'.
  void emitValueConstructor(ConstructorDecl *ctor);
  /// Generates code for an enum case constructor.
  /// This allocates the new 'self' value, injects the enum case,
  /// then returns the final initialized 'self'.
  void emitEnumConstructor(EnumElementDecl *element);
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
  void emitCurryThunk(FuncDecl *fd, SILDeclRef fromLevel, SILDeclRef toLevel);
  /// Generates a thunk from a foreign function to the native Swift conventions.
  void emitForeignThunk(SILDeclRef thunk);
  
  // Generate a nullary function that returns the given value.
  void emitGeneratorFunction(SILDeclRef function, Expr *value);

  /// Generate an ObjC-compatible thunk for a method.
  void emitObjCMethodThunk(SILDeclRef thunk);
  
  /// Generate an ObjC-compatible getter for a property.
  void emitObjCPropertyGetter(SILDeclRef getter);
  
  /// Generate an ObjC-compatible setter for a property.
  void emitObjCPropertySetter(SILDeclRef setter);

  /// Generate an ObjC-compatible getter for a subscript.
  void emitObjCSubscriptGetter(SILDeclRef getter);
  
  /// Generate an ObjC-compatible setter for a subscript.
  void emitObjCSubscriptSetter(SILDeclRef setter);

  /// Generate a lazy global initializer.
  void emitLazyGlobalInitializer(PatternBindingDecl *binding);
  
  /// Generate a global accessor, using the given initializer token and
  /// function
  void emitGlobalAccessor(VarDecl *global,
                          FuncDecl *builtinOnceDecl,
                          SILGlobalVariable *onceToken,
                          SILFunction *onceFunc);
  
  /// Generate a protocol witness entry point, invoking 'witness' at the
  /// abstraction level of 'requirement'.
  void emitProtocolWitness(ProtocolConformance *conformance,
                           SILDeclRef requirement,
                           SILDeclRef witness,
                           ArrayRef<Substitution> witnessSubs,
                           IsFreeFunctionWitness_t isFree,
                           HasInOutSelfAbstractionDifference_t inOutSelf);
  
  //===--------------------------------------------------------------------===//
  // Control flow
  //===--------------------------------------------------------------------===//
  
  /// emitCondition - Emit a boolean expression as a control-flow condition.
  ///
  /// \param E - The expression to be evaluated as a condition.
  /// \param hasFalseCode - true if the false branch doesn't just lead
  ///        to the fallthrough.
  /// \param invertValue - true if this routine should invert the value before
  ///        testing true/false.
  /// \param contArgs - the types of the arguments to the continuation BB.
  ///        Matching argument values must be passed to exitTrue and exitFalse
  ///        of the resulting Condition object.
  Condition emitCondition(Expr *E,
                          bool hasFalseCode = true, bool invertValue = false,
                          ArrayRef<SILType> contArgs = {});

  Condition emitCondition(SILValue V, SILLocation Loc,
                          bool hasFalseCode = true, bool invertValue = false,
                          ArrayRef<SILType> contArgs = {});

  SILBasicBlock *createBasicBlock() {
    return new (F.getModule()) SILBasicBlock(&F);
  }
  
  
  //===--------------------------------------------------------------------===//
  // Memory management
  //===--------------------------------------------------------------------===//
  
  /// emitProlog - Generates prolog code to allocate and clean up mutable
  /// storage for closure captures and local arguments.
  void emitProlog(AnyFunctionRef TheClosure, ArrayRef<Pattern*> paramPatterns,
                  Type resultType);
  void emitProlog(ArrayRef<Pattern*> paramPatterns,
                  Type resultType, DeclContext *DeclCtx);

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
  ///            constructing the epilog, such as a AbstractClosureExpr.
  /// \returns Nothing if the epilog block is unreachable. Otherwise, returns
  ///          the epilog block's return value argument, or a null SILValue if
  ///          the epilog doesn't take a return value. Also returns the location
  ///          of the return instrcution if the epilog block is supposed to host
  ///          the ReturnLocation (This happens in case the predecessor block is
  ///          merged with the epilog block.)
  std::pair<Optional<SILValue>, SILLocation>
    emitEpilogBB(SILLocation TopLevelLoc);
  
  /// \brief Emits a standard epilog which runs top-level cleanups then returns
  /// the function return value, if any.
  ///
  /// \param TopLevelLoc The location of the top-level expression during whose
  ///        evaluation the epilog is being produced, for example, the
  ///        AbstractClosureExpr.
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
  ///
  /// The caller should call manageBufferForExprResult at the instant
  /// that the buffer has been initialized.
  SILValue getBufferForExprResult(SILLocation loc, SILType ty, SGFContext C);

  /// Flag that the buffer for an expression result has been properly
  /// initialized.
  ///
  /// Returns an empty value if the buffer was taken from the context.
  ManagedValue manageBufferForExprResult(SILValue buffer,
                                         const TypeLowering &bufferTL,
                                         SGFContext C);
  
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

  /// Emit the given expression as an r-value.
  RValue emitRValue(Expr *E, SGFContext C = SGFContext());

  /// Emit an r-value that we're ignoring the result of.
  void emitIgnoredRValue(Expr *E);

  /// Emit an r-value, potentially suppressing conversion to a
  /// generalized form.
  RValue emitUngeneralizedRValue(Expr *E);
  
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
  SILValue emitGlobalFunctionRef(SILLocation loc, SILDeclRef constant) {
    return emitGlobalFunctionRef(loc, constant, getConstantInfo(constant));
  }
  SILValue emitGlobalFunctionRef(SILLocation loc, SILDeclRef constant,
                                 SILConstantInfo constantInfo);

  /// Returns a reference to a constant in local context. This will return a
  /// closure object reference if the constant refers to a local func decl.
  /// In rvalue contexts, emitFunctionRef should be used instead, which retains
  /// a local constant and returns a ManagedValue with a cleanup.
  SILValue emitUnmanagedFunctionRef(SILLocation loc, SILDeclRef constant);
  /// Returns a reference to a constant in local context. This will return a
  /// retained closure object reference if the constant refers to a local func
  /// decl.
  ManagedValue emitFunctionRef(SILLocation loc, SILDeclRef constant);
  ManagedValue emitFunctionRef(SILLocation loc, SILDeclRef constant,
                               SILConstantInfo constantInfo);

  /// Emit a reference to the given declaration.
  ///
  /// \param loc The location of the reference.
  /// \param declRef The reference, including any substitutions to be applied
  /// \param refType The type of the reference (after substitutions)
  /// \param uncurryLevel The uncurry level to use for function references.
  ManagedValue emitReferenceToDecl(
                 SILLocation loc,
                 ConcreteDeclRef declRef,
                 Type refType = Type(),
                 unsigned uncurryLevel
                   = SILDeclRef::ConstructAtNaturalUncurryLevel);

  ManagedValue emitClosureValue(SILLocation loc,
                                SILDeclRef function,
                                ArrayRef<Substitution> forwardSubs,
                                AnyFunctionRef TheClosure);
  
  Materialize emitMaterialize(SILLocation loc, ManagedValue v);
  
  RValueSource prepareAccessorBaseArg(SILLocation loc, SILValue base);
  
  ManagedValue emitGetAccessor(SILLocation loc,
                               SILDeclRef getter,
                               ArrayRef<Substitution> substitutions,
                               RValueSource &&optionalSelfValue,
                               RValueSource &&optionalSubscripts,
                               SGFContext C);
  void emitSetAccessor(SILLocation loc,
                       SILDeclRef setter,
                       ArrayRef<Substitution> substitutions,
                       RValueSource &&optionalSelfValue,
                       RValueSource &&optionalSubscripts,
                       RValueSource &&value);

  ManagedValue emitManagedRetain(SILLocation loc, SILValue v);
  ManagedValue emitManagedRetain(SILLocation loc, SILValue v,
                                 const TypeLowering &lowering);
  
  ManagedValue emitManagedRValueWithCleanup(SILValue v);
  ManagedValue emitManagedRValueWithCleanup(SILValue v,
                                            const TypeLowering &lowering);

  ManagedValue emitManagedBufferWithCleanup(SILValue addr);
  ManagedValue emitManagedBufferWithCleanup(SILValue addr,
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

  void emitSemanticStore(SILLocation loc, SILValue value,
                         SILValue dest, const TypeLowering &destTL,
                         IsInitialization_t isInit);
  
  SILValue emitConversionFromSemanticValue(SILLocation loc,
                                           SILValue semanticValue,
                                           SILType storageType);
  
  ManagedValue emitLoad(SILLocation loc, SILValue addr,
                        const TypeLowering &rvalueTL,
                        SGFContext C, IsTake_t isTake);
  
  void emitAssignToLValue(SILLocation loc, RValueSource &&src,
                          const LValue &dest);
  void emitAssignLValueToLValue(SILLocation loc,
                                const LValue &src, const LValue &dest);
  void emitCopyLValueInto(SILLocation loc, const LValue &src,
                          Initialization *dest);
  ManagedValue emitAddressOfLValue(SILLocation loc, LValue const &src);
  ManagedValue emitLoadOfLValue(SILLocation loc, const LValue &src, SGFContext C);
  
  /// Emit a reference to a method from within another method of the type, and
  /// gather all the substitutions necessary to invoke it, without
  /// dynamic dispatch.
  std::tuple<ManagedValue, SILType, ArrayRef<Substitution>>
  emitSiblingMethodRef(SILLocation loc,
                       SILValue selfValue,
                       SILDeclRef methodConstant,
                       ArrayRef<Substitution> innerSubstitutions);
  
  SILValue emitMetatypeOfValue(SILLocation loc, SILValue base);
  
  void emitReturnExpr(SILLocation loc, Expr *ret);

  /// Convert a value with the abstraction patterns of the original type
  /// to a value with the abstraction patterns of the substituted type.
  ManagedValue emitOrigToSubstValue(SILLocation loc, ManagedValue input,
                                    AbstractionPattern origType,
                                    CanType substType,
                                    SGFContext ctx = SGFContext());

  /// Convert a value with the abstraction patterns of the substituted
  /// type to a value with the abstraction patterns of the original type.
  ManagedValue emitSubstToOrigValue(SILLocation loc, ManagedValue input,
                                    AbstractionPattern origType,
                                    CanType substType,
                                    SGFContext ctx = SGFContext());

  /// Convert a value with a specialized representation (such as a thin function
  /// reference, or a function reference with a foreign calling convention) to
  /// the generalized representation of its Swift type, which can then be stored
  /// to a variable or passed as an argument or return value.
  ManagedValue emitGeneralizedValue(SILLocation loc, ManagedValue input,
                                    AbstractionPattern origType,
                                    CanType substType,
                                    SGFContext ctxt = SGFContext());

  ManagedValue emitGeneralizedFunctionValue(SILLocation loc,
                                            ManagedValue input,
                                            AbstractionPattern origType,
                                            CanAnyFunctionType resultType);
  
  /// Convert a native Swift value to a value that can be passed as an argument
  /// to or returned as the result of a function with the given calling
  /// convention.
  ManagedValue emitNativeToBridgedValue(SILLocation loc, ManagedValue v,
                                        AbstractCC destCC,
                                        CanType origNativeTy,
                                        CanType substNativeTy,
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

  /// A convenience method for emitApply that just handles monomorphic
  /// applications.
  ManagedValue emitMonomorphicApply(SILLocation loc,
                                    ManagedValue fn,
                                    ArrayRef<ManagedValue> args,
                                    CanType resultType,
                                    bool forceInline = false);

  ManagedValue emitApplyOfLibraryIntrinsic(SILLocation loc,
                                           FuncDecl *fn,
                                           ArrayRef<Substitution> subs,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext ctx);

  /// Emit a dynamic member reference.
  RValue emitDynamicMemberRefExpr(DynamicMemberRefExpr *e, SGFContext c);

  /// Emit a dynamic subscript.
  RValue emitDynamicSubscriptExpr(DynamicSubscriptExpr *e, SGFContext c);

  /// \brief Emit an unconditional checked cast, including any necessary
  /// abstraction difference between the original and destination types.
  ///
  /// \param loc          The AST location associated with the operation.
  /// \param original     The value to cast.
  /// \param origTy       The original AST-level type.
  /// \param castTy       The destination type.
  /// \param kind         The semantics of the cast.
  ///
  /// \returns the cast value, at its natural abstraction level.
  SILValue emitUnconditionalCheckedCast(SILLocation loc,
                                        SILValue original,
                                        Type origTy,
                                        Type castTy,
                                        CheckedCastKind kind);
  
  /// \brief Emits the abstraction change needed, if any, to perform casts from
  /// the type represented by \c origTL to each of the types represented by
  /// \c castTLs.
  ///
  /// \param loc      The AST location associated with the operation.
  /// \param original The value to cast.
  /// \param origTL   The original type.
  /// \param castTLs  The types to which to cast.
  ///
  /// \returns The value shifted to the highest abstraction level necessary
  /// for the casts, or a null SILValue if no abstraction changes are needed.
  SILValue emitCheckedCastAbstractionChange(SILLocation loc,
                                       SILValue original,
                                       const TypeLowering &origTL,
                                       ArrayRef<const TypeLowering *> castTLs);
  
  /// \brief Emit a conditional checked cast branch. Does not re-abstract the
  /// argument to the success branch. Terminates the current BB.
  ///
  /// \param loc          The AST location associated with the operation.
  /// \param original     The value to cast.
  /// \param originalAbstracted
  ///                     The result of \c emitCheckedCastAbstractionChange
  ///                     applied to the original value.
  /// \param origTL       The original AST-level type.
  /// \param castTL       The destination type.
  /// \param kind         The semantics of the cast.
  ///
  /// \returns a pair of SILBasicBlocks, representing the success and failure
  /// branches of the cast. The argument to the success block is not adjusted
  /// to its natural abstraction level.
  std::pair<SILBasicBlock*, SILBasicBlock*>
  emitCheckedCastBranch(SILLocation loc,
                        SILValue original,
                        SILValue originalAbstracted,
                        const TypeLowering &origTL,
                        const TypeLowering &castTL,
                        CheckedCastKind kind);

  /// Initialize a memory location with an optional value.
  ///
  /// \param loc   The location to use for the resulting optional.
  /// \param value The value to inject into an optional.
  /// \param dest  The uninitialized memory in which to store the result value.
  /// \param optTL Type lowering information for the optional to create.
  void emitInjectOptionalValueInto(SILLocation loc,
                                   RValueSource &&value,
                                   SILValue dest,
                                   const TypeLowering &optTL);

  /// Initialize a memory location with an optional "nothing"
  /// value.
  ///
  /// \param loc   The location to use for the resulting optional.
  /// \param dest  The uninitialized memory in which to store the result value.
  /// \param optTL Type lowering information for the optional to create.
  void emitInjectOptionalNothingInto(SILLocation loc,
                                     SILValue dest,
                                     const TypeLowering &optTL);

  /// \brief Emit a call to the library intrinsic _doesOptionalHaveValue.
  ///
  /// The result is a Builtin.Int1.
  SILValue emitDoesOptionalHaveValue(SILLocation loc, SILValue addr);

  /// \brief Emit a call to the library intrinsic _getOptionalValue
  /// given the address of the optional.
  ManagedValue emitGetOptionalValueFrom(SILLocation loc, ManagedValue addr,
                                        const TypeLowering &optTL,
                                        SGFContext C);

  //===--------------------------------------------------------------------===//
  // Declarations
  //===--------------------------------------------------------------------===//
  
  void visitDecl(Decl *D) {
    llvm_unreachable("Not yet implemented");
  }

  void visitNominalTypeDecl(NominalTypeDecl *D);
  void visitFuncDecl(FuncDecl *D);
  void visitPatternBindingDecl(PatternBindingDecl *D);
  
  std::unique_ptr<Initialization> emitPatternBindingInitialization(Pattern *P);
    
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

  /// Emit the allocation for a local temporary, provides an
  /// Initialization that can be used to initialize it, and registers
  /// cleanups in the active scope.
  ///
  /// The initialization is guaranteed to be a single buffer.
  std::unique_ptr<TemporaryInitialization>
  emitTemporary(SILLocation loc, const TypeLowering &tempTL);

  /// Enter a currently-dormant cleanup to destroy the value in the
  /// given address.
  CleanupHandle enterDormantTemporaryCleanup(SILValue temp,
                                             const TypeLowering &tempTL);

  /// Destroy and deallocate an initialized local variable.
  void destroyLocalVariable(SILLocation L, VarDecl *D);
  
  /// Deallocate an uninitialized local variable.
  void deallocateUninitializedLocalVariable(SILLocation L, VarDecl *D);

  /// Enter a cleanup to deallocate a stack variable.
  CleanupHandle enterDeallocStackCleanup(SILLocation loc, SILValue address);
  
  /// Evaluate an Expr as an lvalue.
  LValue emitLValue(Expr *E);

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
