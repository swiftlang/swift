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
#include "JumpDest.h"
#include "swift/AST/AnyFunctionRef.h"
#include "llvm/ADT/PointerIntPair.h"
#include "swift/SIL/SILBuilder.h"

namespace swift {
namespace Lowering {

class ArgumentSource;
class Condition;
class ConsumableManagedValue;
class Initialization;
class LogicalPathComponent;
class LValue;
class ManagedValue;
class RValue;
class TemporaryInitialization;

/// Represents a temporary allocation.
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

/// How a method is dispatched.
enum class MethodDispatch {
  // The method implementation can be referenced statically.
  Static,
  // The method implementation uses class_method dispatch.
  Class,
};
  
/// Internal context information for the SILGenFunction visitor.
///
/// In general, emission methods which take an SGFContext indicate
/// that they've initialized the emit-into buffer (if they have) by
/// returning a "isInContext()" ManagedValue of whatever type.  Callers who
/// propagate down an SGFContext that might have a emit-into buffer must be
/// aware of this.
///
/// Clients of emission routines that take an SGFContext can also specify that
/// they are ok getting back an RValue at +0 instead of requiring it to be at
/// +1.  The client is then responsible for checking the ManagedValue to see if
/// it got back a ManagedValue at +0 or +1.
class SGFContext {
  enum DesiredTransfer {
    PlusOne,
    ImmediatePlusZero,
    GuaranteedPlusZero,
  };
  llvm::PointerIntPair<Initialization *, 2, DesiredTransfer> state;
public:
  SGFContext() = default;
  
  enum AllowImmediatePlusZero_t {
    /// The client is okay with getting a +0 value and plans to use it
    /// immediately.
    ///
    /// For example, in this context, it would be okay to return +0
    /// even for a load from a mutable variable, because the only way
    /// the value could be invalidated before it's used is a race
    /// condition.
    AllowImmediatePlusZero
  };

  enum AllowGuaranteedPlusZero_t {
    /// The client is okay with getting a +0 value as long as it's
    /// guaranteed to last at least as long as the current evaluation.
    /// (For expression evaluation, this generally means at least
    /// until the end of the current statement.)
    ///
    /// For example, in this context, it would be okay to return +0
    /// for a reference to a local 'let' because that will last until
    /// the 'let' goes out of scope.  However, it would not be okay to
    /// return +0 for a load from a mutable 'var', because that could
    /// be mutated before the end of the statement.
    AllowGuaranteedPlusZero
  };
  
  /// Creates an emitInto context that will store the result of the visited expr
  /// into the given Initialization.
  explicit SGFContext(Initialization *emitInto) : state(emitInto, PlusOne) {
  }
  
  /*implicit*/
  SGFContext(AllowImmediatePlusZero_t) : state(nullptr, ImmediatePlusZero) {
  }

  /*implicit*/
  SGFContext(AllowGuaranteedPlusZero_t) : state(nullptr, GuaranteedPlusZero) {
  }
  
  /// Returns a pointer to the Initialization that the current expression should
  /// store its result to, or null if the expression should allocate temporary
  /// storage for its result.
  Initialization *getEmitInto() const {
    return state.getPointer();
  }
  
  /// Return true if a ManagedValue producer is allowed to return at
  /// +0, given that it cannot guarantee that the value will be valid
  /// until the end of the current evaluation.
  bool isImmediatePlusZeroOk() const {
    return state.getInt() == ImmediatePlusZero;
  }

  /// Return true if a ManagedValue producer is allowed to return at
  /// +0 if it can guarantee that the value will be valid until the
  /// end of the current evaluation.
  bool isGuaranteedPlusZeroOk() const {
    // Either ImmediatePlusZero or GuaranteedPlusZero is fine.
    return state.getInt() >= ImmediatePlusZero;
  }

  /// Get a context for a sub-expression given that arbitrary side
  /// effects may follow the subevaluation.
  SGFContext withFollowingSideEffects() const {
    SGFContext copy = *this;
    if (copy.state.getInt() == ImmediatePlusZero) {
      copy.state.setInt(GuaranteedPlusZero);
    }
    return copy;
  }

  /// Get a context for a sub-expression where we plan to project out
  /// a value.  The Initialization is not okay to propagate down, but
  /// the +0/+1-ness is.
  SGFContext withFollowingProjection() const {
    SGFContext copy;
    copy.state.setInt(copy.state.getInt());
    return copy;
  }
};

class PatternMatchContext;
struct LValueWriteback;

/// A thunk action that a vtable thunk needs to perform on its result.
enum class VTableResultThunk {
  None, ///< No result change.
  MakeOptional, ///< Wrap the result in an optional.
};
/// A thunk action that a vtable thunk needs to perform on a parameter.
enum class VTableParamThunk {
  None, ///< No result change.
  MakeOptional, ///< Wrap the param in an optional.
  ForceIUO, ///< Force-unwrap the IUO param.
};

/// SILGenFunction - an ASTVisitor for producing SIL from function bodies.
class LLVM_LIBRARY_VISIBILITY SILGenFunction
  : public ASTVisitor<SILGenFunction>
{ // style violation because Xcode <rdar://problem/13065676>
public:
  /// The SILGenModule this function belongs to.
  SILGenModule &SGM;
    
  /// The SILFunction being constructed.
  SILFunction &F;
  
  /// The name of the function currently being emitted, as presented to user
  /// code by __FUNCTION__.
  DeclName MagicFunctionName;
  std::string MagicFunctionString;

  ASTContext &getASTContext() const { return SGM.M.getASTContext(); }

  /// This is used to keep track of all SILInstructions inserted by \c B.
  SmallVector<SILInstruction*, 32> InsertedInstrs;
  size_t LastInsnWithoutScope;
  
  /// B - The SILBuilder used to construct the SILFunction.  It is
  /// what maintains the notion of the current block being emitted
  /// into.
  SILBuilder B;
    
  /// IndirectReturnAddress - For a function with an indirect return, holds a
  /// value representing the address to initialize with the return value. Null
  /// for a function that returns by value.
  SILValue IndirectReturnAddress;
  
  std::vector<std::tuple<LabeledStmt*, JumpDest, JumpDest>>
    BreakContinueDestStack;
  std::vector<PatternMatchContext*> PatternMatchStack;
  /// Keep track of our current nested scope.
  std::vector<SILDebugScope*> DebugScopeStack;
  SILDebugScope *MainScope = nullptr;

  /// The cleanup depth and BB for when the operand of a
  /// BindOptionalExpr is a missing value.
  SmallVector<JumpDest, 2> BindOptionalFailureDests;

  /// The cleanup depth and epilog BB for "return" statements.
  JumpDest ReturnDest;
  /// The cleanup depth and epilog BB for "fail" statements.
  JumpDest FailDest;
  /// The 'self' variable that needs to be cleaned up on failure.
  VarDecl *FailSelfDecl = nullptr;

  /// \brief True if a non-void return is required in this function.
  bool NeedsReturn : 1;
    
  /// \brief The SIL location corresponding to the AST node being processed.
  SILLocation CurrentSILLoc;

  /// Cleanups - This records information about the currently active cleanups.
  CleanupManager Cleanups;

  /// The stack of pending writebacks.
  std::vector<LValueWriteback> *WritebackStack = 0;
  std::vector<LValueWriteback> &getWritebackStack();

  bool InWritebackScope = false;
  bool InInOutConversionScope = false;

  /// freeWritebackStack - Just deletes WritebackStack.  Out of line to avoid
  /// having to put the definition of LValueWriteback in this header.
  void freeWritebackStack();

  /// VarLoc - representation of an emitted local variable or constant.  There
  /// are three scenarios here:
  ///
  ///  1) This could be a simple "var" or "let" emitted into an alloc_box.  In
  ///     this case, 'value' contains a pointer (it is always an address) to the
  ///     value, and 'box' contains a pointer to the retain count for the box.
  ///  2) This could be a simple non-address-only "let" represented directly. In
  ///     this case, 'value' is the value of the let and is never of address
  ///     type.  'box' is always nil.
  ///  3) This could be an address-only "let" emitted into an alloc_stack, or
  ///     passed in from somewhere else that has guaranteed lifetime (e.g. an
  ///     incoming argument of 'in_guaranteed' convention).  In this case,
  ///     'value' is a pointer to the memory (and thus, its type is always an
  ///     address) and the 'box' is nil.
  ///
  /// Generally, code shouldn't be written to enumerate these three cases, it
  /// should just handle the case of "box or not" or "address or not", depending
  /// on what the code cares about.
  struct VarLoc {
    /// value - the value of the variable, or the address the variable is
    /// stored at (if "value.getType().isAddress()" is true).
    SILValue value;

    /// box - This is the retainable box for something emitted to an alloc_box.
    /// It may be invalid if no box was made for the value (e.g., because it was
    /// an inout value, or constant emitted to an alloc_stack).
    SILValue box;

    static VarLoc get(SILValue value, SILValue box = SILValue()) {
      VarLoc Result;
      Result.value = value;
      Result.box = box;
      return Result;
    }
  };
  
  /// VarLocs - Entries in this map are generated when a PatternBindingDecl is
  /// emitted. The map is queried to produce the lvalue for a DeclRefExpr to
  /// a local variable.
  llvm::DenseMap<ValueDecl*, VarLoc> VarLocs;

  /// OpenedArchetypes - Mappings of opened archetypes back to the
  /// instruction which opened them.
  llvm::DenseMap<CanType, SILValue> ArchetypeOpenings;

  SILValue getArchetypeOpeningSite(CanArchetypeType archetype) const {
    auto it = ArchetypeOpenings.find(archetype);
    assert(it != ArchetypeOpenings.end() &&
           "opened archetype was not registered with SILGenFunction");
    return it->second;
  }

  void setArchetypeOpeningSite(CanArchetypeType archetype, SILValue site) {
    ArchetypeOpenings.insert({archetype, site});
  }
    
  /// When rebinding 'self' during an initializer delegation, we have to be
  /// careful to preserve the object at 1 retain count during the delegation
  /// because of assumptions in framework code. This enum tracks the state of
  /// 'self' during the delegation.
  enum SelfInitDelegationStates {
    // 'self' is a normal variable.
    NormalSelf,
    
    // 'self' needs to be consumed next time it is referenced.
    WillConsumeSelf,
    
    // 'self' has been consumed.
    DidConsumeSelf,
  };
  SelfInitDelegationStates SelfInitDelegationState = NormalSelf;
  
  /// LocalFunctions - Entries in this map are generated when a local function
  /// declaration that requires local context, such as a func closure, is
  /// emitted. This map is then queried to produce the value for a DeclRefExpr
  /// to a local constant.
  llvm::DenseMap<SILDeclRef, SILValue> LocalFunctions;

  /// Mapping from active opaque value expressions to their values,
  /// along with a bit for each indicating whether it has been consumed yet.
  llvm::DenseMap<OpaqueValueExpr *, std::pair<SILValue, bool>> OpaqueValues;

  /// RAII object that introduces a temporary binding for an opaque value.
  ///
  /// Each time the opaque value expression is referenced, it will be
  /// retained/released separately. When this RAII object goes out of
  /// scope, the value will be destroyed if requested.
  class OpaqueValueRAII {
    SILGenFunction &Self;
    OpaqueValueExpr *OpaqueValue;
    bool Destroy;

    OpaqueValueRAII(const OpaqueValueRAII &) = delete;
    OpaqueValueRAII &operator=(const OpaqueValueRAII &) = delete;

  public:
    OpaqueValueRAII(SILGenFunction &self, OpaqueValueExpr *opaqueValue,
                    SILValue value, bool destroy)
      : Self(self), OpaqueValue(opaqueValue), Destroy(destroy)
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
  Optional<SourceLoc> overrideLocationForMagicIdentifiers;

  /// Emit code to increment a counter for profiling.
  void emitProfilerIncrement(ASTNode N) {
    if (SGM.Profiler)
      SGM.Profiler->emitCounterIncrement(B, N);
  }
  
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
    else {
      DS->setParent(F.getDebugScope());
      MainScope = DS;
    }
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
  /// Generates code for a class destroying destructor. This
  /// emits the body code from the DestructorDecl, calls the base class 
  /// destructor, then implicitly releases the elements of the class.
  void emitDestroyingDestructor(DestructorDecl *dd);

  /// Generates code for an artificial top-level function that starts an
  /// application based on a main class.
  void emitArtificialTopLevel(ClassDecl *mainClass);
  
  /// Generates code for a class deallocating destructor. This
  /// calls the destroying destructor and then deallocates 'self'.
  void emitDeallocatingDestructor(DestructorDecl *dd);
  
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
  /// Generates code to initialize instance variables from their
  /// initializers.
  ///
  /// \param selfDecl The 'self' declaration within the current function.
  /// \param nominal The type whose members are being initialized.
  void emitMemberInitializers(VarDecl *selfDecl, NominalTypeDecl *nominal);

  /// Emit a method that initializes the ivars of a class.
  void emitIVarInitializer(SILDeclRef ivarInitializer);

  /// Emit a method that destroys the ivars of a class.
  void emitIVarDestroyer(SILDeclRef ivarDestroyer);

  /// Generates code to destroy the instance variables of a class.
  ///
  /// \param selfValue The 'self' value.
  /// \param cd The class declaration whose members are being destroyed.
  void emitClassMemberDestruction(SILValue selfValue, ClassDecl *cd,
                                  CleanupLocation cleanupLoc);
  /// Generates code for a curry thunk from one uncurry level
  /// of a function to another.
  void emitCurryThunk(FuncDecl *fd, SILDeclRef fromLevel, SILDeclRef toLevel);
  /// Generates a thunk from a foreign function to the native Swift convention.
  void emitForeignToNativeThunk(SILDeclRef thunk);
  /// Generates a thunk from a native function to the  conventions.
  void emitNativeToForeignThunk(SILDeclRef thunk);
  
  // Generate a nullary function that returns the given value.
  void emitGeneratorFunction(SILDeclRef function, Expr *value);

  /// Generate an ObjC-compatible destructor (-dealloc).
  void emitObjCDestructor(SILDeclRef dtor);

  /// Generate a lazy global initializer.
  void emitLazyGlobalInitializer(PatternBindingDecl *binding);
  
  /// Generate a global accessor, using the given initializer token and
  /// function
  void emitGlobalAccessor(VarDecl *global,
                          SILGlobalVariable *onceToken,
                          SILFunction *onceFunc);

  void emitGlobalGetter(VarDecl *global,
                        SILGlobalVariable *onceToken,
                        SILFunction *onceFunc);
  
  /// Generate a protocol witness entry point, invoking 'witness' at the
  /// abstraction level of 'requirement'.
  void emitProtocolWitness(ProtocolConformance *conformance,
                           SILDeclRef requirement,
                           SILDeclRef witness,
                           ArrayRef<Substitution> witnessSubs,
                           IsFreeFunctionWitness_t isFree);
  
  /// Convert a block to a native function with a thunk.
  ManagedValue emitBlockToFunc(SILLocation loc,
                               ManagedValue block,
                               CanSILFunctionType funcTy);
  
  /// Thunk between a derived and base class.
  void emitVTableThunk(SILDeclRef derived, SILDeclRef base,
                       ArrayRef<VTableParamThunk> paramThunks,
                       VTableResultThunk resultThunk);
  
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

  SILBasicBlock *createBasicBlock(SILBasicBlock *afterBB = nullptr) {
    return new (F.getModule()) SILBasicBlock(&F, afterBB);
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
  /// \returns None if the epilog block is unreachable. Otherwise, returns
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
  
  /// emitSelfDecl - Emit a SILArgument for 'self', register it in varlocs, set
  /// up debug info, etc.  This returns the 'self' value.
  SILValue emitSelfDecl(VarDecl *selfDecl);
  
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
  
  void visitIfConfigStmt(IfConfigStmt *S);

  void visitDoStmt(DoStmt *S);
  
  void visitWhileStmt(WhileStmt *S);
  
  void visitDoWhileStmt(DoWhileStmt *S);
  
  void visitForStmt(ForStmt *S);
  
  void visitForEachStmt(ForEachStmt *S);
  
  void visitBreakStmt(BreakStmt *S);
  
  void visitContinueStmt(ContinueStmt *S);
  
  void visitFallthroughStmt(FallthroughStmt *S);
  
  void visitSwitchStmt(SwitchStmt *S);

  void visitCaseStmt(CaseStmt *S);

  void visitFailStmt(FailStmt *S);

  void emitBreakOutOf(SILLocation loc, Stmt *S);
  
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

  /// Emit the given expression as an r-value that follows the
  /// abstraction patterns of the original type.
  ManagedValue emitRValueAsOrig(Expr *E, AbstractionPattern origPattern,
                                const TypeLowering &origTL,
                                SGFContext C = SGFContext());
  
  /// Emit the given expression, ignoring its result.
  void emitIgnoredExpr(Expr *E);
  
  /// Emit the given expression as an r-value, then (if it is a tuple), combine
  /// it together into a single ManagedValue.
  ManagedValue emitRValueAsSingleValue(Expr *E, SGFContext C = SGFContext());

  /// Emit 'undef' in a particular formal type.
  ManagedValue emitUndef(SILLocation loc, Type type);
  ManagedValue emitUndef(SILLocation loc, SILType type);
  
  std::pair<ManagedValue, SILValue>
  emitUninitializedArrayAllocation(Type ArrayTy,
                                   SILValue Length,
                                   SILLocation Loc);

  SILValue emitConversionToSemanticRValue(SILLocation loc, SILValue value,
                                          const TypeLowering &valueTL);


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
  
  /// Returns a reference to a function value that dynamically dispatches
  /// the function in a runtime-modifiable way.
  SILValue emitDynamicMethodRef(SILLocation loc, SILDeclRef constant,
                                SILConstantInfo constantInfo);  

  /// Returns a reference to a function value that dynamically invokes the 
  
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

  /// Emit the specified VarDecl as an LValue if possible, otherwise return
  /// null.
  ManagedValue emitLValueForDecl(SILLocation loc, VarDecl *var,
                                 CanType formalRValueType,
                                 AccessKind accessKind,
                                 AccessSemantics semantics
                                   = AccessSemantics::Ordinary);
  
  /// Produce a singular RValue for a reference to the specified declaration,
  /// with the given type and in response to the specified epxression.  Try to
  /// emit into the specified SGFContext to avoid copies (when provided).
  ManagedValue emitRValueForDecl(SILLocation loc, ConcreteDeclRef decl, Type ty,
                                 AccessSemantics semantics,
                                 SGFContext C = SGFContext());

  /// Produce a singular RValue for a load from the specified property.
  ManagedValue emitRValueForPropertyLoad(SILLocation loc, ManagedValue base,
                                         bool isSuper, VarDecl *property,
                                         ArrayRef<Substitution> substitutions,
                                         AccessSemantics semantics,
                                         Type propTy, SGFContext C);


  ManagedValue emitClosureValue(SILLocation loc,
                                SILDeclRef function,
                                ArrayRef<Substitution> forwardSubs,
                                AnyFunctionRef TheClosure);
  
  Materialize emitMaterialize(SILLocation loc, ManagedValue v);
  
  ArgumentSource prepareAccessorBaseArg(SILLocation loc, ManagedValue base,
                                        SILDeclRef accessor);

  SILDeclRef getGetterDeclRef(AbstractStorageDecl *decl,
                              bool isDirectAccessorUse);  
  ManagedValue emitGetAccessor(SILLocation loc, SILDeclRef getter,
                               ArrayRef<Substitution> substitutions,
                               ArgumentSource &&optionalSelfValue,
                               bool isSuper, bool isDirectAccessorUse,
                               RValue &&optionalSubscripts, SGFContext C);

  SILDeclRef getSetterDeclRef(AbstractStorageDecl *decl,
                              bool isDirectAccessorUse);  
  void emitSetAccessor(SILLocation loc, SILDeclRef setter,
                       ArrayRef<Substitution> substitutions,
                       ArgumentSource &&optionalSelfValue,
                       bool isSuper, bool isDirectAccessorUse,
                       RValue &&optionalSubscripts, RValue &&value);

  SILDeclRef getMaterializeForSetDeclRef(AbstractStorageDecl *decl,
                                         bool isDirectAccessorUse);  
  std::pair<SILValue, SILValue>
  emitMaterializeForSetAccessor(SILLocation loc, SILDeclRef materializeForSet,
                                ArrayRef<Substitution> substitutions,
                                ArgumentSource &&optionalSelfValue,
                                bool isSuper, bool isDirectAccessorUse,
                                RValue &&optionalSubscripts,
                                SILValue buffer, SILValue callbackStorage);

  SILDeclRef getAddressorDeclRef(AbstractStorageDecl *decl,
                                 AccessKind accessKind,
                                 bool isDirectAccessorUse);
  std::pair<ManagedValue,ManagedValue>
  emitAddressorAccessor(SILLocation loc, SILDeclRef addressor,
                        ArrayRef<Substitution> substitutions,
                        ArgumentSource &&optionalSelfValue,
                        bool isSuper, bool isDirectAccessorUse,
                        RValue &&optionalSubscripts,
                        SILType addressType);

  ManagedValue emitApplyConversionFunction(SILLocation loc,
                                           Expr *funcExpr,
                                           Type resultType,
                                           RValue &&operand);

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
                        SGFContext C, IsTake_t isTake,
                        bool isGuaranteedValid = false);
  
  void emitAssignToLValue(SILLocation loc, RValue &&src,
                          LValue &&dest);
  void emitAssignLValueToLValue(SILLocation loc,
                                LValue &&src, LValue &&dest);
  void emitCopyLValueInto(SILLocation loc, LValue &&src,
                          Initialization *dest);
  ManagedValue emitAddressOfLValue(SILLocation loc, LValue &&src,
                                   AccessKind accessKind);
  ManagedValue emitLoadOfLValue(SILLocation loc, LValue &&src,
                                SGFContext C);
  
  /// Emit a reference to a method from within another method of the type, and
  /// gather all the substitutions necessary to invoke it, without
  /// dynamic dispatch.
  std::tuple<ManagedValue, SILType, ArrayRef<Substitution>>
  emitSiblingMethodRef(SILLocation loc,
                       SILValue selfValue,
                       SILDeclRef methodConstant,
                       ArrayRef<Substitution> innerSubstitutions);
  
  SILValue emitMetatypeOfValue(SILLocation loc, Expr *baseExpr);
  
  void emitReturnExpr(SILLocation loc, Expr *ret);

  /// Turn a consumable managed value into a +1 managed value.
  ManagedValue getManagedValue(SILLocation loc,
                               ConsumableManagedValue value);

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

  /// Emit the control flow for an optional 'bind' operation, branching to the
  /// active failure destination if the optional value addressed by optionalAddr
  /// is nil, and leaving the insertion point on the success branch.
  void emitBindOptional(SILLocation loc, SILValue optionalAddr,
                        unsigned depth);

  //
  // Helpers for emitting ApplyExpr chains.
  //
  
  RValue emitApplyExpr(Expr *e, SGFContext c);

  /// Emit a function application, assuming that the arguments have been
  /// lowered appropriately for the abstraction level but that the
  /// result does need to be turned back into something matching a
  /// formal type.
  ManagedValue emitApply(SILLocation loc,
                         ManagedValue fn,
                         ArrayRef<Substitution> subs,
                         ArrayRef<ManagedValue> args,
                         CanSILFunctionType substFnType,
                         AbstractionPattern origResultType,
                         CanType substResultType,
                         bool transparent,
                         Optional<AbstractCC> overrideCC,
                         SGFContext evalContext);

  ManagedValue emitApplyOfDefaultArgGenerator(SILLocation loc,
                                              ConcreteDeclRef defaultArgsOwner,
                                              unsigned destIndex,
                                              CanType resultType,
                                              SGFContext C = SGFContext());

  /// A convenience method for emitApply that just handles monomorphic
  /// applications.
  ManagedValue emitMonomorphicApply(SILLocation loc,
                                    ManagedValue fn,
                                    ArrayRef<ManagedValue> args,
                                    CanType resultType,
                                    bool transparent = false,
                                    Optional<AbstractCC> overrideCC = None);

  ManagedValue emitApplyOfLibraryIntrinsic(SILLocation loc,
                                           FuncDecl *fn,
                                           ArrayRef<Substitution> subs,
                                           ArrayRef<ManagedValue> args,
                                           SGFContext ctx);

  /// Emit a dynamic member reference.
  RValue emitDynamicMemberRefExpr(DynamicMemberRefExpr *e, SGFContext c);

  /// Emit a dynamic subscript.
  RValue emitDynamicSubscriptExpr(DynamicSubscriptExpr *e, SGFContext c);

  /// \brief Emit a conditional checked cast branch. Does not
  /// re-abstract the argument to the success branch. Terminates the
  /// current BB.
  ///
  /// \param loc          The AST location associated with the operation.
  /// \param src          The abstract value to cast.
  /// \param sourceType   The formal source type.
  /// \param targetType   The formal target type.
  /// \param C            Information about the result of the cast.
  /// \param handleTrue   A callback to invoke with the result of the cast
  ///                     in the success path.  The current BB should be
  ///                     terminated.
  /// \param handleFalse  A callback to invoke in the failure path.  The
  ///                     current BB should be terminated.
  void emitCheckedCastBranch(SILLocation loc, ConsumableManagedValue src,
                             CanType sourceType, CanType targetType,
                             SGFContext C,
                             std::function<void(ManagedValue)> handleTrue,
                             std::function<void()> handleFalse);

  /// \brief Emit a conditional checked cast branch, starting from an
  /// expression.  Terminates the current BB.
  ///
  /// \param loc          The AST location associated with the operation.
  /// \param src          An expression which will generate the value to cast.
  /// \param targetType   The formal target type.
  /// \param C            Information about the result of the cast.
  /// \param handleTrue   A callback to invoke with the result of the cast
  ///                     in the success path.  The current BB should be
  ///                     terminated.
  /// \param handleFalse  A callback to invoke in the failure path.  The
  ///                     current BB should be terminated.
  void emitCheckedCastBranch(SILLocation loc, Expr *src,
                             Type targetType, SGFContext C,
                             std::function<void(ManagedValue)> handleTrue,
                             std::function<void()> handleFalse);

  /// Initialize a memory location with an optional value.
  ///
  /// \param loc   The location to use for the resulting optional.
  /// \param value The value to inject into an optional.
  /// \param dest  The uninitialized memory in which to store the result value.
  /// \param optTL Type lowering information for the optional to create.
  void emitInjectOptionalValueInto(SILLocation loc,
                                   ArgumentSource &&value,
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

  /// \brief Emit a call to the library intrinsic _preconditionOptionalHasValue.
  void emitPreconditionOptionalHasValue(SILLocation loc, SILValue addr);
  
  /// \brief Emit a call to the library intrinsic _doesOptionalHaveValue.
  ///
  /// The result is a Builtin.Int1.
  SILValue emitDoesOptionalHaveValue(SILLocation loc, SILValue addr);
  
  /// \brief Emit a call to the library intrinsic _getOptionalValue
  /// given the address of the optional, which checks that an optional contains
  /// some value and either returns the value or traps if there is none.
  ManagedValue emitCheckedGetOptionalValueFrom(SILLocation loc,
                                               ManagedValue addr,
                                               const TypeLowering &optTL,
                                               SGFContext C);
  
  /// \brief Extract the value from an optional, which must be known to contain
  /// a value.
  ManagedValue emitUncheckedGetOptionalValueFrom(SILLocation loc,
                                                 ManagedValue addr,
                                                 const TypeLowering &optTL,
                                                 SGFContext C);

  typedef std::function<ManagedValue(SILGenFunction &gen,
                                     SILLocation loc,
                                     ManagedValue input,
                                     SILType loweredResultTy)> ValueTransform;

  /// Emit a transformation on the value of an optional type.
  ManagedValue emitOptionalToOptional(SILLocation loc,
                                      ManagedValue input,
                                      SILType loweredResultTy,
                                      const ValueTransform &transform);
  
  /// Build the type of a function transformation thunk.
  CanSILFunctionType buildThunkType(ManagedValue fn,
                                    CanSILFunctionType expectedType,
                                    CanSILFunctionType &substFnType,
                                    SmallVectorImpl<Substitution> &subs);
  
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

  /// Emit an Initialization for a 'var' or 'let' decl in a pattern.
  std::unique_ptr<Initialization>
  emitInitializationForVarDecl(VarDecl *vd, Type patternType);
  
  /// Emit the allocation for a local variable. Returns the address of the
  /// value. Does not register a cleanup.
  void emitLocalVariable(VarDecl *D,
                         Optional<MarkUninitializedInst::Kind> MUIKind);
  
  /// Emit the allocation for a local variable, provides an Initialization
  /// that can be used to initialize it, and registers cleanups in the active
  /// scope.
  std::unique_ptr<Initialization>
  emitLocalVariableWithCleanup(VarDecl *D, bool NeedsMarkUninit);

  /// Emit the allocation for a local temporary, provides an
  /// Initialization that can be used to initialize it, and registers
  /// cleanups in the active scope.
  ///
  /// The initialization is guaranteed to be a single buffer.
  std::unique_ptr<TemporaryInitialization>
  emitTemporary(SILLocation loc, const TypeLowering &tempTL);

  /// Provides an Initialization that can be used to initialize an already-
  /// allocated temporary, and registers cleanups in the active scope.
  ///
  /// The initialization is guaranteed to be a single buffer.
  std::unique_ptr<TemporaryInitialization>
  useBufferAsTemporary(SILLocation loc, SILValue addr,
                       const TypeLowering &tempTL);

  /// Enter a currently-dormant cleanup to destroy the value in the
  /// given address.
  CleanupHandle enterDormantTemporaryCleanup(SILValue temp,
                                             const TypeLowering &tempTL);

  /// Destroy and deallocate an initialized local variable.
  void destroyLocalVariable(SILLocation L, VarDecl *D);
  
  /// Deallocate an uninitialized local variable.
  void deallocateUninitializedLocalVariable(SILLocation L, VarDecl *D);

  /// Enter a cleanup to deallocate a stack variable.
  CleanupHandle enterDeallocStackCleanup(SILValue address);
  
  /// Enter a cleanup to emit a ReleaseValue/destroyAddr of the specified value.
  CleanupHandle enterDestroyCleanup(SILValue valueOrAddr);

  /// Evaluate an Expr as an lvalue.
  LValue emitLValue(Expr *E, AccessKind accessKind);

  /// Emit a reference to a variable as an lvalue.
  LValue emitLValueForAddressedNonMemberVarDecl(SILLocation loc, VarDecl *var,
                                                CanType formalRValueType,
                                                AccessKind accessKind,
                                                AccessSemantics semantics);

  /// Emit an lvalue that directly refers to the given instance variable
  /// (without going through getters or setters).
  LValue emitDirectIVarLValue(SILLocation loc, ManagedValue base, VarDecl *var,
                              AccessKind accessKind);
  
  /// Return forwarding substitutions for the archetypes in the current
  /// function.
  ArrayRef<Substitution> getForwardingSubstitutions();
  
  /// Get the _Pointer protocol used for pointer argument operations.
  ProtocolDecl *getPointerProtocol();
  
  /// Produce a substitution for invoking a pointer argument conversion
  /// intrinsic.
  Substitution getPointerSubstitution(Type pointerType,
                                      ArchetypeType *archetype);
  
  /// Get the method dispatch mechanism for a method.
  MethodDispatch getMethodDispatch(AbstractFunctionDecl *method);
};

} // end namespace Lowering
} // end namespace swift

#endif
