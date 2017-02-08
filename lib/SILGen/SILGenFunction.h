//===--- SILGenFunction.h - Function Specific AST lower context -*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_SILGENFUNCTION_H
#define SWIFT_SILGEN_SILGENFUNCTION_H

#include "Initialization.h"
#include "JumpDest.h"
#include "SILGen.h"
#include "SILGenBuilder.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {
  class ParameterList;
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
  
/// Internal context information for the SILGenFunction visitor.
///
/// In general, emission methods which take an SGFContext indicate
/// that they've initialized the emit-into buffer (if they have) by
/// returning a "isInContext()" ManagedValue of whatever type.  Callers who
/// propagate down an SGFContext that might have an emit-into buffer must be
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
    copy.state.setInt(state.getInt());
    return copy;
  }
};

enum class ApplyOptions : unsigned {
  /// No special treatment is required.
  None = 0,

  /// Suppress the error-handling edge out of the call.  This should
  /// be used carefully; it's used to implement features like 'rethrows'.
  DoesNotThrow = 0x1,
};
inline ApplyOptions operator|(ApplyOptions lhs, ApplyOptions rhs) {
  return ApplyOptions(unsigned(lhs) | unsigned(rhs));
}
inline ApplyOptions &operator|=(ApplyOptions &lhs, ApplyOptions rhs) {
  return (lhs = (lhs | rhs));
}
inline bool operator&(ApplyOptions lhs, ApplyOptions rhs) {
  return ((unsigned(lhs) & unsigned(rhs)) != 0);
}
inline ApplyOptions operator-(ApplyOptions lhs, ApplyOptions rhs) {
  return ApplyOptions(unsigned(lhs) & ~unsigned(rhs));
}
inline ApplyOptions &operator-=(ApplyOptions &lhs, ApplyOptions rhs) {
  return (lhs = (lhs - rhs));
}

class PatternMatchContext;
struct LValueWriteback;

/// A formal section of the function.  This is a SILGen-only concept,
/// meant to improve locality.  It's only reflected in the generated
/// SIL implicitly.
enum class FunctionSection : bool {
  /// The section of the function dedicated to ordinary control flow.
  Ordinary,

  /// The section of the function dedicated to error-handling and
  /// similar things.
  Postmatter,
};

/// Parameter to \c SILGenFunction::emitCaptures that indicates what the
/// capture parameters are being emitted for.
enum class CaptureEmission {
  /// Captures are being emitted for immediate application to a local function.
  ImmediateApplication,
  /// Captures are being emitted for partial application to form a closure
  /// value.
  PartialApplication,
};

/// Represents an LValue opened for mutating access.
///
/// This is used by LogicalPathComponent::getMaterialized() and
/// SILGenFunction::emitMaterializeForSetAccessor().
struct MaterializedLValue {
  ManagedValue temporary;

  // Only set if a callback is required
  CanType origSelfType;
  CanGenericSignature genericSig;
  SILValue callback;
  SILValue callbackStorage;

  MaterializedLValue() {}
  explicit MaterializedLValue(ManagedValue temporary)
    : temporary(temporary) {}
  MaterializedLValue(ManagedValue temporary,
                     CanType origSelfType,
                     CanGenericSignature genericSig,
                     SILValue callback,
                     SILValue callbackStorage)
    : temporary(temporary),
      origSelfType(origSelfType),
      genericSig(genericSig),
      callback(callback),
      callbackStorage(callbackStorage) {}
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

  /// The SILModuleConventions for this SIL module.
  SILModuleConventions silConv;

  /// The name of the function currently being emitted, as presented to user
  /// code by #function.
  DeclName MagicFunctionName;
  std::string MagicFunctionString;

  ASTContext &getASTContext() const { return SGM.M.getASTContext(); }

  /// The first block in the postmatter section of the function, if
  /// anything has been built there.
  ///
  /// (This field must precede B because B's initializer calls
  /// createBasicBlock().)
  SILFunction::iterator StartOfPostmatter;

  /// The current section of the function that we're emitting code in.
  ///
  /// The postmatter section is a part of the function intended for
  /// things like error-handling that don't need to be mixed into the
  /// normal code sequence.
  ///
  /// If the current function section is Ordinary, and
  /// StartOfPostmatter does not point to the function end, the current
  /// insertion block should be ordered before that.
  ///
  /// If the current function section is Postmatter, StartOfPostmatter
  /// does not point to the function end and the current insertion block is
  /// ordered after that (inclusive).
  ///
  /// (This field must precede B because B's initializer calls
  /// createBasicBlock().)
  FunctionSection CurFunctionSection = FunctionSection::Ordinary;

  /// \brief Does this function require a non-void direct return?
  bool NeedsReturn = false;

  /// \brief Is emission currently within a formal modification?
  bool InWritebackScope = false;

  /// \brief Is emission currently within an inout conversion?
  bool InInOutConversionScope = false;

  /// The SILGenBuilder used to construct the SILFunction.  It is what maintains
  /// the notion of the current block being emitted into.
  SILGenBuilder B;

  SILOpenedArchetypesTracker OpenedArchetypesTracker;

  struct BreakContinueDest {
    LabeledStmt *Target;
    JumpDest BreakDest;
    JumpDest ContinueDest;
  };
  
  std::vector<BreakContinueDest> BreakContinueDestStack;
  std::vector<PatternMatchContext*> SwitchStack;
  /// Keep track of our current nested scope.
  std::vector<SILDebugScope*> DebugScopeStack;

  /// The cleanup depth and BB for when the operand of a
  /// BindOptionalExpr is a missing value.
  SmallVector<JumpDest, 2> BindOptionalFailureDests;

  /// The cleanup depth and epilog BB for "return" statements.
  JumpDest ReturnDest = JumpDest::invalid();
  /// The cleanup depth and epilog BB for "fail" statements.
  JumpDest FailDest = JumpDest::invalid();

  /// The destination for throws.  The block will always be in the
  /// postmatter and takes a BB argument of the exception type.
  JumpDest ThrowDest = JumpDest::invalid();
    
  /// \brief The SIL location corresponding to the AST node being processed.
  SILLocation CurrentSILLoc;

  /// Cleanups - This records information about the currently active cleanups.
  CleanupManager Cleanups;

  /// The stack of pending writebacks.
  std::vector<LValueWriteback> *WritebackStack = 0;
  std::vector<LValueWriteback> &getWritebackStack();

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
  
  /// The metatype argument to an allocating constructor, if we're emitting one.
  SILValue AllocatorMetatype;

  /// True if 'return' without an operand or falling off the end of the current
  /// function is valid.
  bool allowsVoidReturn() const { return ReturnDest.getBlock()->args_empty(); }

  /// This location, when set, is used as an override location for magic
  /// identifier expansion (e.g. #file).  This allows default argument
  /// expansion to report the location of the call, instead of the location
  /// of the original expr.
  Optional<SourceLoc> overrideLocationForMagicIdentifiers;

  /// Emit code to increment a counter for profiling.
  void emitProfilerIncrement(ASTNode N) {
    if (SGM.Profiler && SGM.Profiler->hasRegionCounters())
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
  SILGenBuilder &getBuilder() { return B; }
  
  const TypeLowering &getTypeLowering(AbstractionPattern orig, Type subst) {
    return SGM.Types.getTypeLowering(orig, subst);
  }
  const TypeLowering &getTypeLowering(Type t) {
    return SGM.Types.getTypeLowering(t);
  }
  CanSILFunctionType getSILFunctionType(AbstractionPattern orig,
                                        CanFunctionType substFnType,
                                        unsigned uncurryLevel) {
    return SGM.Types.getSILFunctionType(orig, substFnType, uncurryLevel);
  }
  SILType getLoweredType(AbstractionPattern orig, Type subst) {
    return SGM.Types.getLoweredType(orig, subst);
  }
  SILType getLoweredType(Type t) {
    return SGM.Types.getLoweredType(t);
  }
  SILType getLoweredLoadableType(Type t) {
    return SGM.Types.getLoweredLoadableType(t);
  }

  const TypeLowering &getTypeLowering(SILType type) {
    return SGM.Types.getTypeLowering(type);
  }

  SILType getSILType(SILParameterInfo param) const {
    return silConv.getSILType(param);
  }
  SILType getSILType(SILResultInfo result) const {
    return silConv.getSILType(result);
  }

  SILConstantInfo getConstantInfo(SILDeclRef constant) {
    return SGM.Types.getConstantInfo(constant);
  }

  SourceManager &getSourceManager() { return SGM.M.getASTContext().SourceMgr; }

  /// Push a new debug scope and set its parent pointer.
  void enterDebugScope(SILLocation Loc) {
    auto *Parent =
        DebugScopeStack.size() ? DebugScopeStack.back() : F.getDebugScope();
    auto *DS = new (SGM.M) SILDebugScope(Loc, &getFunction(), Parent);
    DebugScopeStack.push_back(DS);
    B.setCurrentDebugScope(DS);
  }

  /// Return to the previous debug scope.
  void leaveDebugScope() {
    DebugScopeStack.pop_back();
    if (DebugScopeStack.size())
      B.setCurrentDebugScope(DebugScopeStack.back());
    // Don't reset the debug scope after leaving the outermost scope,
    // because the debugger is not expecting the function epilogue to
    // be in a different scope.
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
  /// \param dc The DeclContext containing the current function.
  /// \param selfDecl The 'self' declaration within the current function.
  /// \param nominal The type whose members are being initialized.
  void emitMemberInitializers(DeclContext *dc, VarDecl *selfDecl,
                              NominalTypeDecl *nominal);

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
  void emitCurryThunk(ValueDecl *fd,
                      SILDeclRef fromLevel, SILDeclRef toLevel);
  /// Generates a thunk from a foreign function to the native Swift convention.
  void emitForeignToNativeThunk(SILDeclRef thunk);
  /// Generates a thunk from a native function to the conventions.
  void emitNativeToForeignThunk(SILDeclRef thunk);
  
  /// Generate a nullary function that returns the given value.
  void emitGeneratorFunction(SILDeclRef function, Expr *value);

  /// Generate an ObjC-compatible destructor (-dealloc).
  void emitObjCDestructor(SILDeclRef dtor);

  ManagedValue emitGlobalVariableRef(SILLocation loc, VarDecl *var);

  /// Generate a lazy global initializer.
  void emitLazyGlobalInitializer(PatternBindingDecl *binding,
                                 unsigned pbdEntry);
  
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
  ///
  /// This is used for both concrete witness thunks and default witness
  /// thunks.
  void emitProtocolWitness(Type selfType,
                           AbstractionPattern reqtOrigTy,
                           CanAnyFunctionType reqtSubstTy,
                           SILDeclRef requirement,
                           SILDeclRef witness,
                           SubstitutionList witnessSubs,
                           IsFreeFunctionWitness_t isFree);
  
  /// Convert a block to a native function with a thunk.
  ManagedValue emitBlockToFunc(SILLocation loc,
                               ManagedValue block,
                               CanSILFunctionType funcTy);

  /// Convert a native function to a block with a thunk.
  ManagedValue emitFuncToBlock(SILLocation loc,
                               ManagedValue block,
                               CanSILFunctionType funcTy);
  
  /// Thunk with the signature of a base class method calling a derived class
  /// method.
  ///
  /// \param inputOrigType Abstraction pattern of base class method
  /// \param inputSubstType Formal AST type of base class method
  /// \param outputSubstType Formal AST type of derived class method
  void emitVTableThunk(SILDeclRef derived,
                       SILFunction *implFn,
                       AbstractionPattern inputOrigType,
                       CanAnyFunctionType inputSubstType,
                       CanAnyFunctionType outputSubstType);
  
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

  /// Create a new basic block.
  ///
  /// The block can be explicitly placed after a particular block.
  /// Otherwise, if the current insertion point is valid, it will be
  /// placed immediately after it.  Otherwise, it will be placed at the
  /// end of the current function section.
  ///
  /// Because basic blocks are generally constructed with an insertion
  /// point active, users should be aware that this behavior leads to
  /// an emergent LIFO ordering: if code generation requires multiple
  /// blocks, the second block created will be positioned before the
  /// first block.  (This is clearly desirable behavior when blocks
  /// are created by different emissions; it's just a little
  /// counter-intuitive within a single emission.)
  SILBasicBlock *createBasicBlock(SILBasicBlock *afterBB = nullptr);  

  /// Create a new basic block at the end of the given function
  /// section.
  SILBasicBlock *createBasicBlock(FunctionSection section);

  /// Erase a basic block that was speculatively created and turned
  /// out to be unneeded.
  ///
  /// This should be called instead of eraseFromParent() in order to
  /// keep SILGen's internal bookkeeping consistent. 
  ///
  /// The block should be empty and have no predecessors.
  void eraseBasicBlock(SILBasicBlock *block);

  //===--------------------------------------------------------------------===//
  // Memory management
  //===--------------------------------------------------------------------===//

  /// Emit debug info for the artificial error inout argument.
  void emitErrorArgument(SILLocation Loc, unsigned ArgNo);

  /// emitProlog - Generates prolog code to allocate and clean up mutable
  /// storage for closure captures and local arguments.
  void emitProlog(AnyFunctionRef TheClosure,
                  ArrayRef<ParameterList *> paramPatterns, Type resultType,
                  bool throws);
  /// returns the number of variables in paramPatterns.
  unsigned emitProlog(ArrayRef<ParameterList *> paramPatterns, Type resultType,
                      DeclContext *DeclCtx, bool throws);

  /// Create SILArguments in the entry block that bind all the values
  /// of the given pattern suitably for being forwarded.
  void bindParametersForForwarding(const ParameterList *params,
                                   SmallVectorImpl<SILValue> &parameters);

  /// \brief Create (but do not emit) the epilog branch, and save the
  /// current cleanups depth as the destination for return statement branches.
  ///
  /// \param returnType  If non-null, the epilog block will be created with an
  ///                    argument of this type to receive the return value for
  ///                    the function.
  /// \param isThrowing  If true, create an error epilog block.
  /// \param L           The SILLocation which should be associated with
  ///                    cleanup instructions.
  void prepareEpilog(Type returnType, bool isThrowing, CleanupLocation L);
  void prepareRethrowEpilog(CleanupLocation l);
  
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
  ///          of the return instruction if the epilog block is supposed to host
  ///          the ReturnLocation (This happens in case the predecessor block is
  ///          merged with the epilog block.)
  std::pair<Optional<SILValue>, SILLocation>
    emitEpilogBB(SILLocation TopLevelLoc);
  
  /// \brief Emits a standard epilog which runs top-level cleanups then returns
  /// the function return value, if any.  This can be customized by clients, who
  /// set UsesCustomEpilog to true, and optionally inject their own code into
  /// the epilog block before calling this.  If they do this, their code is run
  /// before the top-level cleanups, and the epilog block to continue is
  /// returned as the insertion point of this function.  They must provide the
  /// final exit sequence for the block as well.
  ///
  /// \param TopLevelLoc The location of the top-level expression during whose
  ///        evaluation the epilog is being produced, for example, the
  ///        AbstractClosureExpr.
  /// \param UsesCustomEpilog True if the client wants to manage its own epilog
  ///        logic.
  SILLocation emitEpilog(SILLocation TopLevelLoc,bool UsesCustomEpilog = false);

  /// \brief Emits the standard rethrow epilog using a Swift error result.
  void emitRethrowEpilog(SILLocation topLevelLoc);

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
  // Type conversions for expr emission and thunks
  //===--------------------------------------------------------------------===//

  ManagedValue emitInjectEnum(SILLocation loc,
                              ArgumentSource payload,
                              SILType enumTy,
                              EnumElementDecl *element,
                              SGFContext C);

  ManagedValue emitInjectOptional(SILLocation loc,
                                  const TypeLowering &expectedTL,
                                  SGFContext ctxt,
                       llvm::function_ref<ManagedValue(SGFContext)> generator);

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

  /// Return a value for an optional ".None" of the specified type. This only
  /// works for loadable enum types.
  SILValue getOptionalNoneValue(SILLocation loc, const TypeLowering &optTL);

  /// Return a value for an optional ".Some(x)" of the specified type. This only
  /// works for loadable enum types.
  ManagedValue getOptionalSomeValue(SILLocation loc, ManagedValue value,
                                    const TypeLowering &optTL);

  /// \brief Emit a call to the library intrinsic _doesOptionalHaveValue.
  ///
  /// The result is a Builtin.Int1.
  SILValue emitDoesOptionalHaveValue(SILLocation loc, SILValue addrOrValue);

  /// \brief Emit a switch_enum to call the library intrinsic
  /// _diagnoseUnexpectedNilOptional if the optional has no value.
  void emitPreconditionOptionalHasValue(SILLocation loc, SILValue addr);

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
                                                 ManagedValue addrOrValue,
                                                 const TypeLowering &optTL,
                                                 SGFContext C = SGFContext());

  typedef llvm::function_ref<ManagedValue(SILGenFunction &gen,
                                    SILLocation loc,
                                    ManagedValue input,
                                    SILType loweredResultTy)> ValueTransformRef;

  /// Emit a transformation on the value of an optional type.
  ManagedValue emitOptionalToOptional(SILLocation loc,
                                      ManagedValue input,
                                      SILType loweredResultTy,
                                      ValueTransformRef transform);

  /// Emit a reinterpret-cast from one pointer type to another, using a library
  /// intrinsic.
  RValue emitPointerToPointer(SILLocation loc,
                              ManagedValue input,
                              CanType inputTy,
                              CanType outputTy,
                              SGFContext C = SGFContext());

  ManagedValue emitClassMetatypeToObject(SILLocation loc,
                                         ManagedValue v,
                                         SILType resultTy);

  ManagedValue emitExistentialMetatypeToObject(SILLocation loc,
                                               ManagedValue v,
                                               SILType resultTy);

  ManagedValue emitProtocolMetatypeToObject(SILLocation loc,
                                            CanType inputTy,
                                            SILType resultTy);

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

  struct OpaqueValueState {
    ManagedValue Value;
    bool IsConsumable;
    bool HasBeenConsumed;
  };

  ManagedValue manageOpaqueValue(OpaqueValueState &entry,
                                 SILLocation loc,
                                 SGFContext C);

  /// Open up the given existential value and project its payload.
  ///
  /// \param existentialValue The existential value.
  /// \param openedArchetype The opened existential archetype.
  /// \param loweredOpenedType The lowered type of the projection, which in
  /// practice will be the openedArchetype, possibly wrapped in a metatype.
  SILGenFunction::OpaqueValueState
  emitOpenExistential(SILLocation loc,
                      ManagedValue existentialValue,
                      CanArchetypeType openedArchetype,
                      SILType loweredOpenedType);

  /// \brief Wrap the given value in an existential container.
  ///
  /// \param concreteFormalType AST type of value.
  /// \param concreteTL Type lowering of value.
  /// \param existentialTL Type lowering of existential type.
  /// \param F Function reference to emit the existential contents with the
  /// given context.
  ManagedValue emitExistentialErasure(
                            SILLocation loc,
                            CanType concreteFormalType,
                            const TypeLowering &concreteTL,
                            const TypeLowering &existentialTL,
                            ArrayRef<ProtocolConformanceRef> conformances,
                            SGFContext C,
                            llvm::function_ref<ManagedValue (SGFContext)> F,
                            bool allowEmbeddedNSError = true);

  //===--------------------------------------------------------------------===//
  // Recursive entry points
  //===--------------------------------------------------------------------===//

  using ASTVisitorType::visit;
  
  //===--------------------------------------------------------------------===//
  // Statements
  //===--------------------------------------------------------------------===//

  void visit(Stmt *S) = delete;

  void emitStmt(Stmt *S);

  void emitBreakOutOf(SILLocation loc, Stmt *S);

  void emitCatchDispatch(DoCatchStmt *S, ManagedValue exn,
                         ArrayRef<CatchStmt*> clauses,
                         JumpDest catchFallthroughDest);

  /// Emit code for the throw expr. If \p emitWillThrow is set then emit a
  /// call to swift_willThrow, that will allow the debugger to place a
  /// breakpoint on throw sites.
  void emitThrow(SILLocation loc, ManagedValue exn, bool emitWillThrow = false);
  
  //===--------------------------------------------------------------------===//
  // Patterns
  //===--------------------------------------------------------------------===//

  SILValue emitOSVersionRangeCheck(SILLocation loc, const VersionRange &range);
  void emitStmtCondition(StmtCondition Cond, JumpDest FailDest,
                         SILLocation loc);

  void emitConditionalPBD(PatternBindingDecl *PBD, SILBasicBlock *FailBB);

  void usingImplicitVariablesForPattern(Pattern *pattern, CaseStmt *stmt,
                                        const llvm::function_ref<void(void)> &f);
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

  /// Emit an r-value into temporary memory and return the managed address.
  ManagedValue
  emitMaterializedRValueAsOrig(Expr *E, AbstractionPattern origPattern);
  
  /// Emit the given expression, ignoring its result.
  void emitIgnoredExpr(Expr *E);
  
  /// Emit the given expression as an r-value, then (if it is a tuple), combine
  /// it together into a single ManagedValue.
  ManagedValue emitRValueAsSingleValue(Expr *E, SGFContext C = SGFContext());

  /// Emit 'undef' in a particular formal type.
  ManagedValue emitUndef(SILLocation loc, Type type);
  ManagedValue emitUndef(SILLocation loc, SILType type);
  RValue emitUndefRValue(SILLocation loc, Type type);
  
  std::pair<ManagedValue, SILValue>
  emitUninitializedArrayAllocation(Type ArrayTy,
                                   SILValue Length,
                                   SILLocation Loc);

  CleanupHandle enterDeallocateUninitializedArrayCleanup(SILValue array);
  void emitUninitializedArrayDeallocation(SILLocation loc, SILValue array);

  SILValue emitConversionToSemanticRValue(SILLocation loc, SILValue value,
                                          const TypeLowering &valueTL);

  ManagedValue emitConversionToSemanticRValue(SILLocation loc,
                                              ManagedValue value,
                                              const TypeLowering &valueTL);

  /// Emit the empty tuple value by emitting
  SILValue emitEmptyTuple(SILLocation loc);
  /// "Emit" an RValue representing an empty tuple.
  RValue emitEmptyTupleRValue(SILLocation loc, SGFContext C);

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

  /// Emit the specified VarDecl as an LValue if possible, otherwise return
  /// null.
  ManagedValue emitLValueForDecl(SILLocation loc, VarDecl *var,
                                 CanType formalRValueType,
                                 AccessKind accessKind,
                                 AccessSemantics semantics
                                   = AccessSemantics::Ordinary);
  
  /// Produce an RValue for a reference to the specified declaration,
  /// with the given type and in response to the specified expression.  Try to
  /// emit into the specified SGFContext to avoid copies (when provided).
  RValue emitRValueForDecl(SILLocation loc, ConcreteDeclRef decl, Type ty,
                           AccessSemantics semantics,
                           SGFContext C = SGFContext());

  /// Produce an RValue for a load from the specified property.
  RValue emitRValueForPropertyLoad(SILLocation loc,
                                   ManagedValue base,
                                   CanType baseFormalType,
                                   bool isSuper, VarDecl *property,
                                   SubstitutionList substitutions,
                                   AccessSemantics semantics, Type propTy,
                                   SGFContext C,
                                   bool isGuaranteedValid = false);

  void emitCaptures(SILLocation loc,
                    AnyFunctionRef TheClosure,
                    CaptureEmission purpose,
                    SmallVectorImpl<ManagedValue> &captures);

  /// Produce a reference to a function, which may be a local function
  /// with captures. If the function is generic, substitutions must be
  /// given. The result is re-abstracted to the given expected type.
  ManagedValue emitClosureValue(SILLocation loc,
                                SILDeclRef function,
                                CanType expectedType,
                                SubstitutionList subs);
  
  ArgumentSource prepareAccessorBaseArg(SILLocation loc, ManagedValue base,
                                        CanType baseFormalType,
                                        SILDeclRef accessor);

  SILDeclRef getGetterDeclRef(AbstractStorageDecl *decl,
                              bool isDirectAccessorUse);  
  RValue emitGetAccessor(SILLocation loc, SILDeclRef getter,
                         SubstitutionList substitutions,
                         ArgumentSource &&optionalSelfValue,
                         bool isSuper, bool isDirectAccessorUse,
                         RValue &&optionalSubscripts, SGFContext C);

  SILDeclRef getSetterDeclRef(AbstractStorageDecl *decl,
                              bool isDirectAccessorUse);  
  void emitSetAccessor(SILLocation loc, SILDeclRef setter,
                       SubstitutionList substitutions,
                       ArgumentSource &&optionalSelfValue,
                       bool isSuper, bool isDirectAccessorUse,
                       RValue &&optionalSubscripts, RValue &&value);

  SILDeclRef getMaterializeForSetDeclRef(AbstractStorageDecl *decl,
                                         bool isDirectAccessorUse);  
  MaterializedLValue
  emitMaterializeForSetAccessor(SILLocation loc, SILDeclRef materializeForSet,
                                SubstitutionList substitutions,
                                ArgumentSource &&optionalSelfValue,
                                bool isSuper, bool isDirectAccessorUse,
                                RValue &&optionalSubscripts,
                                SILValue buffer, SILValue callbackStorage);
  bool maybeEmitMaterializeForSetThunk(ProtocolConformance *conformance,
                                       SILLinkage linkage,
                                       Type selfInterfaceType,
                                       Type selfType,
                                       GenericEnvironment *genericEnv,
                                       FuncDecl *requirement,
                                       FuncDecl *witness,
                                       SubstitutionList witnessSubs);
  void emitMaterializeForSet(FuncDecl *decl);

  SILDeclRef getAddressorDeclRef(AbstractStorageDecl *decl,
                                 AccessKind accessKind,
                                 bool isDirectAccessorUse);
  std::pair<ManagedValue,ManagedValue>
  emitAddressorAccessor(SILLocation loc, SILDeclRef addressor,
                        SubstitutionList substitutions,
                        ArgumentSource &&optionalSelfValue,
                        bool isSuper, bool isDirectAccessorUse,
                        RValue &&optionalSubscripts,
                        SILType addressType);

  RValue emitApplyConversionFunction(SILLocation loc,
                                     Expr *funcExpr,
                                     Type resultType,
                                     RValue &&operand);

  ManagedValue emitManagedRetain(SILLocation loc, SILValue v);
  ManagedValue emitManagedRetain(SILLocation loc, SILValue v,
                                 const TypeLowering &lowering);

  ManagedValue emitManagedLoadCopy(SILLocation loc, SILValue v);
  ManagedValue emitManagedLoadCopy(SILLocation loc, SILValue v,
                                   const TypeLowering &lowering);

  ManagedValue emitManagedStoreBorrow(SILLocation loc, SILValue v,
                                      SILValue addr);
  ManagedValue emitManagedStoreBorrow(SILLocation loc, SILValue v,
                                      SILValue addr,
                                      const TypeLowering &lowering);

  ManagedValue emitManagedLoadBorrow(SILLocation loc, SILValue v);
  ManagedValue emitManagedLoadBorrow(SILLocation loc, SILValue v,
                                     const TypeLowering &lowering);

  ManagedValue emitManagedBeginBorrow(SILLocation loc, SILValue v,
                                      const TypeLowering &lowering);
  ManagedValue emitManagedBeginBorrow(SILLocation loc, SILValue v);

  ManagedValue emitManagedBorrowedRValueWithCleanup(SILValue original,
                                                    SILValue borrowedValue);
  ManagedValue emitManagedBorrowedRValueWithCleanup(
      SILValue original, SILValue borrowedValue, const TypeLowering &lowering);

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
  RValue emitLoadOfLValue(SILLocation loc, LValue &&src, SGFContext C,
                          bool isGuaranteedValid = false);

  /// Emit a reference to a method from within another method of the type, and
  /// gather all the substitutions necessary to invoke it, without
  /// dynamic dispatch.
  std::tuple<ManagedValue, SILType, SubstitutionList>
  emitSiblingMethodRef(SILLocation loc,
                       SILValue selfValue,
                       SILDeclRef methodConstant,
                       SubstitutionList innerSubstitutions);
  
  SILValue emitMetatypeOfValue(SILLocation loc, Expr *baseExpr);
  
  void emitReturnExpr(SILLocation loc, Expr *ret);

  RValue emitAnyHashableErasure(SILLocation loc,
                                ManagedValue value,
                                Type type,
                                ProtocolConformanceRef conformance,
                                SGFContext C);

  /// Turn a consumable managed value into a +1 managed value.
  ManagedValue getManagedValue(SILLocation loc,
                               ConsumableManagedValue value);

  //
  // Helpers for emitting ApplyExpr chains.
  //
  
  RValue emitApplyExpr(Expr *e, SGFContext c);

  /// Emit a function application, assuming that the arguments have been
  /// lowered appropriately for the abstraction level but that the
  /// result does need to be turned back into something matching a
  /// formal type.
  RValue emitApply(SILLocation loc,
                   ManagedValue fn,
                   SubstitutionList subs,
                   ArrayRef<ManagedValue> args,
                   CanSILFunctionType substFnType,
                   AbstractionPattern origResultType,
                   CanType substResultType,
                   ApplyOptions options,
                   Optional<SILFunctionTypeRepresentation> overrideRep,
                   const Optional<ForeignErrorConvention> &foreignError,
                   SGFContext evalContext);

  RValue emitApplyOfDefaultArgGenerator(SILLocation loc,
                                        ConcreteDeclRef defaultArgsOwner,
                                        unsigned destIndex,
                                        CanType resultType,
                                        AbstractionPattern origResultType,
                                        SGFContext C = SGFContext());

  RValue emitApplyOfStoredPropertyInitializer(
      SILLocation loc,
      const PatternBindingEntry &entry,
      SubstitutionList subs,
      CanType resultType,
      AbstractionPattern origResultType,
      SGFContext C);

  /// A convenience method for emitApply that just handles monomorphic
  /// applications.
  RValue emitMonomorphicApply(SILLocation loc,
                              ManagedValue fn,
                              ArrayRef<ManagedValue> args,
                              CanType resultType,
                              ApplyOptions options,
                    Optional<SILFunctionTypeRepresentation> overrideRep,
                    const Optional<ForeignErrorConvention> &foreignError);


  RValue emitApplyOfLibraryIntrinsic(SILLocation loc,
                                     FuncDecl *fn,
                                     SubstitutionList subs,
                                     ArrayRef<ManagedValue> args,
                                     SGFContext ctx);

  SILValue emitApplyWithRethrow(SILLocation loc, SILValue fn,
                                SILType substFnType,
                                SubstitutionList subs,
                                ArrayRef<SILValue> args);

  /// Emit a literal that applies the various initializers.
  RValue emitLiteral(LiteralExpr *literal, SGFContext C);

  SILBasicBlock *getTryApplyErrorDest(SILLocation loc,
                                      SILResultInfo exnResult,
                                      bool isSuppressed);

  /// Emit a dynamic member reference.
  RValue emitDynamicMemberRefExpr(DynamicMemberRefExpr *e, SGFContext c);

  /// Emit a dynamic subscript.
  RValue emitDynamicSubscriptExpr(DynamicSubscriptExpr *e, SGFContext c);

  /// Open up the given existential expression and emit its
  /// subexpression in a caller-specified manner.
  ///
  /// \param e The expression.
  ///
  /// \param emitSubExpr A function to call to emit the subexpression
  /// (which will be passed in).
  void emitOpenExistentialExprImpl(OpenExistentialExpr *e,
                                  llvm::function_ref<void(Expr *)> emitSubExpr);

  /// Open up the given existential expression and emit its
  /// subexpression in a caller-specified manner.
  ///
  /// \param e The expression.
  ///
  /// \param emitSubExpr A function to call to emit the subexpression
  /// (which will be passed in).
  template<typename R, typename F>
  R emitOpenExistentialExpr(OpenExistentialExpr *e, F emitSubExpr) {
    Optional<R> result;
    emitOpenExistentialExprImpl(e,
                            [&](Expr *subExpr) {
                              result.emplace(emitSubExpr(subExpr));
                            });
    return std::move(*result);
  }

  /// Open up the given existential expression and emit its
  /// subexpression in a caller-specified manner.
  ///
  /// \param e The expression.
  ///
  /// \param emitSubExpr A function to call to emit the subexpression
  /// (which will be passed in).
  template<typename F>
  void emitOpenExistentialExpr(OpenExistentialExpr *e, F emitSubExpr) {
    emitOpenExistentialExprImpl(e, emitSubExpr);
  }

  /// Mapping from active opaque value expressions to their values,
  /// along with a bit for each indicating whether it has been consumed yet.
  llvm::DenseMap<OpaqueValueExpr *, OpaqueValueState> OpaqueValues;

  /// RAII object that introduces a temporary binding for an opaque value.
  ///
  /// Each time the opaque value expression is referenced, it will be
  /// retained/released separately. When this RAII object goes out of
  /// scope, the value will be destroyed if requested.
  class OpaqueValueRAII {
    SILGenFunction &Self;
    OpaqueValueExpr *OpaqueValue;

    OpaqueValueRAII(const OpaqueValueRAII &) = delete;
    OpaqueValueRAII &operator=(const OpaqueValueRAII &) = delete;

  public:
    OpaqueValueRAII(SILGenFunction &self, OpaqueValueExpr *opaqueValue,
                    OpaqueValueState state)
    : Self(self), OpaqueValue(opaqueValue) {
      assert(Self.OpaqueValues.count(OpaqueValue) == 0 &&
             "Opaque value already has a binding");
      Self.OpaqueValues[OpaqueValue] = state;
    }

    ~OpaqueValueRAII();
  };

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
                             Type sourceType, CanType targetType,
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

  /// Emit the control flow for an optional 'bind' operation, branching to the
  /// active failure destination if the optional value addressed by optionalAddr
  /// is nil, and leaving the insertion point on the success branch.
  ///
  /// NOTE: This operation does *not* consume the managed value.
  ///
  void emitBindOptional(SILLocation loc, ManagedValue optionalAddrOrValue,
                        unsigned depth);

  //===--------------------------------------------------------------------===//
  // Bridging thunks
  //===--------------------------------------------------------------------===//

  /// Convert a native Swift value to a value that can be passed as an argument
  /// to or returned as the result of a function with the given calling
  /// convention.
  ManagedValue emitNativeToBridgedValue(SILLocation loc, ManagedValue v,
                                        SILFunctionTypeRepresentation destRep,
                                        CanType bridgedTy);
  
  /// Convert a value received as the result or argument of a function with
  /// the given calling convention to a native Swift value of the given type.
  ManagedValue emitBridgedToNativeValue(SILLocation loc, ManagedValue v,
                                        SILFunctionTypeRepresentation srcRep,
                                        CanType nativeTy);

  /// Convert a bridged error type to the native Swift Error
  /// representation.  The value may be optional.
  ManagedValue emitBridgedToNativeError(SILLocation loc, ManagedValue v);

  /// Convert a value in the native Swift Error representation to
  /// a bridged error type representation.
  ManagedValue emitNativeToBridgedError(SILLocation loc, ManagedValue v,
                                        CanType bridgedType);
  
  SILValue emitBridgeErrorForForeignError(SILLocation loc,
                                          SILValue nativeError,
                                          SILType bridgedResultType,
                                          SILValue foreignErrorSlot,
                                    const ForeignErrorConvention &foreignError);

  SILValue
  emitBridgeReturnValueForForeignError(SILLocation loc,
                                       SILValue result,
                                       SILFunctionTypeRepresentation repr,
                                       SILType bridgedResultType,
                                       SILValue foreignErrorSlot,
                                 const ForeignErrorConvention &foreignError);

  void emitForeignErrorBlock(SILLocation loc, SILBasicBlock *errorBB,
                             Optional<ManagedValue> errorSlot);

  void emitForeignErrorCheck(SILLocation loc,
                             SmallVectorImpl<ManagedValue> &directResults,
                             ManagedValue errorSlot,
                             bool suppressErrorCheck,
                             const ForeignErrorConvention &foreignError);

  //===--------------------------------------------------------------------===//
  // Re-abstraction thunks
  //===--------------------------------------------------------------------===//

  /// Convert a value with the abstraction patterns of the original type
  /// to a value with the abstraction patterns of the substituted type.
  ManagedValue emitOrigToSubstValue(SILLocation loc, ManagedValue input,
                                    AbstractionPattern origType,
                                    CanType substType,
                                    SGFContext ctx = SGFContext());
  RValue emitOrigToSubstValue(SILLocation loc, RValue &&input,
                              AbstractionPattern origType,
                              CanType substType,
                              SGFContext ctx = SGFContext());

  /// Convert a value with the abstraction patterns of the substituted
  /// type to a value with the abstraction patterns of the original type.
  ManagedValue emitSubstToOrigValue(SILLocation loc, ManagedValue input,
                                    AbstractionPattern origType,
                                    CanType substType,
                                    SGFContext ctx = SGFContext());
  RValue emitSubstToOrigValue(SILLocation loc, RValue &&input,
                              AbstractionPattern origType,
                              CanType substType,
                              SGFContext ctx = SGFContext());

  /// Transform the AST-level types in the function signature without an
  /// abstraction or representation change.
  ManagedValue emitTransformedValue(SILLocation loc, ManagedValue input,
                                    CanType inputType,
                                    CanType outputType,
                                    SGFContext ctx = SGFContext());

  /// Most general form of the above.
  ManagedValue emitTransformedValue(SILLocation loc, ManagedValue input,
                                    AbstractionPattern inputOrigType,
                                    CanType inputSubstType,
                                    AbstractionPattern outputOrigType,
                                    CanType outputSubstType,
                                    SGFContext ctx = SGFContext());
  RValue emitTransformedValue(SILLocation loc, RValue &&input,
                              AbstractionPattern inputOrigType,
                              CanType inputSubstType,
                              AbstractionPattern outputOrigType,
                              CanType outputSubstType,
                              SGFContext ctx = SGFContext());

  /// Used for emitting SILArguments of bare functions, such as thunks and
  /// open-coded materializeForSet.
  void collectThunkParams(SILLocation loc,
                          SmallVectorImpl<ManagedValue> &params,
                          bool allowPlusZero);

  /// Build the type of a function transformation thunk.
  CanSILFunctionType buildThunkType(CanSILFunctionType &sourceType,
                                    CanSILFunctionType &expectedType,
                                    CanType &inputSubstType,
                                    CanType &outputSubstType,
                                    GenericEnvironment *&genericEnv,
                                    SubstitutionMap &interfaceSubs);

  //===--------------------------------------------------------------------===//
  // Declarations
  //===--------------------------------------------------------------------===//
  
  void visitDecl(Decl *D) {
    llvm_unreachable("Not yet implemented");
  }

  void visitFuncDecl(FuncDecl *D);
  void visitPatternBindingDecl(PatternBindingDecl *D);

  void emitPatternBinding(PatternBindingDecl *D, unsigned entry);
  
  std::unique_ptr<Initialization>
  emitPatternBindingInitialization(Pattern *P, JumpDest failureDest);
    
  void visitNominalTypeDecl(NominalTypeDecl *D) {
    // No lowering support needed.
  }

  void visitTypeAliasDecl(TypeAliasDecl *D) {
    // No lowering support needed.
  }

  void visitGenericTypeParamDecl(GenericTypeParamDecl *D) {
    // No lowering support needed.
  }
  void visitAssociatedTypeDecl(AssociatedTypeDecl *D) {
    // No lowering support needed.
  }

  void visitVarDecl(VarDecl *D);

  /// Emit an Initialization for a 'var' or 'let' decl in a pattern.
  std::unique_ptr<Initialization> emitInitializationForVarDecl(VarDecl *vd);
  
  /// Emit the allocation for a local variable, provides an Initialization
  /// that can be used to initialize it, and registers cleanups in the active
  /// scope.
  /// \param ArgNo optionally describes this function argument's
  /// position for debug info.
  std::unique_ptr<Initialization>
  emitLocalVariableWithCleanup(VarDecl *D, bool NeedsMarkUninit,
                               unsigned ArgNo = 0);

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
  useBufferAsTemporary(SILValue addr, const TypeLowering &tempTL);

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
  
  /// Enter a cleanup to emit a ReleaseValue/DestroyAddr of the specified value.
  CleanupHandle enterDestroyCleanup(SILValue valueOrAddr);
  
  /// Enter a cleanup to emit a DeinitExistentialAddr or DeinitExistentialBox
  /// of the specified value.
  CleanupHandle enterDeinitExistentialCleanup(SILValue valueOrAddr,
                                              CanType concreteFormalType,
                                              ExistentialRepresentation repr);

  /// Enter a cleanup to emit an EndBorrow stating that \p borrowed (the
  /// borrowed entity) is no longer borrowed from \p original, the original
  /// value.
  CleanupHandle enterEndBorrowCleanup(SILValue original, SILValue borrowed);

  /// Evaluate an Expr as an lvalue.
  LValue emitLValue(Expr *E, AccessKind accessKind);

  /// Emit a reference to a variable as an lvalue.
  LValue emitLValueForAddressedNonMemberVarDecl(SILLocation loc, VarDecl *var,
                                                CanType formalRValueType,
                                                AccessKind accessKind,
                                                AccessSemantics semantics);

  /// Emit an lvalue that directly refers to the given instance variable
  /// (without going through getters or setters).
  LValue emitPropertyLValue(SILLocation loc, ManagedValue base,
                            CanType baseFormalType, VarDecl *var,
                            AccessKind accessKind, AccessSemantics semantics);

  ManagedValue emitLValueToPointer(SILLocation loc, LValue &&lvalue,
                                   CanType pointerType, PointerTypeKind ptrKind,
                                   AccessKind accessKind);
  
  /// Return forwarding substitutions for the archetypes in the current
  /// function.
  SubstitutionList getForwardingSubstitutions();
  
  /// Get the _Pointer protocol used for pointer argument operations.
  ProtocolDecl *getPointerProtocol();
  
  /// Produce a substitution for invoking a pointer argument conversion
  /// intrinsic.
  Substitution getPointerSubstitution(Type pointerType);
};


/// A utility class for saving and restoring the insertion point.
class SavedInsertionPoint {
  SILGenFunction &SGF;
  SILBasicBlock *SavedIP;
  FunctionSection SavedSection;
public:
  SavedInsertionPoint(SILGenFunction &SGF, SILBasicBlock *newIP,
                      Optional<FunctionSection> optSection = None)
    : SGF(SGF), SavedIP(SGF.B.getInsertionBB()),
      SavedSection(SGF.CurFunctionSection) {
    FunctionSection section = (optSection ? *optSection : SavedSection);
    assert((section != FunctionSection::Postmatter ||
            SGF.StartOfPostmatter != SGF.F.end()) &&
           "trying to move to postmatter without a registered start "
           "of postmatter?");

    SGF.B.setInsertionPoint(newIP);
    SGF.CurFunctionSection = section;
  }

  SavedInsertionPoint(const SavedInsertionPoint &) = delete;
  SavedInsertionPoint &operator=(const SavedInsertionPoint &) = delete;

  ~SavedInsertionPoint() {
    if (SavedIP) {
      SGF.B.setInsertionPoint(SavedIP);
    } else {
      SGF.B.clearInsertionPoint();
    }
    SGF.CurFunctionSection = SavedSection;
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
