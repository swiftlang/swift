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

#include "Cleanup.h"
#include "Scope.h"
#include "TypeInfo.h"
#include "swift/SIL/Function.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class BasicBlock;
  
namespace Lowering {
  class Condition;
  class LValue;
  class ManagedValue;
  class TypeConverter;
  class SILGenFunction;
  struct Writeback;

/// SILGenModule - an ASTVisitor for generating SIL from top-level declarations
/// in a translation unit.
class LLVM_LIBRARY_VISIBILITY SILGenModule : public ASTVisitor<SILGenModule> {
public:
  /// The Module being constructed.
  SILModule &M;
  
  /// Types - This creates and manages TypeInfo objects containing extra
  /// information about types needed for SIL generation.
  TypeConverter Types;
  
  /// TopLevelSGF - The SILGenFunction used to visit top-level code, or null if
  /// the module is not a main module.
  SILGenFunction *TopLevelSGF;
  
  /// Verbose - If true, dumps input ASTs and output SIL to errs as they are
  /// generated.
  bool Verbose;
  
public:
  SILGenModule(SILModule &M, bool Verbose);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;

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
};
  
/// SILGenType - an ASTVisitor for generating SIL from method declarations
/// inside nominal types.
class LLVM_LIBRARY_VISIBILITY SILGenType : public ASTVisitor<SILGenType> {
public:
  SILGenModule &SGM;
  
public:
  SILGenType(SILGenModule &SGM);
  ~SILGenType();

  //===--------------------------------------------------------------------===//
  // Visitors for subdeclarations
  //===--------------------------------------------------------------------===//
  void visitNominalTypeDecl(NominalTypeDecl *ntd);
  void visitFuncDecl(FuncDecl *fd);
  // FIXME: constructor, destructor, other special members
  
  // no-ops. We don't deal with the layout of types.
  void visitPatternBindingDecl(PatternBindingDecl *) {}
  void visitVarDecl(VarDecl *) {}
};

/// CaptureKind - Closure capture modes.
enum class CaptureKind {
  /// A local value captured as a mutable box.
  LValue,
  /// A local value captured by value.
  Constant,
  /// A byref argument captured by address.
  Byref,
  /// A getter-only property.
  Getter,
  /// A settable property.
  GetterSetter
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
  
/// SILGenFunction - an ASTVisitor for producing SIL from function bodies.
class LLVM_LIBRARY_VISIBILITY SILGenFunction
  : public ASTVisitor<SILGenFunction, ManagedValue, void> {
public:
  /// The SILGenModule this function belongs to.
  SILGenModule &SGM;
    
  /// The Function being constructed.
  Function &F;
  
  /// B - The SILBuilder used to construct the Function.  It is what maintains
  /// the notion of the current block being emitted into.
  SILBuilder B;
  
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
    
  bool hasVoidReturn;

public:
  SILGenFunction(SILGenModule &SGM, Function &F, bool hasVoidReturn);
  SILGenFunction(SILGenModule &SGM, Function &F, FuncExpr *FE);
  SILGenFunction(SILGenModule &SGM, Function &F, ClosureExpr *CE);
  ~SILGenFunction();

  /// emitProlog - Generates prolog code to allocate and clean up mutable
  /// storage for closure captures and local arguments.
  void emitProlog(CapturingExpr *ce, ArrayRef<Pattern*> paramPatterns);
  /// emitClosureBody - Generates code for a ClosureExpr body. This is akin
  /// to visiting the body as if wrapped in a ReturnStmt.
  void emitClosureBody(Expr *body);

  /// Return a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Cleanups.getCleanupsDepth();
  }
  
  Function &getFunction() { return F; }
  SILBuilder &getBuilder() { return B; }
    
  TypeInfo const &getTypeInfo(Type t) { return SGM.Types.getTypeInfo(t); }
  
  //===--------------------------------------------------------------------===//
  // Control flow
  //===--------------------------------------------------------------------===//
  
  /// emitCondition - Emit a boolean expression as a control-flow condition.
  ///
  /// \param TheStmt - The statement being lowered, for source information on
  ///        the branch.
  /// \param E - The expression to be evaluated as a condition.
  /// \param hasFalseCode - true if the false branch doesn't just lead
  ///        to the fallthrough.
  /// \param invertValue - true if this routine should invert the value before
  ///        testing true/false.
  Condition emitCondition(Stmt *TheStmt, Expr *E,
                          bool hasFalseCode = true, bool invertValue = false);
  
  
  /// emitBranch - Emit a branch to the given jump destination, threading out
  /// through any cleanups we might need to run.
  void emitBranch(JumpDest D);
  
  //===--------------------------------------------------------------------===//
  // Memory management
  //===--------------------------------------------------------------------===//
  
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
    
  //===--------------------------------------------------------------------===//
  // Statements
  //===--------------------------------------------------------------------===//
  
  void visitBraceStmt(BraceStmt *S);
  void visitSemiStmt(SemiStmt *S) {}
  
  void visitAssignStmt(AssignStmt *S);

  void visitReturnStmt(ReturnStmt *S);
  
  void visitIfStmt(IfStmt *S);
  
  void visitWhileStmt(WhileStmt *S);
  
  void visitDoWhileStmt(DoWhileStmt *S);
  
  void visitForStmt(ForStmt *S);
  
  void visitForEachStmt(ForEachStmt *S);
  
  void visitBreakStmt(BreakStmt *S);
  
  void visitContinueStmt(ContinueStmt *S);
  
  //===--------------------------------------------------------------------===//
  // Expressions
  //===--------------------------------------------------------------------===//
  
  /// Unreachable default case for unimplemented expr nodes.
  ManagedValue visitExpr(Expr *E);
  
  ManagedValue visitApplyExpr(ApplyExpr *E);
  ManagedValue visitDeclRefExpr(DeclRefExpr *E);
  ManagedValue visitIntegerLiteralExpr(IntegerLiteralExpr *E);
  ManagedValue visitFloatLiteralExpr(FloatLiteralExpr *E);
  ManagedValue visitCharacterLiteralExpr(CharacterLiteralExpr *E);
  ManagedValue visitStringLiteralExpr(StringLiteralExpr *E);
  ManagedValue visitLoadExpr(LoadExpr *E);
  ManagedValue visitMaterializeExpr(MaterializeExpr *E);
  ManagedValue visitRequalifyExpr(RequalifyExpr *E);
  ManagedValue visitFunctionConversionExpr(FunctionConversionExpr *E);
  ManagedValue visitCoerceExpr(CoerceExpr *E);
  ManagedValue visitDowncastExpr(DowncastExpr *E);
  ManagedValue visitParenExpr(ParenExpr *E);
  ManagedValue visitTupleExpr(TupleExpr *E);
  ManagedValue visitScalarToTupleExpr(ScalarToTupleExpr *E);
  ManagedValue visitGetMetatypeExpr(GetMetatypeExpr *E);
  ManagedValue visitSpecializeExpr(SpecializeExpr *E);
  ManagedValue visitAddressOfExpr(AddressOfExpr *E);
  ManagedValue visitMemberRefExpr(MemberRefExpr *E);
  ManagedValue visitGenericMemberRefExpr(GenericMemberRefExpr *E);
  ManagedValue visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E);
  ManagedValue visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E);
  ManagedValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E);
  ManagedValue visitModuleExpr(ModuleExpr *E);
  ManagedValue visitTupleElementExpr(TupleElementExpr *E);
  ManagedValue visitSubscriptExpr(SubscriptExpr *E);
  ManagedValue visitGenericSubscriptExpr(GenericSubscriptExpr *E);
  ManagedValue visitTupleShuffleExpr(TupleShuffleExpr *E);
  ManagedValue visitNewArrayExpr(NewArrayExpr *E);
  ManagedValue visitMetatypeExpr(MetatypeExpr *E);
  ManagedValue visitFuncExpr(FuncExpr *E);
  ManagedValue visitClosureExpr(ClosureExpr *E);
  ManagedValue visitInterpolatedStringLiteralExpr(
                                              InterpolatedStringLiteralExpr *E);
    
  void emitApplyArguments(Expr *argsExpr,
                          llvm::SmallVectorImpl<Value> &args,
                          llvm::SmallVectorImpl<Writeback> &writebacks);
  ManagedValue emitArrayInjectionCall(Value ObjectPtr,
                                      Value BasePtr,
                                      Value Length,
                                      Expr *ArrayInjectionFunction);
  ManagedValue emitTupleShuffle(Expr *E,
                                ArrayRef<Value> InOps,
                                ArrayRef<int> ElementMapping,
                                Expr *VarargsInjectionFunction);
  Value emitUnmanagedConstantRef(SILLocation loc, SILConstant constant);
  ManagedValue emitConstantRef(SILLocation loc, SILConstant constant);

  ManagedValue emitReferenceToDecl(SILLocation loc,
                                   ValueDecl *decl);

  ManagedValue emitClosureForCapturingExpr(SILLocation loc,
                                           SILConstant function,
                                           CapturingExpr *body);
  
  Materialize emitGetProperty(SILLocation loc, ManagedValue getter);
    
  ManagedValue emitManagedRValueWithCleanup(Value v);
    
  void emitAssignToLValue(SILLocation loc, Value src, LValue const &dest);
  ManagedValue emitMaterializedLoadFromLValue(SILLocation loc,
                                              LValue const &src);
  ManagedValue emitSpecializedPropertyConstantRef(Expr *expr, Expr *baseExpr,
                                            Expr /*nullable*/ *subscriptExpr,
                                            SILConstant constant,
                                            ArrayRef<Substitution> substs);

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
    
  void visitVarDecl(VarDecl *D) {
    // We handle these in pattern binding.
  }

  /// getDeclCaptureKind - Return the CaptureKind to use when capturing a decl.
  static CaptureKind getDeclCaptureKind(ValueDecl *capture) {
    if (VarDecl *var = dyn_cast<VarDecl>(capture)) {
      if (var->isProperty()) {
        return var->isSettable()
          ? CaptureKind::GetterSetter
          : CaptureKind::Getter;
      }
    }
    
    if (capture->getType()->is<LValueType>())
      return CaptureKind::Byref;
    // FIXME: Check capture analysis info here and capture Byref or
    // Constant if we can get away with it.
    if (capture->getTypeOfReference()->is<LValueType>())
      return CaptureKind::LValue;
    return CaptureKind::Constant;
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
  
  /// Dummy handler to log unimplemented nodes
  
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
