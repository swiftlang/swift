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
  class ManagedValue;
  class TypeConverter;
  class SILGenFunction;

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
  
public:
  SILGenModule(SILModule &M);
  ~SILGenModule();
  
  SILGenModule(SILGenModule const &) = delete;
  void operator=(SILGenModule const &) = delete;

  //===--------------------------------------------------------------------===//
  // Visitors for top-level forms
  //===--------------------------------------------------------------------===//
  // FIXME: visit types, etc.
  void visitFuncDecl(FuncDecl *fd);
  void visitPatternBindingDecl(PatternBindingDecl *vd);
  void visitTopLevelCodeDecl(TopLevelCodeDecl *td);

  /// emitFunction - Generates code for the given FuncExpr and adds the
  /// Function to the current SILModule under the name SILConstant(decl). For
  /// curried functions, curried entry point Functions are also generated and
  /// added to the current SILModule.
  Function *emitFunction(SILConstant decl, FuncExpr *fe);
  /// emitClosure - Generates code for the given ClosureExpr and adds the
  /// Function to the current SILModule under the name SILConstant(ce).
  Function *emitClosure(ClosureExpr *ce);
};

/// CaptureKind - Closure capture modes.
enum class CaptureKind {
  /// A local value captured as a mutable box.
  LValue,
  /// A local value captured by value.
  Constant,
  /// A byref argument captured by address.
  Byref
};
  
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
  /// declaration, such as a func, is emitted. This map is queried to produce
  /// the constant value for a DeclRefExpr to a local constant.
  llvm::DenseMap<ValueDecl*, Value> LocalConstants;
    
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
  
  /// emitAssign - Emits the instructions necessary to reassign a value
  /// to an address. 'v' should be at +1 retain count.
  /// - For trivial loadable types, a 'store v to dest' is generated.
  /// - For reference types, the old value at 'dest' is loaded,
  ///   'v' is stored to 'dest', and then the old value is released.
  /// - For loadable types with reference type members, the reference type
  ///   members of the old value are released following the same sequence as for
  ///   reference types.
  /// - For address-only types, this generates a copy_addr assign instruction.
  void emitAssign(SILLocation loc, Value v, Value dest);
  
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
  ManagedValue visitParenExpr(ParenExpr *E);
  ManagedValue visitTupleExpr(TupleExpr *E);
  ManagedValue visitScalarToTupleExpr(ScalarToTupleExpr *E);
  ManagedValue visitGetMetatypeExpr(GetMetatypeExpr *E);
  ManagedValue visitSpecializeExpr(SpecializeExpr *E);
  ManagedValue visitAddressOfExpr(AddressOfExpr *E);
  ManagedValue visitTupleElementExpr(TupleElementExpr *E);
  ManagedValue visitTupleShuffleExpr(TupleShuffleExpr *E);
  ManagedValue visitNewArrayExpr(NewArrayExpr *E);
  ManagedValue visitMetatypeExpr(MetatypeExpr *E);
  ManagedValue visitFuncExpr(FuncExpr *E);
  ManagedValue visitClosureExpr(ClosureExpr *E);

  ManagedValue emitArrayInjectionCall(Value ObjectPtr,
                                      Value BasePtr,
                                      Value Length,
                                      Expr *ArrayInjectionFunction);
  ManagedValue emitTupleShuffle(Expr *E,
                                ArrayRef<Value> InOps,
                                ArrayRef<int> ElementMapping,
                                Expr *VarargsInjectionFunction);
  ManagedValue emitReferenceToDecl(SILLocation loc,
                                   ValueDecl *decl);

  ManagedValue emitClosureForCapturingExpr(SILLocation loc,
                                           SILConstant function,
                                           CapturingExpr *body);

  //===--------------------------------------------------------------------===//
  // Declarations
  //===--------------------------------------------------------------------===//
  
  void visitDecl(Decl *D) {
    D->dump();
    llvm_unreachable("Not yet implemented");
  }

  // ClassDecl - emitClassDecl
  // OneOfDecl - emitOneOfDecl
  // StructDecl - emitStructDecl
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
    if (capture->getType()->is<LValueType>())
      return CaptureKind::Byref;
    // FIXME: Check capture analysis info here and capture Byref or
    // Constant if we can get away with it.
    if (capture->getTypeOfReference()->is<LValueType>())
      return CaptureKind::LValue;
    return CaptureKind::Constant;
  }
};
  
} // end namespace Lowering
} // end namespace swift

#endif
