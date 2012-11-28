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
#include "swift/SIL/Function.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class BasicBlock;
  
namespace Lowering {
  class Condition;

class LLVM_LIBRARY_VISIBILITY SILGen
  : public ASTVisitor<SILGen, Value, void> {
public:
  /// The Function being constructed.
  Function &F;
  
  /// B - The SILBuilder used to construct the Function.  It is what maintains
  /// the notion of the current block being emitted into.
  SILBuilder B;
  
  std::vector<JumpDest> BreakDestStack;
  std::vector<JumpDest> ContinueDestStack;

  /// Cleanups - This records information about the currently active cleanups.
  CleanupManager Cleanups;

  /// VarLocs - This is the address for the box in which an emitted
  /// variable is stored.
  /// Entries in this map are generated when a PatternBindingDecl is emitted
  /// that contains a reference and are queried for each DeclRefExpr.
  llvm::DenseMap<ValueDecl*, Value> VarLocs;
    
  bool hasVoidReturn;

public:
  SILGen(Function &F, FuncExpr *FE);
  ~SILGen();

  void emitProlog(FuncExpr *FE);

  /// Return a stable reference to the current cleanup.
  CleanupsDepth getCleanupsDepth() const {
    return Cleanups.getCleanupsDepth();
  }
  
  Function &getFunction() { return F; }
  SILBuilder &getBuilder() { return B; }
  
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
  
  /// emitRetainArgument - Emits the instructions necessary for a caller to
  /// retain a value in order to pass it as a function argument.
  /// - For trivial loadable types, this is a no-op.
  /// - For reference types, 'v' is retained.
  /// - For loadable types with reference type members, the reference type
  ///   members are all retained.
  /// - For address-only types, this is a no-op. Address-only arguments are
  ///   callee-copied.
  void emitRetainArgument(SILLocation loc, Value v);
    
  /// emitCopy - Emits the instructions necessary to store a copy of a value
  /// to an address.
  /// - For trivial loadable types, a 'store v to dest' is generated.
  /// - For reference types, 'v' is retained before being stored.
  /// - For loadable types with reference type members, the reference type
  ///   members are retained before being stored.
  /// - For address-only types, this generates a copy_addr instruction.
  /// The operation may be either an initialization, which stores the value to
  /// uninitialized memory, or an assignment, which replaces an already existing
  /// value stored at the destination address.
  ///
  /// \param loc - The location information to assign to the generated
  ///   instructions.
  /// \param v - The value to copy. It should be either a value of a loadable
  ///   type or the address of a value of an address-only type.
  /// \param dest - The value of the address to store the copy to. It should be
  ///   an address type matching the type of v.
  /// \param isAssignment - If true, indicates that there is already a value
  ///   stored at the destination address. The existing value will be loaded
  ///   and released if necessary.
  void emitCopy(SILLocation loc, Value v, Value dest, bool isAssignment);

  /// emitDestroyArgument - Emits the instructions necessary for a callee to
  /// destroy a value passed to it as an argument.
  /// - For trivial loadable types, this is a no-op.
  /// - For reference types, 'v' is released.
  /// - For loadable types with reference type members, the reference type
  ///   members are all released.
  /// - For address-only types, the value at the address is destroyed using
  ///   a destroy_addr instruction.
  void emitDestroyArgument(SILLocation loc, Value v);
    
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
  
  Value visitExpr(Expr *E) {
    E->dump();
    llvm_unreachable("Not yet implemented");
  }
  
  Value visitApplyExpr(ApplyExpr *E);
  Value visitDeclRefExpr(DeclRefExpr *E);
  Value visitIntegerLiteralExpr(IntegerLiteralExpr *E);
  Value visitFloatLiteralExpr(FloatLiteralExpr *E);
  Value visitCharacterLiteralExpr(CharacterLiteralExpr *E);
  Value visitStringLiteralExpr(StringLiteralExpr *E);
  Value visitLoadExpr(LoadExpr *E);
  Value visitMaterializeExpr(MaterializeExpr *E);
  Value visitRequalifyExpr(RequalifyExpr *E);
  Value visitFunctionConversionExpr(FunctionConversionExpr *E);
  Value visitParenExpr(ParenExpr *E);
  Value visitTupleExpr(TupleExpr *E);
  Value visitScalarToTupleExpr(ScalarToTupleExpr *E);
  Value visitGetMetatypeExpr(GetMetatypeExpr *E);
  Value visitSpecializeExpr(SpecializeExpr *E);
  Value visitAddressOfExpr(AddressOfExpr *E);
  Value visitTupleElementExpr(TupleElementExpr *E);
  Value visitTupleShuffleExpr(TupleShuffleExpr *E);
  Value visitNewArrayExpr(NewArrayExpr *E);
  Value visitMetatypeExpr(MetatypeExpr *E);

  Value emitArrayInjectionCall(Value ObjectPtr, Value BasePtr,
                               Value Length, Expr *ArrayInjectionFunction);
  Value emitTupleShuffle(Expr *E, ArrayRef<Value> InOps,
                         ArrayRef<int> ElementMapping,
                         Expr *VarargsInjectionFunction);

  /// emitApply - Creates an apply instruction and retain instructions for any
  /// arguments that require retaining.
  Value emitApply(SILLocation loc, Value fn, ArrayRef<Value> args);

  //===--------------------------------------------------------------------===//
  // Declarations
  //===--------------------------------------------------------------------===//
  
  void visitDecl(Decl *D) {
    D->dump();
    llvm_unreachable("Not yet implemented");
  }

  // ClassDecl - emitClassDecl
  // FuncDecl - emitLocalFunction
  // OneOfDecl - emitOneOfDecl
  void visitPatternBindingDecl(PatternBindingDecl *D);
  // StructDecl - emitStructDecl
    
  void visitTypeAliasDecl(TypeAliasDecl *D) {
    // No lowering support needed.
  }
    
  void visitVarDecl(VarDecl *D) {
    // We handle these in pattern binding.
  }
};
  
} // end namespace Lowering
} // end namespace swift

#endif
