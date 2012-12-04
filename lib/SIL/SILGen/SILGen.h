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
    
  /// VarLocs - This is the address for the box in which an emitted
  /// variable is stored.
  /// Entries in this map are generated when a PatternBindingDecl is emitted
  /// that contains a reference and are queried for each DeclRefExpr.
  llvm::DenseMap<ValueDecl*, Value> VarLocs;
    
  bool hasVoidReturn;

public:
  SILGenFunction(SILGenModule &SGM, Function &F, bool hasVoidReturn);
  SILGenFunction(SILGenModule &SGM, Function &F, FuncExpr *FE);
  ~SILGenFunction();

  void emitProlog(FuncExpr *FE);

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

  ManagedValue emitArrayInjectionCall(Value ObjectPtr,
                                      Value BasePtr,
                                      Value Length,
                                      Expr *ArrayInjectionFunction);
  ManagedValue emitTupleShuffle(Expr *E,
                                ArrayRef<Value> InOps,
                                ArrayRef<int> ElementMapping,
                                Expr *VarargsInjectionFunction);

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
