#ifndef SWIFTC_AST_ASTNODE_H
#define SWIFTC_AST_ASTNODE_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/SourceLoc.h"
#include <memory>
#include <vector>

namespace swiftc {

// Forward declarations
class Expr;
class Stmt;
class Decl;
class Type;
class Pattern;

enum class NodeKind {
  // Expressions
  IntegerLiteralExpr,
  FloatingPointLiteralExpr,
  StringLiteralExpr,
  BooleanLiteralExpr,
  NilLiteralExpr,
  IdentifierExpr,
  BinaryOperatorExpr,
  UnaryOperatorExpr,
  CallExpr,
  MemberRefExpr,
  RangeExpr,
  SubscriptExpr,
  TupleExpr,
  ArrayExpr,
  DictionaryExpr,
  ClosureExpr,
  
  // Statements
  ExprStmt,
  DeclStmt,
  CompoundStmt,
  ReturnStmt,
  BreakStmt,
  ContinueStmt,
  IfStmt,
  WhileStmt,
  ForStmt,
  SwitchStmt,
  CaseStmt,
  GuardStmt,
  DoStmt,
  ThrowStmt,
  TryStmt,
  CatchStmt,
  
  // Declarations
  VarDecl,
  FuncDecl,
  ClassDecl,
  StructDecl,
  EnumDecl,
  ProtocolDecl,
  ExtensionDecl,
  PrecedenceGroupDecl,
  OperatorDecl,
  TypeAliasDecl,
  ImportDecl,
  
  // Patterns
  IdentifierPattern,
  TuplePattern,
  WildcardPattern,
  OptionalPattern,
  IsPattern,
  AsPattern,
  
  // Types
  IdentifierType,
  TupleType,
  FunctionType,
  ArrayType,
  DictionaryType,
  OptionalType,
  ImplicitlyUnwrappedOptionalType,
  ProtocolCompositionType,
  MetatypeType,
  GenericType,
  
  NUM_NODE_KINDS
};

/// Base class for all AST nodes.
class ASTNode {
  NodeKind Kind;
  SourceRange Range;

protected:
  ASTNode(NodeKind kind, SourceRange range) : Kind(kind), Range(range) {}

public:
  virtual ~ASTNode() = default;

  NodeKind getKind() const { return Kind; }
  SourceRange getSourceRange() const { return Range; }
  SourceLoc getStartLoc() const { return Range.getStart(); }
  SourceLoc getEndLoc() const { return Range.getEnd(); }

  template<typename T>
  bool isa() const {
    return T::classof(this);
  }

  template<typename T>
  T* dyn_cast() {
    return T::classof(this) ? static_cast<T*>(this) : nullptr;
  }

  template<typename T>
  const T* dyn_cast() const {
    return T::classof(this) ? static_cast<const T*>(this) : nullptr;
  }
};

} // namespace swiftc

#endif // SWIFTC_AST_ASTNODE_H