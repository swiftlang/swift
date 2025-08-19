#ifndef SWIFTC_AST_STMT_H
#define SWIFTC_AST_STMT_H

#include "swiftc/AST/ASTNode.h"
#include <memory>
#include <vector>

namespace swiftc {

class Expr;
class Decl;
class Pattern;

/// Base class for all statements.
class Stmt : public ASTNode {
protected:
  Stmt(NodeKind kind, SourceRange range) : ASTNode(kind, range) {}

public:
  static bool classof(const ASTNode* node) {
    return node->getKind() >= NodeKind::ExprStmt &&
           node->getKind() <= NodeKind::CatchStmt;
  }
};

/// Expression statement.
class ExprStmt : public Stmt {
  std::unique_ptr<Expr> Expression;

public:
  ExprStmt(SourceRange range, std::unique_ptr<Expr> expr)
      : Stmt(NodeKind::ExprStmt, range), Expression(std::move(expr)) {}

  Expr* getExpression() const { return Expression.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ExprStmt;
  }
};

/// Declaration statement.
class DeclStmt : public Stmt {
  std::unique_ptr<Decl> Declaration;

public:
  DeclStmt(SourceRange range, std::unique_ptr<Decl> decl)
      : Stmt(NodeKind::DeclStmt, range), Declaration(std::move(decl)) {}

  Decl* getDeclaration() const { return Declaration.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::DeclStmt;
  }
};

/// Return statement.
class ReturnStmt : public Stmt {
  std::unique_ptr<Expr> Value;

public:
  ReturnStmt(SourceRange range, std::unique_ptr<Expr> value = nullptr)
      : Stmt(NodeKind::ReturnStmt, range), Value(std::move(value)) {}

  Expr* getValue() const { return Value.get(); }
  bool hasValue() const { return Value != nullptr; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ReturnStmt;
  }
};

/// Break statement.
class BreakStmt : public Stmt {
public:
  BreakStmt(SourceRange range) : Stmt(NodeKind::BreakStmt, range) {}

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::BreakStmt;
  }
};

/// Continue statement.
class ContinueStmt : public Stmt {
public:
  ContinueStmt(SourceRange range) : Stmt(NodeKind::ContinueStmt, range) {}

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ContinueStmt;
  }
};

/// If statement.
class IfStmt : public Stmt {
  std::unique_ptr<Expr> Condition;
  std::unique_ptr<Stmt> ThenStmt;
  std::unique_ptr<Stmt> ElseStmt;

public:
  IfStmt(SourceRange range, std::unique_ptr<Expr> condition,
         std::unique_ptr<Stmt> thenStmt, std::unique_ptr<Stmt> elseStmt = nullptr)
      : Stmt(NodeKind::IfStmt, range),
        Condition(std::move(condition)), ThenStmt(std::move(thenStmt)),
        ElseStmt(std::move(elseStmt)) {}

  Expr* getCondition() const { return Condition.get(); }
  Stmt* getThenStmt() const { return ThenStmt.get(); }
  Stmt* getElseStmt() const { return ElseStmt.get(); }
  bool hasElse() const { return ElseStmt != nullptr; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::IfStmt;
  }
};

/// While statement.
class WhileStmt : public Stmt {
  std::unique_ptr<Expr> Condition;
  std::unique_ptr<Stmt> Body;

public:
  WhileStmt(SourceRange range, std::unique_ptr<Expr> condition,
            std::unique_ptr<Stmt> body)
      : Stmt(NodeKind::WhileStmt, range),
        Condition(std::move(condition)), Body(std::move(body)) {}

  Expr* getCondition() const { return Condition.get(); }
  Stmt* getBody() const { return Body.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::WhileStmt;
  }
};

/// Compound statement (block of statements).
class CompoundStmt : public Stmt {
  std::vector<std::unique_ptr<Stmt>> Statements;

public:
  CompoundStmt(SourceRange range, std::vector<std::unique_ptr<Stmt>> stmts)
      : Stmt(NodeKind::CompoundStmt, range), Statements(std::move(stmts)) {}

  const std::vector<std::unique_ptr<Stmt>>& getStatements() const { return Statements; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::CompoundStmt;
  }
};

/// For-in statement.
class ForStmt : public Stmt {
  std::string IteratorVar;
  std::unique_ptr<Expr> Sequence;
  std::unique_ptr<Stmt> Body;

public:
  ForStmt(SourceRange range, StringRef iterVar, std::unique_ptr<Expr> sequence, std::unique_ptr<Stmt> body)
      : Stmt(NodeKind::ForStmt, range), IteratorVar(iterVar.str()),
        Sequence(std::move(sequence)), Body(std::move(body)) {}

  StringRef getIteratorVar() const { return IteratorVar; }
  Expr* getSequence() const { return Sequence.get(); }
  Stmt* getBody() const { return Body.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ForStmt;
  }
};

} // namespace swiftc

#endif // SWIFTC_AST_STMT_H