#ifndef SWIFTC_AST_EXPR_H
#define SWIFTC_AST_EXPR_H

#include "swiftc/AST/ASTNode.h"
#include "swiftc/Basic/LLVM.h"
#include <memory>
#include <vector>

namespace swiftc {

class Type;

/// Base class for all expressions.
class Expr : public ASTNode {
protected:
  Expr(NodeKind kind, SourceRange range) : ASTNode(kind, range) {}

public:
  static bool classof(const ASTNode* node) {
    return node->getKind() >= NodeKind::IntegerLiteralExpr &&
           node->getKind() <= NodeKind::ClosureExpr;
  }
};

/// Integer literal expression.
class IntegerLiteralExpr : public Expr {
  int64_t Value;

public:
  IntegerLiteralExpr(SourceRange range, int64_t value)
      : Expr(NodeKind::IntegerLiteralExpr, range), Value(value) {}

  int64_t getValue() const { return Value; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::IntegerLiteralExpr;
  }
};

/// Floating point literal expression.
class FloatingPointLiteralExpr : public Expr {
  double Value;

public:
  FloatingPointLiteralExpr(SourceRange range, double value)
      : Expr(NodeKind::FloatingPointLiteralExpr, range), Value(value) {}

  double getValue() const { return Value; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::FloatingPointLiteralExpr;
  }
};

/// String literal expression.
class StringLiteralExpr : public Expr {
  std::string Value;

public:
  StringLiteralExpr(SourceRange range, StringRef value)
      : Expr(NodeKind::StringLiteralExpr, range), Value(value.str()) {}

  StringRef getValue() const { return Value; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::StringLiteralExpr;
  }
};

/// Boolean literal expression.
class BooleanLiteralExpr : public Expr {
  bool Value;

public:
  BooleanLiteralExpr(SourceRange range, bool value)
      : Expr(NodeKind::BooleanLiteralExpr, range), Value(value) {}

  bool getValue() const { return Value; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::BooleanLiteralExpr;
  }
};

/// Nil literal expression.
class NilLiteralExpr : public Expr {
public:
  NilLiteralExpr(SourceRange range)
      : Expr(NodeKind::NilLiteralExpr, range) {}

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::NilLiteralExpr;
  }
};

/// Identifier expression.
class IdentifierExpr : public Expr {
  std::string Name;

public:
  IdentifierExpr(SourceRange range, StringRef name)
      : Expr(NodeKind::IdentifierExpr, range), Name(name.str()) {}

  StringRef getName() const { return Name; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::IdentifierExpr;
  }
};

/// Binary operator expression.
class BinaryOperatorExpr : public Expr {
  std::unique_ptr<Expr> LHS;
  std::unique_ptr<Expr> RHS;
  std::string OperatorName;

public:
  BinaryOperatorExpr(SourceRange range, std::unique_ptr<Expr> lhs,
                     StringRef op, std::unique_ptr<Expr> rhs)
      : Expr(NodeKind::BinaryOperatorExpr, range),
        LHS(std::move(lhs)), RHS(std::move(rhs)), OperatorName(op.str()) {}

  Expr* getLHS() const { return LHS.get(); }
  Expr* getRHS() const { return RHS.get(); }
  StringRef getOperatorName() const { return OperatorName; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::BinaryOperatorExpr;
  }
};

/// Unary operator expression.
class UnaryOperatorExpr : public Expr {
  std::unique_ptr<Expr> Operand;
  std::string OperatorName;
  bool IsPrefix;

public:
  UnaryOperatorExpr(SourceRange range, StringRef op, std::unique_ptr<Expr> operand,
                    bool isPrefix)
      : Expr(NodeKind::UnaryOperatorExpr, range),
        Operand(std::move(operand)), OperatorName(op.str()), IsPrefix(isPrefix) {}

  Expr* getOperand() const { return Operand.get(); }
  StringRef getOperatorName() const { return OperatorName; }
  bool isPrefix() const { return IsPrefix; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::UnaryOperatorExpr;
  }
};

/// Function call expression.
class CallExpr : public Expr {
  std::unique_ptr<Expr> Callee;
  std::vector<std::unique_ptr<Expr>> Arguments;

public:
  CallExpr(SourceRange range, std::unique_ptr<Expr> callee,
           std::vector<std::unique_ptr<Expr>> args)
      : Expr(NodeKind::CallExpr, range),
        Callee(std::move(callee)), Arguments(std::move(args)) {}

  Expr* getCallee() const { return Callee.get(); }
  const std::vector<std::unique_ptr<Expr>>& getArguments() const { return Arguments; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::CallExpr;
  }
};

/// Member reference expression (e.g., obj.member).
class MemberRefExpr : public Expr {
  std::unique_ptr<Expr> Base;
  std::string MemberName;

public:
  MemberRefExpr(SourceRange range, std::unique_ptr<Expr> base, StringRef member)
      : Expr(NodeKind::MemberRefExpr, range),
        Base(std::move(base)), MemberName(member.str()) {}

  Expr* getBase() const { return Base.get(); }
  StringRef getMemberName() const { return MemberName; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::MemberRefExpr;
  }
};

/// Range expression (e.g., 1..<10, 1...10).
class RangeExpr : public Expr {
  std::unique_ptr<Expr> Start;
  std::unique_ptr<Expr> End;
  bool IsInclusive;

public:
  RangeExpr(SourceRange range, std::unique_ptr<Expr> start, std::unique_ptr<Expr> end, bool inclusive)
      : Expr(NodeKind::RangeExpr, range),
        Start(std::move(start)), End(std::move(end)), IsInclusive(inclusive) {}

  Expr* getStart() const { return Start.get(); }
  Expr* getEnd() const { return End.get(); }
  bool isInclusive() const { return IsInclusive; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::RangeExpr;
  }
};

/// Array literal expression (e.g., [1, 2, 3]).
class ArrayLiteralExpr : public Expr {
  std::vector<std::unique_ptr<Expr>> Elements;

public:
  ArrayLiteralExpr(SourceRange range, std::vector<std::unique_ptr<Expr>> elements)
      : Expr(NodeKind::ArrayExpr, range), Elements(std::move(elements)) {}

  const std::vector<std::unique_ptr<Expr>>& getElements() const { return Elements; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ArrayExpr;
  }
};

/// Dictionary literal expression (e.g., ["key": value]).
class DictionaryLiteralExpr : public Expr {
public:
  struct KeyValuePair {
    std::unique_ptr<Expr> Key;
    std::unique_ptr<Expr> Value;
    
    KeyValuePair(std::unique_ptr<Expr> key, std::unique_ptr<Expr> value)
        : Key(std::move(key)), Value(std::move(value)) {}
  };
  
private:
  std::vector<KeyValuePair> Elements;

public:
  DictionaryLiteralExpr(SourceRange range, std::vector<KeyValuePair> elements)
      : Expr(NodeKind::DictionaryExpr, range), Elements(std::move(elements)) {}

  const std::vector<KeyValuePair>& getElements() const { return Elements; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::DictionaryExpr;
  }
};

/// Closure expression (e.g., { $0 + 1 }, { x in x * 2 }).
class ClosureExpr : public Expr {
  std::vector<std::string> Parameters;
  std::unique_ptr<Expr> Body;
  bool HasExplicitParameters;

public:
  ClosureExpr(SourceRange range, std::vector<std::string> params, 
              std::unique_ptr<Expr> body, bool hasExplicitParams = false)
      : Expr(NodeKind::ClosureExpr, range), 
        Parameters(std::move(params)), Body(std::move(body)),
        HasExplicitParameters(hasExplicitParams) {}

  const std::vector<std::string>& getParameters() const { return Parameters; }
  Expr* getBody() const { return Body.get(); }
  bool hasExplicitParameters() const { return HasExplicitParameters; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ClosureExpr;
  }
};

/// Subscript expression (e.g., array[index]).
class SubscriptExpr : public Expr {
  std::unique_ptr<Expr> Base;
  std::vector<std::unique_ptr<Expr>> Indices;

public:
  SubscriptExpr(SourceRange range, std::unique_ptr<Expr> base,
                std::vector<std::unique_ptr<Expr>> indices)
      : Expr(NodeKind::SubscriptExpr, range),
        Base(std::move(base)), Indices(std::move(indices)) {}

  Expr* getBase() const { return Base.get(); }
  const std::vector<std::unique_ptr<Expr>>& getIndices() const { return Indices; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::SubscriptExpr;
  }
};

/// Tuple expression (e.g., (a, b, c)).
class TupleExpr : public Expr {
  std::vector<std::unique_ptr<Expr>> Elements;

public:
  TupleExpr(SourceRange range, std::vector<std::unique_ptr<Expr>> elements)
      : Expr(NodeKind::TupleExpr, range), Elements(std::move(elements)) {}

  const std::vector<std::unique_ptr<Expr>>& getElements() const { return Elements; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::TupleExpr;
  }
};

} // namespace swiftc

#endif // SWIFTC_AST_EXPR_H