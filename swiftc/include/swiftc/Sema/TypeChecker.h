#ifndef SWIFTC_SEMA_TYPECHECKER_H
#define SWIFTC_SEMA_TYPECHECKER_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/AST/ASTNode.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Stmt.h"
#include "swiftc/AST/Decl.h"
#include "swiftc/AST/Type.h"
#include <memory>
#include <unordered_map>

namespace swiftc {

// ProtocolConformance is defined in Decl.h

/// Generic environment for type checking.
class GenericEnvironment {
  std::unordered_map<std::string, Type*> GenericParams;
  std::vector<GenericConstraint> Constraints;
  
public:
  void addGenericParam(StringRef name, Type* type) {
    GenericParams[name.str()] = type;
  }
  
  Type* lookupGenericParam(StringRef name) const {
    auto it = GenericParams.find(name.str());
    return it != GenericParams.end() ? it->second : nullptr;
  }
  
  void addConstraint(GenericConstraint constraint) {
    Constraints.push_back(std::move(constraint));
  }
  
  const std::vector<GenericConstraint>& getConstraints() const { return Constraints; }
  bool isEmpty() const { return GenericParams.empty(); }
};

/// Type checker for semantic analysis.
class TypeChecker {
  DiagnosticEngine& Diags;
  std::unordered_map<std::string, Type*> SymbolTable;
  std::vector<GenericEnvironment> GenericEnvironments; // Stack of generic environments
  std::vector<ProtocolConformance> ProtocolConformances; // Known protocol conformances
  std::unordered_map<std::string, ProtocolDecl*> Protocols; // Known protocols

public:
  TypeChecker(DiagnosticEngine& diags) : Diags(diags) {}

  /// Type check a list of top-level declarations.
  bool typeCheck(std::vector<std::unique_ptr<Decl>>& decls);

  /// Type check a declaration.
  bool typeCheckDecl(Decl* decl);

  /// Type check a statement.
  bool typeCheckStmt(Stmt* stmt);

  /// Type check an expression and return its type.
  Type* typeCheckExpr(Expr* expr);

private:
  /// Add a symbol to the symbol table.
  void addSymbol(StringRef name, Type* type);

  /// Look up a symbol in the symbol table.
  Type* lookupSymbol(StringRef name);

  /// Check if two types are compatible.
  bool areTypesCompatible(Type* lhs, Type* rhs);

  /// Get the built-in type for a name.
  Type* getBuiltinType(StringRef name);
  
  /// Generic type checking methods
  bool typeCheckGenericParams(GenericParamList* genericParams);
  bool typeCheckGenericConstraints(const std::vector<GenericConstraint>& constraints);
  Type* resolveGenericType(Type* type);
  bool checkGenericConformance(Type* type, Type* protocol);
  
  /// Generic environment management
  void pushGenericEnvironment();
  void popGenericEnvironment();
  GenericEnvironment& getCurrentGenericEnvironment();
  Type* lookupGenericParam(StringRef name);
  
  /// Type specialization
  Type* specializeGenericType(Type* genericType, const std::vector<Type*>& typeArgs);
  bool canSpecializeGenericType(Type* genericType, const std::vector<Type*>& typeArgs);
  
  /// Protocol conformance checking
  bool typeCheckProtocol(ProtocolDecl* protocol);
  bool checkProtocolConformance(Type* type, Type* protocol);
  void addProtocolConformance(Type* type, Type* protocol, std::vector<std::unique_ptr<Decl>> witnesses = {});
  ProtocolConformance* lookupProtocolConformance(Type* type, Type* protocol);
  std::vector<Type*> getConformedProtocols(Type* type);
  
  /// Protocol witness table generation
  bool generateWitnessTable(Type* type, Type* protocol);
  bool checkProtocolRequirements(Type* type, ProtocolDecl* protocol);
  
  /// Extension type checking
  bool typeCheckExtension(ExtensionDecl* extension);
};

} // namespace swiftc

#endif // SWIFTC_SEMA_TYPECHECKER_H