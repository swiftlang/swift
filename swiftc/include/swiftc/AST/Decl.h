#ifndef SWIFTC_AST_DECL_H
#define SWIFTC_AST_DECL_H

#include "swiftc/AST/ASTNode.h"
#include <memory>
#include <vector>

namespace swiftc {

class Type;
class Expr;
class Stmt;
class Pattern;

/// Generic parameter constraint (e.g., T: Equatable, U: Collection).
struct GenericConstraint {
  std::unique_ptr<Type> Subject;    // The type being constrained (e.g., T)
  std::unique_ptr<Type> Requirement; // The constraint (e.g., Equatable)
  
  enum Kind {
    Conformance,    // T: Protocol
    SameType,       // T == U
    Superclass      // T: Class
  } ConstraintKind;
  
  GenericConstraint(std::unique_ptr<Type> subject, std::unique_ptr<Type> requirement, Kind kind)
      : Subject(std::move(subject)), Requirement(std::move(requirement)), ConstraintKind(kind) {}
};

/// Generic parameter list for generic declarations.
class GenericParamList {
  std::vector<std::unique_ptr<Type>> Parameters;  // Generic type parameters
  std::vector<GenericConstraint> Constraints;     // Where clause constraints
  
public:
  GenericParamList(std::vector<std::unique_ptr<Type>> params, 
                   std::vector<GenericConstraint> constraints = {})
      : Parameters(std::move(params)), Constraints(std::move(constraints)) {}
  
  const std::vector<std::unique_ptr<Type>>& getParameters() const { return Parameters; }
  const std::vector<GenericConstraint>& getConstraints() const { return Constraints; }
  size_t getNumParameters() const { return Parameters.size(); }
  bool hasConstraints() const { return !Constraints.empty(); }
  bool isEmpty() const { return Parameters.empty(); }
};

/// Base class for all declarations.
class Decl : public ASTNode {
protected:
  Decl(NodeKind kind, SourceRange range) : ASTNode(kind, range) {}

public:
  static bool classof(const ASTNode* node) {
    return node->getKind() >= NodeKind::VarDecl &&
           node->getKind() <= NodeKind::ImportDecl;
  }
};

/// Variable declaration.
class VarDecl : public Decl {
  std::string Name;
  std::unique_ptr<Type> DeclaredType;
  std::unique_ptr<Expr> Initializer;
  bool IsLet;

public:
  VarDecl(SourceRange range, StringRef name, std::unique_ptr<Type> type,
          std::unique_ptr<Expr> init, bool isLet)
      : Decl(NodeKind::VarDecl, range), Name(name.str()),
        DeclaredType(std::move(type)), Initializer(std::move(init)), IsLet(isLet) {}

  StringRef getName() const { return Name; }
  Type* getType() const { return DeclaredType.get(); }
  Expr* getInitializer() const { return Initializer.get(); }
  bool isLet() const { return IsLet; }
  bool hasInitializer() const { return Initializer != nullptr; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::VarDecl;
  }
};

/// Type alias declaration (typealias/associatedtype).
class TypeAliasDecl : public Decl {
  std::string Name;
  std::unique_ptr<Type> UnderlyingType;
  bool IsAssociatedType;

public:
  TypeAliasDecl(SourceRange range, StringRef name, std::unique_ptr<Type> underlyingType = nullptr,
                bool isAssociatedType = false)
      : Decl(NodeKind::TypeAliasDecl, range), Name(name.str()),
        UnderlyingType(std::move(underlyingType)), IsAssociatedType(isAssociatedType) {}

  StringRef getName() const { return Name; }
  Type* getUnderlyingType() const { return UnderlyingType.get(); }
  bool isAssociatedType() const { return IsAssociatedType; }
  bool hasUnderlyingType() const { return UnderlyingType != nullptr; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::TypeAliasDecl;
  }
};

/// Function parameter.
class ParamDecl {
  std::string ExternalName;
  std::string InternalName;
  std::unique_ptr<Type> ParamType;
  std::unique_ptr<Expr> DefaultValue;

public:
  ParamDecl(StringRef externalName, StringRef internalName,
            std::unique_ptr<Type> type, std::unique_ptr<Expr> defaultValue = nullptr)
      : ExternalName(externalName.str()), InternalName(internalName.str()),
        ParamType(std::move(type)), DefaultValue(std::move(defaultValue)) {}

  StringRef getExternalName() const { return ExternalName; }
  StringRef getInternalName() const { return InternalName; }
  Type* getType() const { return ParamType.get(); }
  Expr* getDefaultValue() const { return DefaultValue.get(); }
  bool hasDefaultValue() const { return DefaultValue != nullptr; }
};

/// Function declaration.
class FuncDecl : public Decl {
  std::string Name;
  std::vector<std::unique_ptr<ParamDecl>> Parameters;
  std::unique_ptr<Type> ReturnType;
  std::unique_ptr<Stmt> Body;
  std::unique_ptr<GenericParamList> GenericParams; // Generic parameters
  bool IsThrows;
  bool IsRethrows;

public:
  FuncDecl(SourceRange range, StringRef name,
           std::vector<std::unique_ptr<ParamDecl>> params,
           std::unique_ptr<Type> returnType, std::unique_ptr<Stmt> body,
           std::unique_ptr<GenericParamList> genericParams = nullptr,
           bool isThrows = false, bool isRethrows = false)
      : Decl(NodeKind::FuncDecl, range), Name(name.str()),
        Parameters(std::move(params)), ReturnType(std::move(returnType)),
        Body(std::move(body)), GenericParams(std::move(genericParams)),
        IsThrows(isThrows), IsRethrows(isRethrows) {}

  StringRef getName() const { return Name; }
  const std::vector<std::unique_ptr<ParamDecl>>& getParameters() const { return Parameters; }
  Type* getReturnType() const { return ReturnType.get(); }
  GenericParamList* getGenericParams() const { return GenericParams.get(); }
  bool isGeneric() const { return GenericParams && !GenericParams->isEmpty(); }
  Stmt* getBody() const { return Body.get(); }
  bool isThrows() const { return IsThrows; }
  bool isRethrows() const { return IsRethrows; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::FuncDecl;
  }
};

/// Class declaration.
class ClassDecl : public Decl {
  std::string Name;
  std::vector<std::unique_ptr<Decl>> Members;
  std::unique_ptr<GenericParamList> GenericParams; // Generic parameters
  std::vector<std::unique_ptr<Type>> InheritedTypes; // Superclass and protocols

public:
  ClassDecl(SourceRange range, StringRef name,
            std::vector<std::unique_ptr<Decl>> members,
            std::unique_ptr<GenericParamList> genericParams = nullptr,
            std::vector<std::unique_ptr<Type>> inheritedTypes = {})
      : Decl(NodeKind::ClassDecl, range), Name(name.str()),
        Members(std::move(members)), GenericParams(std::move(genericParams)),
        InheritedTypes(std::move(inheritedTypes)) {}

  StringRef getName() const { return Name; }
  const std::vector<std::unique_ptr<Decl>>& getMembers() const { return Members; }
  GenericParamList* getGenericParams() const { return GenericParams.get(); }
  const std::vector<std::unique_ptr<Type>>& getInheritedTypes() const { return InheritedTypes; }
  bool isGeneric() const { return GenericParams && !GenericParams->isEmpty(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ClassDecl;
  }
};

/// Struct declaration.
class StructDecl : public Decl {
  std::string Name;
  std::vector<std::unique_ptr<Decl>> Members;
  std::unique_ptr<GenericParamList> GenericParams; // Generic parameters
  std::vector<std::unique_ptr<Type>> ConformedProtocols; // Protocol conformances

public:
  StructDecl(SourceRange range, StringRef name,
             std::vector<std::unique_ptr<Decl>> members,
             std::unique_ptr<GenericParamList> genericParams = nullptr,
             std::vector<std::unique_ptr<Type>> conformedProtocols = {})
      : Decl(NodeKind::StructDecl, range), Name(name.str()),
        Members(std::move(members)), GenericParams(std::move(genericParams)),
        ConformedProtocols(std::move(conformedProtocols)) {}

  StringRef getName() const { return Name; }
  const std::vector<std::unique_ptr<Decl>>& getMembers() const { return Members; }
  GenericParamList* getGenericParams() const { return GenericParams.get(); }
  const std::vector<std::unique_ptr<Type>>& getConformedProtocols() const { return ConformedProtocols; }
  bool isGeneric() const { return GenericParams && !GenericParams->isEmpty(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::StructDecl;
  }
};

/// Enum case declaration.
class EnumCaseDecl : public Decl {
  std::string Name;
  std::vector<std::unique_ptr<Type>> AssociatedTypes; // Associated values
  std::unique_ptr<Expr> RawValue; // Raw value (for Int, String enums)

public:
  EnumCaseDecl(SourceRange range, StringRef name, 
               std::vector<std::unique_ptr<Type>> associatedTypes = {},
               std::unique_ptr<Expr> rawValue = nullptr)
      : Decl(NodeKind::VarDecl, range), Name(name.str()), // Use VarDecl for now
        AssociatedTypes(std::move(associatedTypes)), RawValue(std::move(rawValue)) {}

  StringRef getName() const { return Name; }
  const std::vector<std::unique_ptr<Type>>& getAssociatedTypes() const { return AssociatedTypes; }
  Expr* getRawValue() const { return RawValue.get(); }
  bool hasAssociatedTypes() const { return !AssociatedTypes.empty(); }
  bool hasRawValue() const { return RawValue != nullptr; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::VarDecl; // Use VarDecl for now
  }
};

/// Enum declaration.
class EnumDecl : public Decl {
  std::string Name;
  std::vector<std::unique_ptr<EnumCaseDecl>> Cases;
  std::unique_ptr<Type> RawType; // Raw value type (Int, String, etc.)
  std::vector<std::unique_ptr<Decl>> Members; // Methods, computed properties, etc.
  std::unique_ptr<GenericParamList> GenericParams;

public:
  EnumDecl(SourceRange range, StringRef name,
           std::vector<std::unique_ptr<EnumCaseDecl>> cases = {},
           std::unique_ptr<Type> rawType = nullptr,
           std::vector<std::unique_ptr<Decl>> members = {},
           std::unique_ptr<GenericParamList> genericParams = nullptr)
      : Decl(NodeKind::EnumDecl, range), Name(name.str()),
        Cases(std::move(cases)), RawType(std::move(rawType)),
        Members(std::move(members)), GenericParams(std::move(genericParams)) {}

  StringRef getName() const { return Name; }
  const std::vector<std::unique_ptr<EnumCaseDecl>>& getCases() const { return Cases; }
  Type* getRawType() const { return RawType.get(); }
  const std::vector<std::unique_ptr<Decl>>& getMembers() const { return Members; }
  GenericParamList* getGenericParams() const { return GenericParams.get(); }
  bool hasRawType() const { return RawType != nullptr; }
  bool isGeneric() const { return GenericParams && !GenericParams->isEmpty(); }

  void addCase(std::unique_ptr<EnumCaseDecl> enumCase) {
    Cases.push_back(std::move(enumCase));
  }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::EnumDecl;
  }
};

/// Import declaration.
class ImportDecl : public Decl {
  std::string ModuleName;

public:
  ImportDecl(SourceRange range, StringRef moduleName)
      : Decl(NodeKind::ImportDecl, range), ModuleName(moduleName.str()) {}

  StringRef getModuleName() const { return ModuleName; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ImportDecl;
  }
};

/// Protocol requirement (method, property, associatedtype, etc.).
class ProtocolRequirement {
public:
  enum Kind {
    Method,         // func requirement
    Property,       // var requirement
    AssociatedType, // associatedtype requirement
    Initializer     // init requirement
  };
  
private:
  Kind RequirementKind;
  std::unique_ptr<Decl> Declaration;
  
public:
  ProtocolRequirement(Kind kind, std::unique_ptr<Decl> decl)
      : RequirementKind(kind), Declaration(std::move(decl)) {}
  
  Kind getKind() const { return RequirementKind; }
  Decl* getDeclaration() const { return Declaration.get(); }
};

/// Protocol declaration.
class ProtocolDecl : public Decl {
  std::string Name;
  std::vector<std::unique_ptr<Type>> InheritedProtocols; // Protocol inheritance
  std::vector<ProtocolRequirement> Requirements;         // Protocol requirements
  std::unique_ptr<GenericParamList> GenericParams;       // Associated types as generics

public:
  ProtocolDecl(SourceRange range, StringRef name,
               std::vector<std::unique_ptr<Type>> inheritedProtocols = {},
               std::vector<ProtocolRequirement> requirements = {},
               std::unique_ptr<GenericParamList> genericParams = nullptr)
      : Decl(NodeKind::ProtocolDecl, range), Name(name.str()),
        InheritedProtocols(std::move(inheritedProtocols)),
        Requirements(std::move(requirements)),
        GenericParams(std::move(genericParams)) {}

  StringRef getName() const { return Name; }
  const std::vector<std::unique_ptr<Type>>& getInheritedProtocols() const { return InheritedProtocols; }
  const std::vector<ProtocolRequirement>& getRequirements() const { return Requirements; }
  GenericParamList* getGenericParams() const { return GenericParams.get(); }
  bool hasAssociatedTypes() const { return GenericParams && !GenericParams->isEmpty(); }

  void addRequirement(ProtocolRequirement requirement) {
    Requirements.push_back(std::move(requirement));
  }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ProtocolDecl;
  }
};

/// Protocol conformance information.
class ProtocolConformance {
  Type* ConformingType;
  Type* Protocol;
  std::vector<std::unique_ptr<Decl>> WitnessTable; // Method implementations
  
public:
  ProtocolConformance(Type* conformingType, Type* protocol,
                      std::vector<std::unique_ptr<Decl>> witnessTable = {})
      : ConformingType(conformingType), Protocol(protocol),
        WitnessTable(std::move(witnessTable)) {}
  
  Type* getConformingType() const { return ConformingType; }
  Type* getProtocol() const { return Protocol; }
  const std::vector<std::unique_ptr<Decl>>& getWitnessTable() const { return WitnessTable; }
  
  void addWitness(std::unique_ptr<Decl> witness) {
    WitnessTable.push_back(std::move(witness));
  }
};

/// Extension declaration (can add protocol conformances).
class ExtensionDecl : public Decl {
  std::unique_ptr<Type> ExtendedType;
  std::vector<std::unique_ptr<Type>> ConformedProtocols;
  std::vector<std::unique_ptr<Decl>> Members;
  std::unique_ptr<GenericParamList> GenericParams;

public:
  ExtensionDecl(SourceRange range, std::unique_ptr<Type> extendedType,
                std::vector<std::unique_ptr<Type>> conformedProtocols = {},
                std::vector<std::unique_ptr<Decl>> members = {},
                std::unique_ptr<GenericParamList> genericParams = nullptr)
      : Decl(NodeKind::ExtensionDecl, range), ExtendedType(std::move(extendedType)),
        ConformedProtocols(std::move(conformedProtocols)), Members(std::move(members)),
        GenericParams(std::move(genericParams)) {}

  Type* getExtendedType() const { return ExtendedType.get(); }
  const std::vector<std::unique_ptr<Type>>& getConformedProtocols() const { return ConformedProtocols; }
  const std::vector<std::unique_ptr<Decl>>& getMembers() const { return Members; }
  GenericParamList* getGenericParams() const { return GenericParams.get(); }
  bool isGeneric() const { return GenericParams && !GenericParams->isEmpty(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ExtensionDecl;
  }
};

/// Precedence group declaration.
class PrecedenceGroupDecl : public Decl {
  std::string Name;
  std::string HigherThan;
  std::string LowerThan;

public:
  PrecedenceGroupDecl(SourceRange range, StringRef name, StringRef higherThan = "", StringRef lowerThan = "")
      : Decl(NodeKind::PrecedenceGroupDecl, range), Name(name.str()),
        HigherThan(higherThan.str()), LowerThan(lowerThan.str()) {}

  StringRef getName() const { return Name; }
  StringRef getHigherThan() const { return HigherThan; }
  StringRef getLowerThan() const { return LowerThan; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::PrecedenceGroupDecl;
  }
};

/// Operator declaration.
class OperatorDecl : public Decl {
public:
  enum OperatorKind { Infix, Prefix, Postfix };

private:
  std::string Name;
  std::string PrecedenceGroup;
  OperatorKind Kind;

public:
  OperatorDecl(SourceRange range, StringRef name, OperatorKind kind, StringRef precedenceGroup = "")
      : Decl(NodeKind::OperatorDecl, range), Name(name.str()),
        PrecedenceGroup(precedenceGroup.str()), Kind(kind) {}

  StringRef getName() const { return Name; }
  StringRef getPrecedenceGroup() const { return PrecedenceGroup; }
  OperatorKind getOperatorKind() const { return Kind; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::OperatorDecl;
  }
};

} // namespace swiftc

#endif // SWIFTC_AST_DECL_H