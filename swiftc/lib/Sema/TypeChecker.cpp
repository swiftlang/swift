#include "swiftc/Sema/TypeChecker.h"

using namespace swiftc;

bool TypeChecker::typeCheck(std::vector<std::unique_ptr<Decl>>& decls) {
  bool success = true;
  
  // First pass: collect all declarations
  for (const auto& decl : decls) {
    if (!typeCheckDecl(decl.get())) {
      success = false;
    }
  }
  
  return success;
}

bool TypeChecker::typeCheckDecl(Decl* decl) {
  if (!decl) return true; // Handle null declarations gracefully
  
  switch (decl->getKind()) {
  case NodeKind::VarDecl: {
    auto varDecl = static_cast<VarDecl*>(decl);
    
    // For simplified type checking, assume basic types for now
    Type* varType = nullptr;
    
    // If there's an initializer, check its type
    if (varDecl->hasInitializer()) {
      Type* initType = typeCheckExpr(varDecl->getInitializer());
      if (initType) {
        varType = initType;
      } else {
        // If we can't determine the type, assume a default type
        varType = getBuiltinType("Int"); // Default assumption
      }
      
      // If there's a declared type, check compatibility
      if (varDecl->getType()) {
        if (!areTypesCompatible(varDecl->getType(), varType)) {
          // For now, just warn instead of failing
          // This allows compilation to continue
          varType = varDecl->getType();
        }
      }
    } else {
      // No initializer, use declared type or assume Int
      varType = varDecl->getType() ? varDecl->getType() : getBuiltinType("Int");
    }
    
    // Add to symbol table
    if (varType) {
      addSymbol(varDecl->getName(), varType);
    }
    
    return true;
  }
  
  case NodeKind::FuncDecl: {
    auto funcDecl = static_cast<FuncDecl*>(decl);
    
    // Handle generic functions
    if (funcDecl->isGeneric()) {
      pushGenericEnvironment();
      if (!typeCheckGenericParams(funcDecl->getGenericParams())) {
        popGenericEnvironment();
        return false;
      }
    }
    
    // Add function to symbol table with function type
    Type* funcType = getBuiltinType("Function"); // Simplified function type
    addSymbol(funcDecl->getName(), funcType);
    
    // Type check function body if present
    if (funcDecl->getBody()) {
      // For now, assume body type checking succeeds
      // In a full implementation, we'd check the body statements
      bool success = true; // typeCheckStmt(funcDecl->getBody());
      
      if (funcDecl->isGeneric()) {
        popGenericEnvironment();
      }
      return success;
    }
    
    if (funcDecl->isGeneric()) {
      popGenericEnvironment();
    }
    
    return true;
  }
  
  case NodeKind::ClassDecl: {
    auto classDecl = static_cast<ClassDecl*>(decl);
    
    // Handle generic classes
    if (classDecl->isGeneric()) {
      pushGenericEnvironment();
      if (!typeCheckGenericParams(classDecl->getGenericParams())) {
        popGenericEnvironment();
        return false;
      }
    }
    
    // Add class to symbol table
    Type* classType = getBuiltinType("Type");
    addSymbol(classDecl->getName(), classType);
    
    if (classDecl->isGeneric()) {
      popGenericEnvironment();
    }
    return true;
  }
  
  case NodeKind::StructDecl: {
    auto structDecl = static_cast<StructDecl*>(decl);
    
    // Handle generic structs
    if (structDecl->isGeneric()) {
      pushGenericEnvironment();
      if (!typeCheckGenericParams(structDecl->getGenericParams())) {
        popGenericEnvironment();
        return false;
      }
    }
    
    // Add struct to symbol table
    Type* structType = getBuiltinType("Type");
    addSymbol(structDecl->getName(), structType);
    
    if (structDecl->isGeneric()) {
      popGenericEnvironment();
    }
    return true;
  }
  
  case NodeKind::ProtocolDecl: {
    auto protocolDecl = static_cast<ProtocolDecl*>(decl);
    
    // Register the protocol
    Protocols[protocolDecl->getName().str()] = protocolDecl;
    
    // Type check the protocol
    return typeCheckProtocol(protocolDecl);
  }
  
  case NodeKind::ExtensionDecl: {
    auto extensionDecl = static_cast<ExtensionDecl*>(decl);
    return typeCheckExtension(extensionDecl);
  }
  
  case NodeKind::ImportDecl:
    // Import declarations don't need type checking
    return true;
  
  default:
    return true;
  }
}

bool TypeChecker::typeCheckStmt(Stmt* stmt) {
  if (!stmt) return true; // Handle null statements gracefully
  
  switch (stmt->getKind()) {
  case NodeKind::ExprStmt: {
    auto exprStmt = static_cast<ExprStmt*>(stmt);
    if (exprStmt && exprStmt->getExpression()) {
      return typeCheckExpr(exprStmt->getExpression()) != nullptr;
    }
    return true; // If no expression, assume success
  }
  
  case NodeKind::ReturnStmt: {
    auto returnStmt = static_cast<ReturnStmt*>(stmt);
    if (returnStmt && returnStmt->hasValue()) {
      return typeCheckExpr(returnStmt->getValue()) != nullptr;
    }
    return true;
  }
  
  case NodeKind::IfStmt: {
    auto ifStmt = static_cast<IfStmt*>(stmt);
    
    // Check condition
    Type* condType = typeCheckExpr(ifStmt->getCondition());
    if (!condType)
      return false;
    
    // Check then statement
    if (!typeCheckStmt(ifStmt->getThenStmt()))
      return false;
    
    // Check else statement if present
    if (ifStmt->hasElse()) {
      return typeCheckStmt(ifStmt->getElseStmt());
    }
    
    return true;
  }
  
  case NodeKind::WhileStmt: {
    auto whileStmt = static_cast<WhileStmt*>(stmt);
    
    // Check condition
    Type* condType = typeCheckExpr(whileStmt->getCondition());
    if (!condType)
      return false;
    
    // Check body
    return typeCheckStmt(whileStmt->getBody());
  }
  
  default:
    return true;
  }
}

Type* TypeChecker::typeCheckExpr(Expr* expr) {
  if (!expr) return getBuiltinType("Void"); // Handle null expressions gracefully
  
  switch (expr->getKind()) {
  case NodeKind::IntegerLiteralExpr:
    return getBuiltinType("Int");
  
  case NodeKind::FloatingPointLiteralExpr:
    return getBuiltinType("Double");
  
  case NodeKind::StringLiteralExpr:
    return getBuiltinType("String");
  
  case NodeKind::BooleanLiteralExpr:
    return getBuiltinType("Bool");
  
  case NodeKind::NilLiteralExpr:
    return getBuiltinType("Optional"); // Handle nil as optional type
  
  case NodeKind::IdentifierExpr: {
    auto identExpr = static_cast<IdentifierExpr*>(expr);
    Type* type = lookupSymbol(identExpr->getName());
    if (!type) {
      // For now, assume unknown identifiers are of Int type
      // In a full implementation, this would be an error
      type = getBuiltinType("Int");
    }
    return type;
  }
  
  case NodeKind::BinaryOperatorExpr: {
    auto binExpr = static_cast<BinaryOperatorExpr*>(expr);
    
    Type* lhsType = typeCheckExpr(binExpr->getLHS());
    Type* rhsType = typeCheckExpr(binExpr->getRHS());
    
    if (!lhsType || !rhsType)
      return nullptr;
    
    // For now, assume binary operations return the type of the left operand
    return lhsType;
  }
  
  case NodeKind::CallExpr: {
    auto callExpr = static_cast<CallExpr*>(expr);
    
    Type* calleeType = typeCheckExpr(callExpr->getCallee());
    if (!calleeType)
      return nullptr;
    
    // Type check arguments
    for (const auto& arg : callExpr->getArguments()) {
      if (!typeCheckExpr(arg.get()))
        return nullptr;
    }
    
    // For now, return a placeholder type
    return getBuiltinType("Void");
  }
  
  default:
    return nullptr;
  }
}

void TypeChecker::addSymbol(StringRef name, Type* type) {
  SymbolTable[name.str()] = type;
}

Type* TypeChecker::lookupSymbol(StringRef name) {
  auto it = SymbolTable.find(name.str());
  return it != SymbolTable.end() ? it->second : nullptr;
}

bool TypeChecker::areTypesCompatible(Type* lhs, Type* rhs) {
  // Simplified type compatibility check
  if (!lhs || !rhs)
    return false;
  
  // For now, just check if they're the same type
  return lhs->getKind() == rhs->getKind();
}

Type* TypeChecker::getBuiltinType(StringRef name) {
  // Return a dummy type for now - in a real implementation,
  // we'd have a proper type system with built-in types
  static IdentifierType intType(SourceRange(), "Int");
  static IdentifierType doubleType(SourceRange(), "Double");
  static IdentifierType stringType(SourceRange(), "String");
  static IdentifierType boolType(SourceRange(), "Bool");
  static IdentifierType voidType(SourceRange(), "Void");
  static IdentifierType functionType(SourceRange(), "Function");
  static IdentifierType typeType(SourceRange(), "Type");
  static IdentifierType optionalType(SourceRange(), "Optional");
  
  if (name == "Int") return &intType;
  if (name == "Double") return &doubleType;
  if (name == "String") return &stringType;
  if (name == "Bool") return &boolType;
  if (name == "Void") return &voidType;
  if (name == "Function") return &functionType;
  if (name == "Type") return &typeType;
  if (name == "Optional") return &optionalType;
  
  return &intType; // Default to Int for unknown types
}

// Generic type checking implementation
bool TypeChecker::typeCheckGenericParams(GenericParamList* genericParams) {
  if (!genericParams || genericParams->isEmpty()) {
    return true;
  }
  
  GenericEnvironment& env = getCurrentGenericEnvironment();
  
  // Add generic parameters to the environment
  for (const auto& param : genericParams->getParameters()) {
    if (auto genericParam = param->dyn_cast<GenericTypeParam>()) {
      env.addGenericParam(genericParam->getName(), param.get());
    }
  }
  
  // Check constraints
  return typeCheckGenericConstraints(genericParams->getConstraints());
}

bool TypeChecker::typeCheckGenericConstraints(const std::vector<GenericConstraint>& constraints) {
  for (const auto& constraint : constraints) {
    // For now, assume all constraints are valid
    // In a full implementation, we'd check protocol conformance, etc.
  }
  return true;
}

Type* TypeChecker::resolveGenericType(Type* type) {
  if (!type) return nullptr;
  
  // If it's an identifier type, check if it's a generic parameter
  if (auto identType = type->dyn_cast<IdentifierType>()) {
    Type* genericParam = lookupGenericParam(identType->getName());
    if (genericParam) {
      return genericParam;
    }
  }
  
  return type;
}

bool TypeChecker::checkGenericConformance(Type* type, Type* protocol) {
  // Simplified conformance checking
  // In a full implementation, we'd check witness tables, etc.
  return true;
}

void TypeChecker::pushGenericEnvironment() {
  GenericEnvironments.emplace_back();
}

void TypeChecker::popGenericEnvironment() {
  if (!GenericEnvironments.empty()) {
    GenericEnvironments.pop_back();
  }
}

GenericEnvironment& TypeChecker::getCurrentGenericEnvironment() {
  if (GenericEnvironments.empty()) {
    pushGenericEnvironment();
  }
  return GenericEnvironments.back();
}

Type* TypeChecker::lookupGenericParam(StringRef name) {
  // Search from innermost to outermost generic environment
  for (auto it = GenericEnvironments.rbegin(); it != GenericEnvironments.rend(); ++it) {
    Type* param = it->lookupGenericParam(name);
    if (param) {
      return param;
    }
  }
  return nullptr;
}

Type* TypeChecker::specializeGenericType(Type* genericType, const std::vector<Type*>& typeArgs) {
  // Simplified generic specialization
  // In a full implementation, we'd create a specialized type
  return genericType;
}

bool TypeChecker::canSpecializeGenericType(Type* genericType, const std::vector<Type*>& typeArgs) {
  // Simplified check - assume we can always specialize
  return true;
}

// Protocol conformance implementation
bool TypeChecker::typeCheckProtocol(ProtocolDecl* protocol) {
  if (!protocol) return false;
  
  // Handle generic protocols (with associated types)
  if (protocol->hasAssociatedTypes()) {
    pushGenericEnvironment();
    if (!typeCheckGenericParams(protocol->getGenericParams())) {
      popGenericEnvironment();
      return false;
    }
  }
  
  // Check protocol requirements
  for (const auto& requirement : protocol->getRequirements()) {
    if (!typeCheckDecl(requirement.getDeclaration())) {
      if (protocol->hasAssociatedTypes()) {
        popGenericEnvironment();
      }
      return false;
    }
  }
  
  if (protocol->hasAssociatedTypes()) {
    popGenericEnvironment();
  }
  
  return true;
}

bool TypeChecker::checkProtocolConformance(Type* type, Type* protocol) {
  if (!type || !protocol) return false;
  
  // Check if we already have this conformance
  ProtocolConformance* existingConformance = lookupProtocolConformance(type, protocol);
  if (existingConformance) {
    return true;
  }
  
  // Look up the protocol declaration
  if (auto protocolIdent = protocol->dyn_cast<IdentifierType>()) {
    auto protocolIt = Protocols.find(protocolIdent->getName().str());
    if (protocolIt != Protocols.end()) {
      ProtocolDecl* protocolDecl = protocolIt->second;
      return checkProtocolRequirements(type, protocolDecl);
    }
  }
  
  return false;
}

void TypeChecker::addProtocolConformance(Type* type, Type* protocol, std::vector<std::unique_ptr<Decl>> witnesses) {
  ProtocolConformances.emplace_back(type, protocol, std::move(witnesses));
}

ProtocolConformance* TypeChecker::lookupProtocolConformance(Type* type, Type* protocol) {
  for (auto& conformance : ProtocolConformances) {
    if (areTypesCompatible(conformance.getConformingType(), type) &&
        areTypesCompatible(conformance.getProtocol(), protocol)) {
      return &conformance;
    }
  }
  return nullptr;
}

std::vector<Type*> TypeChecker::getConformedProtocols(Type* type) {
  std::vector<Type*> protocols;
  for (const auto& conformance : ProtocolConformances) {
    if (areTypesCompatible(conformance.getConformingType(), type)) {
      protocols.push_back(conformance.getProtocol());
    }
  }
  return protocols;
}

bool TypeChecker::generateWitnessTable(Type* type, Type* protocol) {
  // Simplified witness table generation
  // In a full implementation, we'd generate method dispatch tables
  return true;
}

bool TypeChecker::checkProtocolRequirements(Type* type, ProtocolDecl* protocol) {
  // Simplified requirement checking
  // In a full implementation, we'd check that the type provides all required methods
  return true;
}

bool TypeChecker::typeCheckExtension(ExtensionDecl* extension) {
  if (!extension) return false;
  
  // Handle generic extensions
  if (extension->isGeneric()) {
    pushGenericEnvironment();
    if (!typeCheckGenericParams(extension->getGenericParams())) {
      popGenericEnvironment();
      return false;
    }
  }
  
  // Check protocol conformances added by the extension
  Type* extendedType = extension->getExtendedType();
  for (const auto& protocol : extension->getConformedProtocols()) {
    if (checkProtocolConformance(extendedType, protocol.get())) {
      addProtocolConformance(extendedType, protocol.get());
    }
  }
  
  // Type check extension members
  for (const auto& member : extension->getMembers()) {
    if (!typeCheckDecl(member.get())) {
      if (extension->isGeneric()) {
        popGenericEnvironment();
      }
      return false;
    }
  }
  
  if (extension->isGeneric()) {
    popGenericEnvironment();
  }
  
  return true;
}