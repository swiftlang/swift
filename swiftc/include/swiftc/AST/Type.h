#ifndef SWIFTC_AST_TYPE_H
#define SWIFTC_AST_TYPE_H

#include "swiftc/AST/ASTNode.h"
#include "swiftc/Basic/LLVM.h"
#include <memory>
#include <vector>

namespace swiftc {

/// Base class for all types.
class Type : public ASTNode {
protected:
  Type(NodeKind kind, SourceRange range) : ASTNode(kind, range) {}

public:
  static bool classof(const ASTNode* node) {
    return node->getKind() >= NodeKind::IdentifierType &&
           node->getKind() <= NodeKind::GenericType;
  }
};

/// Identifier type (e.g., Int, String, MyClass).
class IdentifierType : public Type {
  std::string Name;

public:
  IdentifierType(SourceRange range, StringRef name)
      : Type(NodeKind::IdentifierType, range), Name(name.str()) {}

  StringRef getName() const { return Name; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::IdentifierType;
  }
};

/// Tuple type (e.g., (Int, String)).
class TupleType : public Type {
  std::vector<std::unique_ptr<Type>> Elements;

public:
  TupleType(SourceRange range, std::vector<std::unique_ptr<Type>> elements)
      : Type(NodeKind::TupleType, range), Elements(std::move(elements)) {}

  const std::vector<std::unique_ptr<Type>>& getElements() const { return Elements; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::TupleType;
  }
};

/// Function type (e.g., (Int, String) -> Bool).
class FunctionType : public Type {
  std::unique_ptr<Type> ParameterType;
  std::unique_ptr<Type> ReturnType;

public:
  FunctionType(SourceRange range, std::unique_ptr<Type> paramType,
               std::unique_ptr<Type> returnType)
      : Type(NodeKind::FunctionType, range),
        ParameterType(std::move(paramType)), ReturnType(std::move(returnType)) {}

  Type* getParameterType() const { return ParameterType.get(); }
  Type* getReturnType() const { return ReturnType.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::FunctionType;
  }
};

/// Array type (e.g., [Int]).
class ArrayType : public Type {
  std::unique_ptr<Type> ElementType;

public:
  ArrayType(SourceRange range, std::unique_ptr<Type> elementType)
      : Type(NodeKind::ArrayType, range), ElementType(std::move(elementType)) {}

  Type* getElementType() const { return ElementType.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ArrayType;
  }
};

/// Dictionary type (e.g., [String: Int]).
class DictionaryType : public Type {
  std::unique_ptr<Type> KeyType;
  std::unique_ptr<Type> ValueType;

public:
  DictionaryType(SourceRange range, std::unique_ptr<Type> keyType,
                 std::unique_ptr<Type> valueType)
      : Type(NodeKind::DictionaryType, range),
        KeyType(std::move(keyType)), ValueType(std::move(valueType)) {}

  Type* getKeyType() const { return KeyType.get(); }
  Type* getValueType() const { return ValueType.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::DictionaryType;
  }
};

/// Optional type (e.g., Int?).
class OptionalType : public Type {
  std::unique_ptr<Type> WrappedType;

public:
  OptionalType(SourceRange range, std::unique_ptr<Type> wrappedType)
      : Type(NodeKind::OptionalType, range), WrappedType(std::move(wrappedType)) {}

  Type* getWrappedType() const { return WrappedType.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::OptionalType;
  }
};

/// Implicitly unwrapped optional type (e.g., Int!).
class ImplicitlyUnwrappedOptionalType : public Type {
  std::unique_ptr<Type> WrappedType;

public:
  ImplicitlyUnwrappedOptionalType(SourceRange range, std::unique_ptr<Type> wrappedType)
      : Type(NodeKind::ImplicitlyUnwrappedOptionalType, range),
        WrappedType(std::move(wrappedType)) {}

  Type* getWrappedType() const { return WrappedType.get(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ImplicitlyUnwrappedOptionalType;
  }
};

/// Generic type parameter (e.g., T in Array<T>).
class GenericTypeParam : public Type {
  std::string Name;
  int Depth;  // Nesting depth for generic parameters
  int Index;  // Index within the generic parameter list

public:
  GenericTypeParam(SourceRange range, StringRef name, int depth = 0, int index = 0)
      : Type(NodeKind::GenericType, range), Name(name.str()), Depth(depth), Index(index) {}

  StringRef getName() const { return Name; }
  int getDepth() const { return Depth; }
  int getIndex() const { return Index; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::GenericType;
  }
};

/// Bound generic type (e.g., Array<Int>, Dictionary<String, Int>).
class BoundGenericType : public Type {
  std::unique_ptr<Type> GenericType;
  std::vector<std::unique_ptr<Type>> TypeArguments;

public:
  BoundGenericType(SourceRange range, std::unique_ptr<Type> genericType,
                   std::vector<std::unique_ptr<Type>> typeArguments)
      : Type(NodeKind::GenericType, range),
        GenericType(std::move(genericType)), TypeArguments(std::move(typeArguments)) {}

  Type* getGenericType() const { return GenericType.get(); }
  const std::vector<std::unique_ptr<Type>>& getTypeArguments() const { return TypeArguments; }
  size_t getNumTypeArguments() const { return TypeArguments.size(); }
  Type* getTypeArgument(size_t index) const {
    return index < TypeArguments.size() ? TypeArguments[index].get() : nullptr;
  }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::GenericType;
  }
};

/// Protocol composition type (e.g., Protocol1 & Protocol2).
class ProtocolCompositionType : public Type {
  std::vector<std::unique_ptr<Type>> Protocols;

public:
  ProtocolCompositionType(SourceRange range, std::vector<std::unique_ptr<Type>> protocols)
      : Type(NodeKind::ProtocolCompositionType, range), Protocols(std::move(protocols)) {}

  const std::vector<std::unique_ptr<Type>>& getProtocols() const { return Protocols; }
  size_t getNumProtocols() const { return Protocols.size(); }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::ProtocolCompositionType;
  }
};

/// Metatype (e.g., Int.Type).
class MetatypeType : public Type {
  std::unique_ptr<Type> InstanceType;
  bool IsProtocol; // true for Protocol.Type, false for concrete types

public:
  MetatypeType(SourceRange range, std::unique_ptr<Type> instanceType, bool isProtocol = false)
      : Type(NodeKind::MetatypeType, range), InstanceType(std::move(instanceType)), IsProtocol(isProtocol) {}

  Type* getInstanceType() const { return InstanceType.get(); }
  bool isProtocolMetatype() const { return IsProtocol; }

  static bool classof(const ASTNode* node) {
    return node->getKind() == NodeKind::MetatypeType;
  }
};

} // namespace swiftc

#endif // SWIFTC_AST_TYPE_H