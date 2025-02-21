//===--- ConstTypeInfo.h - Const Nominal Type Info Structure ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CONST_TYPE_INFO_H
#define SWIFT_AST_CONST_TYPE_INFO_H

#include "swift/AST/Attr.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include <memory>
#include <string>
#include <vector>

namespace swift {
class NominalTypeDecl;
class VarDecl;
class Type;

/// Representation of a compile-time-known value, for example
/// in a type property initializer expression
class CompileTimeValue {
public:
  enum ValueKind {
    RawLiteral,
    InitCall,
    Builder,
    Dictionary,
    Array,
    Tuple,
    Enum,
    Type,
    KeyPath,
    FunctionCall,
    StaticFunctionCall,
    MemberReference,
    InterpolatedString,
    NilLiteral,
    Runtime
  };

  ValueKind getKind() const { return Kind; }

protected:
  CompileTimeValue(ValueKind ValueKind) : Kind(ValueKind) {}

private:
  ValueKind Kind;
};

/// A string representation of a raw literal value,
/// for example an integer or string or float literal.
class RawLiteralValue : public CompileTimeValue {
public:
  RawLiteralValue(std::string Value)
  : CompileTimeValue(ValueKind::RawLiteral), Value(Value) {}

  std::string getValue() const { return Value; }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::RawLiteral;
  }

private:
  std::string Value;
};

/// A representation of an Optional<Wrapped> value declared as nil
/// or left undeclared.
///
/// Nil values were previously represented as RawLiteralValue with
/// value "nil". This caused ambiguous values when extracting values,
/// such as an Optional<String> of value "nil".

class NilLiteralValue : public CompileTimeValue {
public:
  NilLiteralValue() : CompileTimeValue(ValueKind::NilLiteral) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::NilLiteral;
  }
};

struct FunctionParameter {
  std::string Label;
  swift::Type Type;
  std::shared_ptr<CompileTimeValue> Value;
};

/// A representation of a call to a type's initializer
/// with a collection of (potentially compile-time-known) parameters
class InitCallValue : public CompileTimeValue {
public:
  InitCallValue(swift::Type Type, std::vector<FunctionParameter> Parameters)
      : CompileTimeValue(ValueKind::InitCall), Type(Type),
        Parameters(Parameters) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::InitCall;
  }

  swift::Type getType() const { return Type; }
  std::vector<FunctionParameter> getParameters() const { return Parameters; }

private:
  swift::Type Type;
  std::vector<FunctionParameter> Parameters;
};

/// A representation of a Builder pattern initialization expression. For
/// example:
///
/// @FooBuilder
/// public static var foos: [Foo] {
///    Foo(name: "foos.1")
///    Foo(name: "foos.2")
/// }
///
/// In this example, the result builder type is FooBuilder
/// The members are Foo(name: "foos.1") and Foo(name: "foos.2")
///
class BuilderValue : public CompileTimeValue {
public:
  enum MemberKind {
    Expression,
    Either,
    Optional,
    LimitedAvailability,
    Array,
    Unknown
  };

  /// A base class for individual members being declared inside the result
  /// builder
  class BuilderMember {
  public:
    MemberKind getKind() const { return Kind; }

  protected:
    BuilderMember(MemberKind MemberKind) : Kind(MemberKind) {}

  private:
    MemberKind Kind;
  };

  /// A basic expression that is defined inside the result builder. For example:
  ///  {
  ///     Foo(name: "1")
  ///  }
  ///
  class SingleMember : public BuilderMember {
  public:
    SingleMember(std::shared_ptr<CompileTimeValue> Element)
        : BuilderMember(MemberKind::Expression), Element(Element) {}

    static bool classof(const BuilderMember *T) {
      return T->getKind() == MemberKind::Expression;
    }

    std::shared_ptr<CompileTimeValue> getElement() const { return Element; }

  private:
    std::shared_ptr<CompileTimeValue> Element;
  };

  /// A member that represents when the individual values are defined by
  /// iterating over an array. For example:
  ///     for i in 1...3 {
  ///         Foo(name: "MyFooProviderInferred.foos.Array.\(i)")
  ///     }
  ///
  class ArrayMember : public BuilderMember {
  public:
    ArrayMember(std::vector<std::shared_ptr<BuilderMember>> Elements)
        : BuilderMember(MemberKind::Array), Elements(Elements) {}

    static bool classof(const BuilderMember *T) {
      return T->getKind() == MemberKind::Array;
    }

    std::vector<std::shared_ptr<BuilderMember>> getElements() const {
      return Elements;
    }

  private:
    std::vector<std::shared_ptr<BuilderMember>> Elements;
  };

  /// A member that is defined conditionally. It can be of the following types:
  ///
  /// 1. A regular if-else condition
  ///      if condition {
  ///         Foo(name: "1")
  ///      } else {
  ///         Foo(name: "2")
  ///      }
  ///
  /// 2. An optional
  ///     if condition {
  ///         Foo(name: "1")
  ///     }
  ///
  /// 3. Limited availability
  ///     if #available(macOS 99, *) {
  ///         Foo(name: "1")
  ///         Foo(name: "2")
  ///     }
  ///
  class ConditionalMember : public BuilderMember {
  public:
    class AvailabilitySpec {
    private:
      AvailabilityDomain Domain;
      llvm::VersionTuple Version;

    public:
      AvailabilitySpec(AvailabilityDomain Domain, llvm::VersionTuple Version)
          : Domain(Domain), Version(Version) {}

      AvailabilityDomain getDomain() const { return Domain; }
      llvm::VersionTuple getVersion() const { return Version; }
    };

    ConditionalMember(MemberKind MemberKind,
                      std::vector<AvailabilitySpec> AvailabilitySpecs,
                      std::vector<std::shared_ptr<BuilderMember>> IfElements,
                      std::vector<std::shared_ptr<BuilderMember>> ElseElements)
        : BuilderMember(MemberKind), AvailabilitySpecs(AvailabilitySpecs),
          IfElements(IfElements), ElseElements(ElseElements) {}

    ConditionalMember(MemberKind MemberKind,
                      std::vector<std::shared_ptr<BuilderMember>> IfElements,
                      std::vector<std::shared_ptr<BuilderMember>> ElseElements)
        : BuilderMember(MemberKind), IfElements(IfElements),
          ElseElements(ElseElements) {}

    static bool classof(const BuilderMember *T) {
      auto Kind = T->getKind();
      return (Kind == MemberKind::Either) ||
             (Kind == MemberKind::LimitedAvailability) ||
             (Kind == MemberKind::Optional);
    }

    std::optional<std::vector<AvailabilitySpec>> getAvailabilitySpecs() const {
      return AvailabilitySpecs;
    }
    std::vector<std::shared_ptr<BuilderMember>> getIfElements() const {
      return IfElements;
    }
    std::vector<std::shared_ptr<BuilderMember>> getElseElements() const {
      return ElseElements;
    }

  private:
    std::optional<std::vector<AvailabilitySpec>> AvailabilitySpecs;
    std::vector<std::shared_ptr<BuilderMember>> IfElements;
    std::vector<std::shared_ptr<BuilderMember>> ElseElements;
  };

  BuilderValue(std::vector<std::shared_ptr<BuilderMember>> Members)
      : CompileTimeValue(ValueKind::Builder), ResultBuilderType(std::nullopt),
        Members(Members) {}

  BuilderValue(CustomAttr *ResultBuilderType,
               std::vector<std::shared_ptr<BuilderMember>> Members)
      : CompileTimeValue(ValueKind::Builder),
        ResultBuilderType(ResultBuilderType), Members(Members) {}

  std::optional<CustomAttr *> getResultBuilderType() const {
    return ResultBuilderType;
  }
  std::vector<std::shared_ptr<BuilderMember>> getMembers() const {
    return Members;
  }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Builder;
  }

private:
  std::optional<CustomAttr *> ResultBuilderType;
  std::vector<std::shared_ptr<BuilderMember>> Members;
};

struct TupleElement {
  std::optional<std::string> Label;
  swift::Type Type;
  std::shared_ptr<CompileTimeValue> Value;
};

/// A representation of a tuple and each tuple-element
class TupleValue : public CompileTimeValue {
public:
  TupleValue(std::vector<TupleElement> Elements)
      : CompileTimeValue(ValueKind::Tuple), Elements(Elements) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Tuple;
  }

  std::vector<TupleElement> getElements() const { return Elements; }

private:
  std::vector<TupleElement> Elements;
};

/// An array literal value representation
class ArrayValue : public CompileTimeValue {
public:
  ArrayValue(std::vector<std::shared_ptr<CompileTimeValue>> Elements)
      : CompileTimeValue(ValueKind::Array), Elements(Elements) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Array;
  }
  std::vector<std::shared_ptr<CompileTimeValue>> getElements() const {
    return Elements;
  }

private:
  std::vector<std::shared_ptr<CompileTimeValue>> Elements;
};

/// A dictionary literal value representation
class DictionaryValue : public CompileTimeValue {
public:
  DictionaryValue(std::vector<std::shared_ptr<TupleValue>> elements)
      : CompileTimeValue(ValueKind::Dictionary), Elements(elements) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Dictionary;
  }

  std::vector<std::shared_ptr<TupleValue>> getElements() const {
    return Elements;
  }

private:
  std::vector<std::shared_ptr<TupleValue>> Elements;
};

/// An enum value representation
class EnumValue : public CompileTimeValue {
public:
  EnumValue(std::string Identifier,
            std::optional<std::vector<FunctionParameter>> Parameters)
      : CompileTimeValue(ValueKind::Enum), Identifier(Identifier),
        Parameters(Parameters) {}

  std::string getIdentifier() const { return Identifier; }
  std::optional<std::vector<FunctionParameter>> getParameters() const {
    return Parameters;
  }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Enum;
  }

private:
  std::string Identifier;
  std::optional<std::vector<FunctionParameter>> Parameters;
};

/// A type value representation
class TypeValue : public CompileTimeValue {
public:
  TypeValue(swift::Type Type) : CompileTimeValue(ValueKind::Type), Type(Type) {}

  swift::Type getType() const { return Type; }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Type;
  }

private:
  swift::Type Type;
};

/// A representation of a Keypath
class KeyPathValue : public CompileTimeValue {
public:
  struct Component {
    std::string Label;
    swift::Type Type;
  };
  KeyPathValue(std::string Path,
               swift::Type RootType,
               std::vector<Component> Components)
  : CompileTimeValue(ValueKind::KeyPath), Path(Path), RootType(RootType), Components(Components) {}

  std::string getPath() const { return Path; }
  swift::Type getRootType() const { return RootType; }
  std::vector<Component> getComponents() const {
    return Components;
  }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::KeyPath;
  }

private:
  std::string Path;
  swift::Type RootType;
  std::vector<Component> Components;
};

/// A function call representation. This is for a function declaration such as
/// let foo = bar(baz: "abc")
class FunctionCallValue : public CompileTimeValue {
public:
  FunctionCallValue(std::string Identifier,
                    std::optional<std::vector<FunctionParameter>> Parameters)
      : CompileTimeValue(ValueKind::FunctionCall), Identifier(Identifier),
        Parameters(Parameters) {}

  std::string getIdentifier() const { return Identifier; }
  std::optional<std::vector<FunctionParameter>> getParameters() const {
    return Parameters;
  }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::FunctionCall;
  }

private:
  std::string Identifier;
  std::optional<std::vector<FunctionParameter>> Parameters;
};

/// A static function reference representation such as
/// let foo = MyStruct.bar(item: "")
/// let foo = MyStruct.bar()
class StaticFunctionCallValue : public CompileTimeValue {
public:
  StaticFunctionCallValue(std::string Label, swift::Type Type,
                          std::vector<FunctionParameter> Parameters)
      : CompileTimeValue(ValueKind::StaticFunctionCall), Label(Label),
        Type(Type), Parameters(Parameters) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::StaticFunctionCall;
  }

  std::string getLabel() const { return Label; }
  swift::Type getType() const { return Type; }
  std::vector<FunctionParameter> getParameters() const { return Parameters; }

private:
  std::string Label;
  swift::Type Type;
  std::vector<FunctionParameter> Parameters;
};

/// A member reference representation such as
/// let foo = MyStruct.bar
class MemberReferenceValue : public CompileTimeValue {
public:
  MemberReferenceValue(swift::Type BaseType, std::string MemberLabel)
      : CompileTimeValue(ValueKind::MemberReference), BaseType(BaseType),
        MemberLabel(MemberLabel) {}

  std::string getMemberLabel() const { return MemberLabel; }
  swift::Type getBaseType() const { return BaseType; }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::MemberReference;
  }

private:
  swift::Type BaseType;
  std::string MemberLabel;
};

/// A representation of an Interpolated String Literal
class InterpolatedStringLiteralValue : public CompileTimeValue {
public:
  InterpolatedStringLiteralValue(
      std::vector<std::shared_ptr<CompileTimeValue>> Segments)
      : CompileTimeValue(ValueKind::InterpolatedString), Segments(Segments) {}

  std::vector<std::shared_ptr<CompileTimeValue>> getSegments() const {
    return Segments;
  }

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::InterpolatedString;
  }

private:
  std::vector<std::shared_ptr<CompileTimeValue>> Segments;
};

/// A representation of an arbitrary value that does not fall under
/// any of the above categories.
class RuntimeValue : public CompileTimeValue {
public:
  RuntimeValue() : CompileTimeValue(ValueKind::Runtime) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Runtime;
  }
};

struct CustomAttrValue {
  const swift::CustomAttr *Attr;
  std::vector<FunctionParameter> Parameters;
};

/// A representation of a single associated value for an enumeration case.
struct EnumElementParameterValue {
  std::optional<std::string> Label;
  swift::Type Type;
};

/// A representation of a single enumeration case.
struct EnumElementDeclValue {
  std::string Name;
  std::optional<std::string> RawValue;
  std::optional<std::vector<EnumElementParameterValue>> Parameters;
};

using AttrValueVector = llvm::SmallVector<CustomAttrValue, 2>;
struct ConstValueTypePropertyInfo {
  swift::VarDecl *VarDecl;
  std::shared_ptr<CompileTimeValue> Value;
  std::optional<AttrValueVector> PropertyWrappers;

  ConstValueTypePropertyInfo(swift::VarDecl *VarDecl,
                             std::shared_ptr<CompileTimeValue> Value,
                             std::optional<AttrValueVector> PropertyWrappers)
      : VarDecl(VarDecl), Value(Value), PropertyWrappers(PropertyWrappers) {}

  ConstValueTypePropertyInfo(swift::VarDecl *VarDecl,
                             std::shared_ptr<CompileTimeValue> Value)
      : VarDecl(VarDecl), Value(Value),
        PropertyWrappers(std::optional<AttrValueVector>()) {}
};

struct ConstValueTypeInfo {
  swift::NominalTypeDecl *TypeDecl;
  std::vector<ConstValueTypePropertyInfo> Properties;
  std::optional<std::vector<EnumElementDeclValue>> EnumElements;
};
} // namespace swift
#endif
