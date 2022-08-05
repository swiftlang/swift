//===--- ConstTypeInfo.h - Const Nominal Type Info Structure ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CONST_TYPE_INFO_H
#define SWIFT_AST_CONST_TYPE_INFO_H

#include <string>
#include <vector>
#include <memory>

namespace swift {
class NominalTypeDecl;
class VarDecl;
class Type;
} // namespace swift

/// Representation of a compile-time-known value, for example
/// in a type property initializer expression
class CompileTimeValue {
public:
  enum ValueKind { RawLiteral, InitCall, Builder, Dictionary, Runtime };

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

struct FunctionParameter {
public:
  std::string getLabel() { return Label; }
  swift::Type *getType() { return Type; }

private:
  std::string Label;
  swift::Type *Type;
};

/// A representation of a call to a type's initializer
/// with a collection of (potentially compile-time-known) parameters
class InitCallValue : public CompileTimeValue {
public:
  InitCallValue(std::string Name, std::vector<FunctionParameter> Parameters)
      : CompileTimeValue(ValueKind::InitCall), Name(Name),
        Parameters(Parameters) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::InitCall;
  }

  std::string getName() const { return Name; }

private:
  std::string Name;
  std::vector<FunctionParameter> Parameters;
};

/// A representation of a Builder pattern initialization expression
class BuilderValue : public CompileTimeValue {
public:
  BuilderValue() : CompileTimeValue(ValueKind::Builder) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Builder;
  }

private:
  std::vector<CompileTimeValue> Members;
};

/// A dictionary literal value representation
class DictionaryValue : public CompileTimeValue {
public:
  DictionaryValue() : CompileTimeValue(ValueKind::Dictionary) {}

  static bool classof(const CompileTimeValue *T) {
    return T->getKind() == ValueKind::Dictionary;
  }
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

struct ConstValueTypePropertyInfo {
  swift::VarDecl *VarDecl;
  std::shared_ptr<CompileTimeValue> Value;
};

struct ConstValueTypeInfo {
  swift::NominalTypeDecl *TypeDecl;
  std::vector<ConstValueTypePropertyInfo> Properties;
};

#endif
