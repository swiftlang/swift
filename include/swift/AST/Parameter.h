//===--- Parameter.h - Functions & closures parameters ----------*- C++ -*-===//
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
//
// This file defines the Parameter class, the ParameterList class and support
// logic.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PARAMETER_H
#define SWIFT_AST_PARAMETER_H

#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/TypeLoc.h"

namespace swift {
  class ParamDecl;
  class ExprHandle;
  
/// This describes a single parameter, including such feats as:
///
///   a b : Int           //< Differing internal vs external name.
///   inout a : Int       //< inout parameter.
///   @autoclosure a : T  //< Parameter attribute.
///   a : Int = 42        //< Default value.
///   a : Int...          //< Varargs parameter.
///
/// Parameters are stored in parameter lists, and multiple levels of currying
/// are represented with multiple parameter lists.
///
struct Parameter {
  /// The decl keeps track of the internal and external parameter name, as well
  /// as the parameter attributes.
  ParamDecl *decl;

  /// This is the location of the ":" token.
  SourceLoc colonLoc;
  
  /// This is the type specified, including location information.
  TypeLoc type;
  
  // TODO: ExprHandle can probably go away now, we should be able to just have
  // an Expr* here, along with a "is checked" bit.

  /// The default value, if any, along with whether this is varargs.
  llvm::PointerIntPair<ExprHandle *, 1, bool> defaultValueAndIsVarargs;

  ExprHandle *getDefaultValue() const {
    return defaultValueAndIsVarargs.getPointer();
  }
  /// Whether or not this parameter is varargs.
  bool isVarargs() const { return defaultValueAndIsVarargs.getInt(); }
  
  /// Information about a symbolic default argument, like __FILE__.
  DefaultArgumentKind defaultArgumentKind;
};


/// This describes a list of parameters.  Each parameter descriptor is tail
/// allocated onto this list.
class alignas(alignof(Parameter)) ParameterList {
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8);

  size_t numParameters;

  ParameterList(size_t numParameters) : numParameters(numParameters) {}
  void operator=(const ParameterList&) = delete;
public:
  static ParameterList *create(ASTContext &Context, ArrayRef<Parameter> Params);
  
  size_t getNumParameters() const {
    return numParameters;
  }
  
  MutableArrayRef<Parameter> getParameters() {
    auto Ptr = reinterpret_cast<Parameter*>(this + 1);
    return { Ptr, numParameters };
  }
  ArrayRef<Parameter> getParameters() const {
    auto Ptr = reinterpret_cast<const Parameter*>(this + 1);
    return { Ptr, numParameters };
  }
  
  const Parameter &getParameter(unsigned i) const {
    return getParameters()[i];
  }

  Parameter &getParameter(unsigned i) {
    return getParameters()[i];
  }
};

} // end namespace swift.

#endif
