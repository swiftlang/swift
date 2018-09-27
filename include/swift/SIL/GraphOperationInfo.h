//===--- GraphOperationInfo.h - GraphOperationInst Parse Logic --*- C++ -*-===//
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
//
// This file defines the parsing logic for a GraphOperationInst, in particular
// decoding the mangled inst name string for the arguments and attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GRAPH_OPERATION_INFO_H
#define SWIFT_SIL_GRAPH_OPERATION_INFO_H

#include "swift/AST/Identifier.h"
#include "swift/AST/TensorFlow.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class GraphOperationInst;

namespace tf {
/// Holds information about a TensorFlow operation as represented in SIL
/// as GraphOperationInst.
struct GraphOperationInfo {
public:
  /// Indicates how the argument should be lowered to the TF graph.
  enum class ArgumentLowering {
    /// This is a TensorFlow value type, aggregate of TensorFlow value types,
    /// array of TensorFlow value types, or array of aggregates of TensorFlow
    /// value types. It should be lowered to an Input or InputList.
    ///
    /// Written as unnamed argument.
    Input,

    /// This should be lowered to an attribute, in the most direct way. e.g.
    /// integers should be lowered to integer attributes, metatypes should be
    /// lowered to type attributes, TensorShapes should be lowered to shape
    /// attributes, etc.
    ///
    /// Written as named argument without "$" suffix.
    NormalAttribute,

    /// An array or scalar that should be converted to a Tensor before lowering
    /// to an attribute.
    ///
    /// Written as named argument with "$tensor" suffix.
    TensorAttribute,

    /// An array of integers that should be lowered to a shape attribute.
    ///
    /// Written as named argument with "$shape" suffix.
    ShapeAttribute,

    /// A metatype of a TensorFlow value type or aggregate of TensorFlow value
    /// types that should be lowered into a list of unknown shape attributes.
    ///
    /// Written as named argument with "$unknownShapeList" suffix.
    UnknownShapeListAttribute,

    /// A metatype of a TensorFlow value type or aggregate of TensorFlow value
    /// types that should be lowered into a list of type attributes.
    ///
    /// Written as named argument with "$typeList" suffix.
    TypeListAttribute,

    /// An argument specifying the address where an indirect output should be
    /// stored. This occurs when the graph_op exists in a context where its
    /// output is address-only.
    ///
    /// Written as argument with name "$out".
    Out,
  };

  enum StructuredArgumentKind {
    /// Single argument.
    /// Mangled name is ",i${name}" where ${name} is an optional name.
    SAK_Single,
    /// Argument list.
    /// Mangled name is ",L${name},e,...,e" where ${name} is an optional name
    /// and where the number of e's denotes the number of elements.
    SAK_List,
  };

  /// The arguments to a GraphOperationInst may be grouped into various
  /// structures. This is a tagged union representing those structures.
  class StructuredArgument {
    friend struct GraphOperationInfo;

    StructuredArgumentKind Kind;
    StringRef Name;
    union {
      /// Argument for SAK_Single.
      SILValue SingleArgument;
      /// Arguments for SAK_List.
      ArrayRef<Operand> ArgumentList;
    };

  public:
   StructuredArgument(StructuredArgumentKind Kind, StringRef Name,
                      SILValue SingleArgument)
        : Kind(Kind), Name(Name), SingleArgument(SingleArgument) {}
    StructuredArgument(StructuredArgumentKind Kind, StringRef Name,
                      ArrayRef<Operand> ArgumentList)
        : Kind(Kind), Name(Name), ArgumentList(ArgumentList) {}

    StructuredArgumentKind getKind() const {
      return Kind;
    }

    /// Returns the name, including a suffix that denotes the ArgumentLowering.
    StringRef getArgumentNameWithSuffix() const {
      return Name;
    }

    SILValue getSingleArgument() const {
      assert(getKind() == SAK_Single);
      return SingleArgument;
    }

    OperandValueArrayRef getArgumentList() const {
      assert(getKind() == SAK_List);
      return ArgumentList;
    }

    /// Returns this argument's name, without suffix, and the ArgumentLowering.
    std::pair<StringRef, ArgumentLowering> getArgumentNameAndLowering() const;
  };

private:
  /// The instruction being analyzed.
  GraphOperationInst *inst;

  /// The TensorFlow op name, decoded from inst.
  StringRef OperationName;

  /// The StructuredArguments for this operation, decoded from inst. (See
  /// documentation on StructuredArgument for explanation).
  llvm::SmallVector<StructuredArgument, 4> StructuredArguments;

public:
  explicit GraphOperationInfo(GraphOperationInst *inst);

  /// Get the instruction being analyzed.
  GraphOperationInst *getInst() const {
    return inst;
  }

  /// Get the TensorFlow op name.
  llvm::StringRef getOperationName() const {
    return OperationName;
  }

  /// Get the StructuredArguments for this operation. (See documentation on
  /// StructuredArgument for explanation).
  const llvm::SmallVectorImpl<StructuredArgument> &getStructuredArguments() const {
    return StructuredArguments;
  }

  /// Get an int-typed attribute at `attrIdx`, which must have `attrName`.
  int64_t getIntAttr(unsigned attrIdx, llvm::StringRef attrName) const;

  /// Get a string-typed attribute at `attrIdx`, which must have `attrName`.
  std::string getStringAttr(unsigned attrIdx, llvm::StringRef attrName) const;
  // /// Get a float-typed attribute at `attrIdx`, which must have `attrName`.
  // float getFloatAttr(unsigned attrIdx, llvm::StringRef attrName) const;

  void assertWithDump(bool cond, const char *assertMsg) const;

  /// Return the string suffix for the specified ArgumentLowering.
  static const char *getArgumentLoweringSuffix(ArgumentLowering lowering);

  /// Given an argument name like foo$tensor, decode the name and the
  /// ArgumentLowering.  If the name is empty, this defaults to
  /// ArgumentLowering::Input.  If the name is non-empty but there is no
  /// modifier specified, then this defaults to
  /// ArgumentLowering::NormalAttribute.
  static std::pair<llvm::StringRef, ArgumentLowering>
  decodeArgumentName(StringRef Name);
};

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind classifyTensorFlowValue(SILType ty);

} // end namespace tf
} // end namespace swift
#endif // SWIFT_SIL_GRAPH_OPERATION_INFO_H
