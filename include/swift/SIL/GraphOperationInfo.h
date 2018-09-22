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
// decoding the mangled inst name string for the operands and attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GRAPH_OPERATION_INFO_H
#define SWIFT_SIL_GRAPH_OPERATION_INFO_H

#include "swift/AST/Identifier.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class GraphOperationInst;

namespace tf {
/// Holds information about a TensorFlow operation as represented in SIL
/// as GraphOperationInst.
struct GraphOperationInfo {
  /// Indicates how the operand should be lowered to the TF graph.
  enum class OperandLowering {
    /// This is a TensorFlow value type, aggregate of TensorFlow value types,
    /// array of TensorFlow value types, or array of aggregates of TensorFlow
    /// value types. It should be lowered to an Input or InputList.
    ///
    /// Written as unnamed operand.
    Input,

    /// This should be lowered to an attribute, in the most direct way. e.g.
    /// integers should be lowered to integer attributes, metatypes should be
    /// lowered to type attributes, TensorShapes should be lowered to shape
    /// attributes, etc.
    ///
    /// Written as named operand without "$" suffix.
    NormalAttribute,

    /// An array or scalar that should be converted to a Tensor before lowering
    /// to an attribute.
    ///
    /// Written as named operand with "$tensor" suffix.
    TensorAttribute,

    /// An array of integers that should be lowered to a shape attribute.
    ///
    /// Written as named operand with "$shape" suffix.
    ShapeAttribute,

    /// A metatype of a TensorFlow value type or aggregate of TensorFlow value
    /// types that should be lowered into a list of unknown shape attributes.
    ///
    /// Written as named operand with "$unknownShapeList" suffix.
    UnknownShapeListAttribute,

    /// A metatype of a TensorFlow value type or aggregate of TensorFlow value
    /// types that should be lowered into a list of type attributes.
    ///
    /// Written as named operand with "$typeList" suffix.
    TypeListAttribute,

    /// An operand specifying the address where an indirect output should be
    /// stored. This occurs when the graph_op exists in a context where its
    /// output is address-only.
    ///
    /// Written as operand with name "$out".
    Out,
  };

  /// Return the string suffix for the specified OperandLowering.
  static const char * getOperandLoweringSuffix(OperandLowering lowering);

  /// The instruction being analyzed.
  GraphOperationInst *inst;

  explicit GraphOperationInfo(GraphOperationInst *inst) : inst(inst) {}

  /// Return the device attribute associated with `inst`, which is required to
  /// exist.
  // StringRef getDeviceString() const;

  // /// Return the device type for this instruction.
  // DeviceType getDeviceType() const {
  //   return getOpDeviceType(getDeviceString());
  // }

  enum StructuredOperandKind {
    /// Single operand.
    /// Mangled name is ",i${name}" where ${name} is an optional name.
    SOK_Single,
    /// Operand list.
    /// Mangled name is ",L${name},e,...,e" where ${name} is an optional name
    /// and where the number of e's denotes the number of elements.
    SOK_List,
  };

  /// The operands to a GraphOperationInst may be grouped into various
  /// structures. This is a tagged union representing those structures.
  class StructuredOperand {
    friend struct GraphOperationInfo;

    StructuredOperandKind Kind;
    StringRef Name;
    union {
      /// Operand for SOK_Single.
      SILValue SingleOperand;
      /// Operands for SOK_List.
      ArrayRef<Operand> OperandList;
    };

  public:
   StructuredOperand(StructuredOperandKind Kind, StringRef Name,
                      SILValue SingleOperand)
        : Kind(Kind), Name(Name), SingleOperand(SingleOperand) {}
    StructuredOperand(StructuredOperandKind Kind, StringRef Name,
                      ArrayRef<Operand> OperandList)
        : Kind(Kind), Name(Name), OperandList(OperandList) {}

    StructuredOperandKind getKind() const {
      return Kind;
    }

    /// Returns the name, including a suffix that denotes the OperandLowering.
    StringRef getNameWithSuffix() const {
      return Name;
    }

    SILValue getSingleOperand() const {
      assert(getKind() == SOK_Single);
      return SingleOperand;
    }

    OperandValueArrayRef getOperandList() const {
      assert(getKind() == SOK_List);
      return OperandList;
    }

    /// Returns the result of GraphOperationInfo::decodeOperandName on this
    /// operand.
    std::pair<StringRef, OperandLowering> decodeName() const;
  };

  /// Get the TensorFlow op name.
  llvm::StringRef getName() const;

  /// Decode the name of a graph_op into its TensorFlow op name and a list of
  /// StructuredOperands.
  llvm::StringRef decodeName(
      llvm::SmallVectorImpl<StructuredOperand> &structuredOperands) const;

  /// Given an operand name like foo$tensor, decode the name and the
  /// OperandLowering.  If the name is empty, this defaults to
  /// OperandLowering::Input.  If the name is non-empty but there is no
  /// modifier specified, then this defaults to
  /// OperandLowering::NormalAttribute.
  static std::pair<llvm::StringRef, OperandLowering>
  decodeOperandName(StringRef Name);

  /// Get an int-typed attribute at `attrIdx`, which must have `attrName`.
  int64_t getIntAttr(unsigned attrIdx, llvm::StringRef attrName) const;

  /// Get a string-typed attribute at `attrIdx`, which must have `attrName`.
  std::string getStringAttr(unsigned attrIdx, llvm::StringRef attrName) const;
  // /// Get a float-typed attribute at `attrIdx`, which must have `attrName`.
  // float getFloatAttr(unsigned attrIdx, llvm::StringRef attrName) const;

  void assertWithDump(bool cond, const char *assertMsg) const;
};
} // end namespace tf
} // end namespace swift
#endif // SWIFT_SIL_GRAPH_OPERATION_INFO_H
