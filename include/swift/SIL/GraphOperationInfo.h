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
#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
class GraphOperationInst;
class SymbolicValue;

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
    /// integers should be lowered to integer attributes, TensorShapes should
    /// be lowered to shape attributes, etc.
    ///
    /// Written as named argument without "$" suffix.
    NormalAttribute,

    /// An array or scalar that should be converted to a Tensor before lowering
    /// to an attribute.
    ///
    /// Written as named argument with "$tensor" suffix.
    ///
    /// graph_op lowering in TFLowerGraph.cpp and in IRGenSIL.cpp relies on
    /// other attributes in order to interpret the meaning of a
    /// TensorAttribute:
    /// - TensorAttributes must come after a $dtype attr specifying the dtype
    ///   for the Tensor
    /// - TensorAttributes with array values must have a $shape attr immediately
    ///   after them, specifying the shape of the tensor
    ///
    /// Dynamic graph_op lowering in IRGenSIL.cpp cannot lower TensorAttributes,
    /// so all TensorAttributes, and the corresponding $dtype and $shape
    /// attributes must be fully const-evaluated before lowering. This does not
    /// limit anything, because programs that need to initialize tensors at
    /// runtime can simply use the runtime tensor initialization functions in
    /// the TensorFlow module.
    TensorAttribute,

    /// An array of integers that should be lowered to a shape attribute.
    ///
    /// Written as named argument with "$shape" suffix.
    ShapeAttribute,

    /// A metatype of a TensorFlow value type or aggregate of TensorFlow value
    /// types that should be lowered into a list of unknown shape attributes.
    ///
    /// Written as named argument with "$unknownShapeList" suffix.
    ///
    /// TODO(SR-8830): Remove this when we have a TensorAggregate protocol.
    UnknownShapeListAttribute,

    /// A metatype of a TensorFlow value type or aggregate of TensorFlow value
    /// types. Deabstraction lowers this to TFDataTypeAttribute.
    ///
    /// Written as named argument with "$typeList" suffix.
    ///
    /// TODO(SR-8830): Remove this when we have a TensorAggregate protocol.
    TypeListAttribute,

    /// A TensorDataType (which is two nested structs wrapping a UInt32), a
    /// UInt32 representing a TensorDataType, or a list of such elements.
    /// Should be lowered to a type attribute or type list attribute.
    ///
    /// Written as named argument with "$dtype" suffix.
    TFDataTypeAttribute,

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

    /// Whether this argument must be lowered to a constant, even for IRGen'd
    /// graph_ops.
    bool mustBeLoweredToConstant() const {
      return std::get<1>(getArgumentNameAndLowering()) ==
          ArgumentLowering::TensorAttribute;
    }
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

/// TODO: Consider moving these helper functions and consts to a separate header
/// file.

/// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
/// VariantHandle.
bool isTensorFlowValue(SILType ty);

/// Determine whether the specified type is one of our well-known types, and
/// if so, which one it is.
TFValueKind classifyTensorFlowValue(SILType ty);

// We assume the following special attr names do not occur in the regular
// attributes of any TF ops.
static const char TF_DEVICE_ATTR[] = "__device";
// This pseudo-attribute is propagated from a tfop inst to TensorTransfer, and
// then to D2D send/recv insts. When lowering to TF graph, the pseudo-attribute
// is used when creating TPU infeed/outfeed ops, and is dropped when creating
// other TF ops (e.g. a "Const" op).
static const char TF_SHAPE_ARRAY_ATTR[] = "__shapes";

/// Return true if `attrName` is TF_SHAPE_ARRAY_ATTR, `attrValue` is an array of
/// TensorShape-typed elements.
bool isShapeArrayPseudoAttr(StringRef attrName, SymbolicValue attrValue);

/// Decode a shape attribute of type TensorShape or Optional<TensorShape>. It
/// stores the dimensions to `result`, and return the rank. Note that "nil as
/// Optional<TensorShape>" represents "unknown rank", and that we return -1 in
/// that case.
int decodeShapeAttr(const ASTContext &ctx, SymbolicValue attr,
                    SmallVectorImpl<int64_t> &result);

/// Decode the shape array in `attrValue` into `dims`, `numDims` and `dimPtrs`.
void decodeShapeArray(const ASTContext &ctx, SymbolicValue attrValue,
                      SmallVectorImpl<int64_t> &dims,
                      SmallVectorImpl<int> &numDims,
                      SmallVectorImpl<int64_t *> &dimPtrs);

/// Return the TF_DataType represented by `value`. `value` must be a 32-bit
/// unsigned integer value, or a single element aggregate of a 32-bit unsigned
/// integer value.
unsigned getTFDataType(SymbolicValue value);

/// Return a constant integer representing the TensorDataType for the given
/// Swift type. `type` must be a valid TensorFlow type.
SymbolicValue convertSwiftTypeToConstantTFDataType(Type type);

/// Return the graph function name for a SIL function that is being used as a
/// function attribute. This transformation may modify the name to make it
/// conform to graph function name rules.
std::string getGraphFuncNameForFuncAttr(StringRef silFnName);

} // end namespace tf
} // end namespace swift
#endif // SWIFT_SIL_GRAPH_OPERATION_INFO_H
