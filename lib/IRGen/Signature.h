//===--- Signature.h - An IR function signature -----------------*- C++ -*-===//
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
//  This file defines the Signature type, which encapsulates all the
//  information necessary to call a function value correctly.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_SIGNATURE_H
#define SWIFT_IRGEN_SIGNATURE_H

#include "llvm/IR/Attributes.h"
#include "swift/AST/Types.h"

namespace llvm {
  class FunctionType;
}

namespace clang {
  namespace CodeGen {
    class CGFunctionInfo;    
  }
}

namespace swift {
  class Identifier;
  enum class SILFunctionTypeRepresentation : uint8_t;
  class SILType;

namespace irgen {

/// An encapsulation of different foreign calling-convention lowering
/// information we might have.  Should be interpreted according to the
/// abstract CC of the formal function type.
class ForeignFunctionInfo {
public:
  const clang::CodeGen::CGFunctionInfo *ClangInfo = nullptr;
};

/// A signature represents something which can actually be called.
class Signature {
  llvm::FunctionType *Type = nullptr;
  llvm::AttributeList Attributes;
  ForeignFunctionInfo ForeignInfo;

public:
  bool isValid() const {
    return Type != nullptr;
  }

  static Signature get(IRGenModule &IGM, CanSILFunctionType formalType);

  llvm::FunctionType *getType() const {
    assert(isValid());
    return Type;
  }

  llvm::AttributeList getAttributes() const {
    assert(isValid());
    return Attributes;
  }

  const ForeignFunctionInfo &getForeignInfo() const {
    assert(isValid());
    return ForeignInfo;
  }
};

} // end namespace irgen
} // end namespace swift

#endif
