//===--- AbstractLayout.h - Abstract type layout information ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures for abstract type layout information,
// used to encode the layout of hidden C types in .swiftmodule files.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ABSTRACTLAYOUT_H
#define SWIFT_AST_ABSTRACTLAYOUT_H

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/ReferenceCounting.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILTypeProperties.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class NominalTypeDecl;

enum class NativeConventionLLVMTypeKind : uint8_t {
  Integer,
  Pointer,
  Half,
  Float,
  Double,
  FP128,
  PPCFP128,
  X86FP80,
  FixedVector,
};

struct NativeConventionComponent {
  uint64_t begin;
  uint64_t end;
  NativeConventionLLVMTypeKind typeKind;
  uint64_t typePayload;
  NativeConventionLLVMTypeKind vectorElementKind;
  uint64_t vectorElementPayload;
};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wnon-virtual-dtor"
class AbstractTypeLayout : public ASTAllocated<AbstractTypeLayout> {
public:
  enum class Kind : uint8_t {
    LoadableTrivialHiddenType,
  };

private:
  Kind TheKind;

public:
  std::string mangledName;
  SILTypeProperties typeProperties;
  std::optional<ReferenceCounting> referenceCountingSystem;

  explicit AbstractTypeLayout(Kind kind) : TheKind(kind) {}

  Kind getKind() const { return TheKind; }
  virtual void print(llvm::raw_ostream &os) const = 0;
};

class LoadableTrivialHiddenTypeAbstractLayout final
    : public AbstractTypeLayout {
public:
  uint64_t size;
  uint64_t alignment;
  uint64_t stride;
  bool bitwiseCopyable;
  bool isOpaque;
  bool nativeParameterRequiresIndirect = false;
  bool nativeResultRequiresIndirect = false;
  std::vector<NativeConventionComponent> nativeComponents;

  LoadableTrivialHiddenTypeAbstractLayout(uint64_t size, uint64_t alignment,
                                          uint64_t stride,
                                          bool bitwiseCopyable)
      : LoadableTrivialHiddenTypeAbstractLayout(
            size, alignment, stride, bitwiseCopyable, /*opaque=*/false) {}

  LoadableTrivialHiddenTypeAbstractLayout(uint64_t size, uint64_t alignment,
                                          uint64_t stride,
                                          bool bitwiseCopyable, bool opaque)
      : AbstractTypeLayout(Kind::LoadableTrivialHiddenType) {
    this->size = size;
    this->alignment = alignment;
    this->stride = stride;
    this->bitwiseCopyable = bitwiseCopyable;
    this->isOpaque = opaque;
    typeProperties = SILTypeProperties::forTrivial();
  }

  void print(llvm::raw_ostream &os) const override {
    os << "kind=LoadableTrivialHiddenType"
       << ", mangledName=" << mangledName
       << ", size=" << size
       << ", alignment=" << alignment
       << ", stride=" << stride
       << ", bitwiseCopyable=" << (bitwiseCopyable ? "true" : "false")
       << ", opaque=" << (isOpaque ? "true" : "false")
       << ", typeProperties=" << typeProperties.getRawFlags()
       << ", nativeParameterRequiresIndirect="
       << (nativeParameterRequiresIndirect ? "true" : "false")
       << ", nativeResultRequiresIndirect="
       << (nativeResultRequiresIndirect ? "true" : "false")
       << ", nativeComponents=" << nativeComponents.size();
  }
};
#pragma clang diagnostic pop

} // namespace swift

#endif // SWIFT_AST_ABSTRACTLAYOUT_H
