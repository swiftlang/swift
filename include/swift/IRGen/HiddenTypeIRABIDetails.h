//===--- HiddenTypeIRABIDetails.h - ABI details for hidden types -*- C++ -*-===//
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
/// \file
/// Defines data classes that describe the ABI layout of types whose
/// definitions are hidden (e.g., from @_implementationOnly imports).
/// These are serialized into swiftmodules in place of the AST
/// representation of a type when ABI details about a hidden type
/// need exporting.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_HIDDENTYPEIRAABIDETAILS_H
#define SWIFT_IRGEN_HIDDENTYPEIRAABIDETAILS_H

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/ReferenceCounting.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILTypeProperties.h"
#include "llvm/ADT/ArrayRef.h"
#include <optional>
#include <vector>

namespace swift {
namespace irgen {

class HiddenTypeIRABIInfo : public ASTAllocated<HiddenTypeIRABIInfo> {
public:
  enum class Kind {
    LoadableStruct,
  };

private:
  Kind TheKind;
  std::string MangledTypeName;
  SILTypeProperties TypeProperties;

protected:
  HiddenTypeIRABIInfo(Kind kind) : TheKind(kind) {}

public:
  Kind getKind() const { return TheKind; }

  StringRef getMangledTypeName() const { return MangledTypeName; }
  void setMangledTypeName(StringRef name) { MangledTypeName = name.str(); }

  SILTypeProperties getSILTypeProperties() const { return TypeProperties; }
  void setSILTypeProperties(SILTypeProperties props) { TypeProperties = props; }

  virtual std::optional<ReferenceCounting> getReferenceCountingSystem() const {
    return std::nullopt;
  }

protected:
  ~HiddenTypeIRABIInfo() {}
};

class HiddenStructTypeIRABIInfo : public HiddenTypeIRABIInfo {
  std::vector<Type> FieldTypes;

public:
  const bool Copyable;

  HiddenStructTypeIRABIInfo(llvm::ArrayRef<Type> fieldTypes,
                            bool copyable)
      : HiddenTypeIRABIInfo(Kind::LoadableStruct),
        FieldTypes(fieldTypes.begin(), fieldTypes.end()),
        Copyable(copyable) {}

  llvm::ArrayRef<Type> getFieldTypes() const { return FieldTypes; }

  static bool classof(const HiddenTypeIRABIInfo *info) {
    return info->getKind() == Kind::LoadableStruct;
  }

protected:
  ~HiddenStructTypeIRABIInfo() {}
};

} // end namespace irgen
} // end namespace swift

#endif // SWIFT_IRGEN_HIDDENTYPEIRAABIDETAILS_H
