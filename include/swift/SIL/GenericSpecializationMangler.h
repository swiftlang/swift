//===- GenericSpecializationMangler.h - generic specializations -*- C++ -*-===//
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

#ifndef SWIFT_SIL_UTILS_GENERICSPECIALIZATIONMANGLER_H
#define SWIFT_SIL_UTILS_GENERICSPECIALIZATIONMANGLER_H

#include "swift/AST/ASTMangler.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/SILFunction.h"

namespace swift {
namespace Mangle {

enum class SpecializationKind : uint8_t {
  Generic,
  NotReAbstractedGeneric,
  FunctionSignature,
};

/// Inject SpecializationPass into the Mangle namespace.
using SpecializationPass = Demangle::SpecializationPass;

/// The base class for specialization mangles.
class SpecializationMangler : public Mangle::ASTMangler {
protected:
  /// The specialization pass.
  SpecializationPass Pass;

  IsSerialized_t Serialized;

  /// The original function which is specialized.
  SILFunction *Function;
  std::string FunctionName;

  llvm::SmallVector<char, 32> ArgOpStorage;
  llvm::raw_svector_ostream ArgOpBuffer;

protected:
  SpecializationMangler(SpecializationPass P, IsSerialized_t Serialized,
                        SILFunction *F)
      : Pass(P), Serialized(Serialized), Function(F),
        ArgOpBuffer(ArgOpStorage) {}

  SpecializationMangler(SpecializationPass P, IsSerialized_t Serialized,
                        std::string functionName)
      : Pass(P), Serialized(Serialized), Function(nullptr),
        FunctionName(functionName), ArgOpBuffer(ArgOpStorage) {}

  void beginMangling();

  /// Finish the mangling of the symbol and return the mangled name.
  std::string finalize();

  void appendSpecializationOperator(StringRef Op) {
    appendOperator(Op, StringRef(ArgOpStorage.data(), ArgOpStorage.size()));
  }
};

// The mangler for specialized generic functions.
class GenericSpecializationMangler : public SpecializationMangler {

  SubstitutionMap SubMap;
  bool isReAbstracted;
  bool isInlined;
  bool isPrespecializaton;

public:
  GenericSpecializationMangler(SILFunction *F, SubstitutionMap SubMap,
                               IsSerialized_t Serialized, bool isReAbstracted,
                               bool isInlined = false,
                               bool isPrespecializaton = false)
      : SpecializationMangler(SpecializationPass::GenericSpecializer,
                              Serialized, F),
        SubMap(SubMap), isReAbstracted(isReAbstracted), isInlined(isInlined),
        isPrespecializaton(isPrespecializaton) {}

  GenericSpecializationMangler(std::string origFuncName, SubstitutionMap SubMap)
      : SpecializationMangler(SpecializationPass::GenericSpecializer,
                              IsNotSerialized, origFuncName),
        SubMap(SubMap), isReAbstracted(true), isInlined(false),
        isPrespecializaton(true) {}

  std::string mangle(GenericSignature Sig = GenericSignature());

  // TODO: This utility should move from the libswiftSILOptimizer to
  // libswiftSIL.
  static std::string manglePrespecialization(std::string unspecializedName,
                                             GenericSignature genericSig,
                                             GenericSignature specializedSig);
};

} // end namespace Mangle
} // end namespace swift

#endif
