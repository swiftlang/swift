//===--- PrintSwiftToClangCoreScaffold.cpp - Print core decls ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "PrintSwiftToClangCoreScaffold.h"
#include "ClangSyntaxPrinter.h"
#include "PrimitiveTypeMapping.h"
#include "SwiftToClangInteropContext.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/IRGen/IRABIDetailsProvider.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

static void printKnownCType(Type t, PrimitiveTypeMapping &typeMapping,
                            raw_ostream &os) {
  auto info =
      typeMapping.getKnownCTypeInfo(t->getNominalOrBoundGenericNominal());
  assert(info.hasValue() && "not a known type");
  os << info->name;
  if (info->canBeNullable)
    os << " _Null_unspecified";
}

static void printKnownStruct(
    PrimitiveTypeMapping &typeMapping, raw_ostream &os, StringRef name,
    const IRABIDetailsProvider::TypeRecordABIRepresentation &typeRecord) {
  assert(typeRecord.getMembers().size() > 1);
  os << "struct " << name << " {\n";
  for (const auto &ty : llvm::enumerate(typeRecord.getMembers())) {
    os << "  ";
    printKnownCType(ty.value(), typeMapping, os);
    os << " _" << ty.index() << ";\n";
  }
  os << "};\n";
}

static void printKnownTypedef(
    PrimitiveTypeMapping &typeMapping, raw_ostream &os, StringRef name,
    const IRABIDetailsProvider::TypeRecordABIRepresentation &typeRecord) {
  assert(typeRecord.getMembers().size() == 1);
  os << "typedef ";
  printKnownCType(typeRecord.getMembers()[0], typeMapping, os);
  os << " " << name << ";\n";
}

static void printKnownType(
    PrimitiveTypeMapping &typeMapping, raw_ostream &os, StringRef name,
    const IRABIDetailsProvider::TypeRecordABIRepresentation &typeRecord) {
  if (typeRecord.getMembers().size() == 1)
    return printKnownTypedef(typeMapping, os, name, typeRecord);
  printKnownStruct(typeMapping, os, name, typeRecord);
}

static void printValueWitnessTableFunctionType(raw_ostream &os, StringRef name,
                                               StringRef returnType,
                                               std::string paramTypes,
                                               uint16_t ptrauthDisc) {
  os << "using ValueWitness" << name << "Ty = " << returnType
     << "(* __ptrauth_swift_value_witness_function_pointer(" << ptrauthDisc
     << "))(" << paramTypes << ");\n";
}

static std::string makeParams(const char *arg) { return arg; }

template <class... T>
static std::string makeParams(const char *arg, const T... args) {
  return std::string(arg) + ", " + makeParams(args...);
}

static void printValueWitnessTable(raw_ostream &os) {
  std::string members;
  llvm::raw_string_ostream membersOS(members);

#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define DATA_VALUE_WITNESS(lowerId, upperId, type)                             \
  membersOS << "  " << type << " " << #lowerId << ";\n";
#define FUNCTION_VALUE_WITNESS(lowerId, upperId, returnType, paramTypes)       \
  printValueWitnessTableFunctionType(                                          \
      os, #upperId, returnType, makeParams paramTypes,                         \
      SpecialPointerAuthDiscriminators::upperId);                              \
  membersOS << "  ValueWitness" << #upperId << "Ty _Nonnull " << #lowerId      \
            << ";\n";
#define MUTABLE_VALUE_TYPE "void * _Nonnull"
#define IMMUTABLE_VALUE_TYPE "const void * _Nonnull"
#define MUTABLE_BUFFER_TYPE "void * _Nonnull"
#define IMMUTABLE_BUFFER_TYPE "const void * _Nonnull"
#define TYPE_TYPE "void * _Nonnull"
#define SIZE_TYPE "size_t"
#define INT_TYPE "int"
#define UINT_TYPE "unsigned"
#define VOID_TYPE "void"
#include "swift/ABI/ValueWitness.def"

  os << "\nstruct ValueWitnessTable {\n" << membersOS.str() << "};\n";
}

static void printTypeMetadataResponseType(SwiftToClangInteropContext &ctx,
                                          PrimitiveTypeMapping &typeMapping,
                                          raw_ostream &os) {
  os << "// Swift type metadata response type.\n";
  // Print out the type metadata structure.
  auto funcSig = ctx.getIrABIDetails().getTypeMetadataAccessFunctionSignature();
  printKnownType(typeMapping, os, "MetadataResponseTy", funcSig.returnType);
  assert(funcSig.parameterTypes.size() == 1);
  os << "// Swift type metadata request type.\n";
  printKnownType(typeMapping, os, "MetadataRequestTy",
                 funcSig.parameterTypes[0]);
}

void swift::printSwiftToClangCoreScaffold(SwiftToClangInteropContext &ctx,
                                          PrimitiveTypeMapping &typeMapping,
                                          raw_ostream &os) {
  ClangSyntaxPrinter printer(os);
  printer.printNamespace("swift", [&](raw_ostream &) {
    printer.printNamespace(
        cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &) {
          printer.printExternC([&](raw_ostream &os) {
            printTypeMetadataResponseType(ctx, typeMapping, os);
            os << "\n";
            printValueWitnessTable(os);
          });
        });
  });
}
