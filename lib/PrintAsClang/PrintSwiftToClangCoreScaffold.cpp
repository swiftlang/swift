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
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/IRGen/IRABIDetailsProvider.h"
#include "swift/IRGen/Linking.h"
#include "clang/Basic/AddressSpaces.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

static void printKnownStruct(ASTContext &astContext,
    PrimitiveTypeMapping &typeMapping, raw_ostream &os, StringRef name,
    const IRABIDetailsProvider::TypeRecordABIRepresentation &typeRecord) {
  assert(typeRecord.getMembers().size() > 1);
  os << "struct " << name << " {\n";
  for (const auto &ty : llvm::enumerate(typeRecord.getMembers())) {
    os << "  ";
    ClangSyntaxPrinter(astContext, os).printKnownCType(ty.value(), typeMapping);
    os << " _" << ty.index() << ";\n";
  }
  os << "};\n";
}

static void printKnownTypedef(ASTContext &astContext,
    PrimitiveTypeMapping &typeMapping, raw_ostream &os, StringRef name,
    const IRABIDetailsProvider::TypeRecordABIRepresentation &typeRecord) {
  assert(typeRecord.getMembers().size() == 1);
  os << "typedef ";
  ClangSyntaxPrinter(astContext, os).printKnownCType(typeRecord.getMembers()[0],
                                         typeMapping);
  os << " " << name << ";\n";
}

static void printKnownType(ASTContext &astContext,
    PrimitiveTypeMapping &typeMapping, raw_ostream &os, StringRef name,
    const IRABIDetailsProvider::TypeRecordABIRepresentation &typeRecord) {
  if (typeRecord.getMembers().size() == 1)
    return printKnownTypedef(astContext, typeMapping, os, name, typeRecord);
  printKnownStruct(astContext, typeMapping, os, name, typeRecord);
}

static void printTypeMetadataResponseType(SwiftToClangInteropContext &ctx,
                                          PrimitiveTypeMapping &typeMapping,
                                          raw_ostream &os) {
  os << "// Swift type metadata response type.\n";
  // Print out the type metadata structure.
  auto funcSig = ctx.getIrABIDetails().getTypeMetadataAccessFunctionSignature();
  printKnownType(ctx.getASTContext(), typeMapping, os, "MetadataResponseTy", funcSig.returnType);
  assert(funcSig.parameterTypes.size() == 1);
  os << "// Swift type metadata request type.\n";
  printKnownType(ctx.getASTContext(), typeMapping, os, "MetadataRequestTy",
                 funcSig.parameterTypes[0]);
}

void printPrimitiveGenericTypeTraits(raw_ostream &os, ASTContext &astContext,
                                     PrimitiveTypeMapping &typeMapping,
                                     bool isCForwardDefinition) {
  SmallVector<Type, 16> supportedPrimitiveTypes = {
      astContext.getBoolType(),

      // Primitive integer, C integer and Int/UInt mappings.
      astContext.getInt8Type(), astContext.getUInt8Type(),
      astContext.getInt16Type(), astContext.getUInt16Type(),
      astContext.getInt32Type(), astContext.getUInt32Type(),
      astContext.getInt64Type(), astContext.getUInt64Type(),

      // Primitive floating point type mappings.
      astContext.getFloatType(), astContext.getDoubleType(),

      // Pointer types.
      // FIXME: support raw pointers?
      astContext.getOpaquePointerType(),
  };

  // Ensure that `long` and `unsigned long` are treated as valid
  // generic Swift types (`Int` and `UInt`) on platforms
  // that do define `Int`/`ptrdiff_t` as `long` and don't define `int64_t` to be
  // `long`.
  auto &clangTI =
      astContext.getClangModuleLoader()->getClangASTContext().getTargetInfo();
  bool isSwiftIntLong =
      clangTI.getPtrDiffType(clang::LangAS::Default) == clang::TransferrableTargetInfo::SignedLong;
  bool isInt64Long =
      clangTI.getInt64Type() == clang::TransferrableTargetInfo::SignedLong;
  if (isSwiftIntLong && !isInt64Long) {
    supportedPrimitiveTypes.push_back(astContext.getIntType());
    supportedPrimitiveTypes.push_back(astContext.getUIntType());
  }

  // The following Swift types bridge to C++ types that aren't available on
  // every target. Only emit trait specializations for them when the target
  // supports the underlying C++ type, so that the generated header remains
  // compilable everywhere while still supporting these types where possible.
  auto stdlibModule = astContext.getStdlibModule();

  // We do not have metadata for primitive types in Embedded Swift.
  // As a result, the following features are not supported with primitive types in this mode:
  // - Dynamic casts
  // - Static self
  // - Generic requirement parameters
  // - Metadata source parameter
  bool embedded = astContext.LangOpts.hasFeature(Feature::Embedded);
  bool guardAvailability = !embedded && astContext.LangOpts.Target.isOSDarwin();

  // Some of these types were introduced after the oldest runtime that the
  // deployment target supports. Referencing their type metadata from the
  // generated header would make the C++ client fail to launch when back
  // deployed.
  auto isAvailableAtDeploymentTarget = [&](Type type) {
    auto nominal = type->getNominalDecl();
    if (!nominal)
      return false;
    if (embedded)
      return true;
    auto availableRange = AvailabilityInference::availableRange(nominal);
    if (guardAvailability)
      return !availableRange.isKnownUnreachable();
    return AvailabilityRange::forDeploymentTarget(astContext)
        .isContainedIn(availableRange);
  };
  auto addIfAvailable = [&](Type type) {
    if (type && isAvailableAtDeploymentTarget(type))
      supportedPrimitiveTypes.push_back(type);
  };

  addIfAvailable(astContext.getNamedSwiftType(stdlibModule, "CChar32"));

  if (clangTI.hasInt128Type()) {
    addIfAvailable(astContext.getInt128Type());
    addIfAvailable(astContext.getUInt128Type());
  }

  if (clangTI.hasFloat16Type())
    addIfAvailable(astContext.getNamedSwiftType(stdlibModule, "Float16"));

  for (Type type : supportedPrimitiveTypes) {
    // A separately installed stdlib header serves every deployment target for
    // this platform. Select newer primitive metadata at C++ compile time rather
    // than baking in the deployment target used to build the toolchain.
    auto availableRange =
        AvailabilityInference::availableRange(type->getNominalDecl());
    bool emitAvailabilityGuard =
        guardAvailability && availableRange.hasMinimumVersion();
    if (emitAvailabilityGuard) {
      auto version = availableRange.getRawMinimumVersion();
      unsigned major = version.getMajor();
      unsigned minor = version.getMinor().value_or(0);
      unsigned patch = version.getSubminor().value_or(0);
      unsigned encodedVersion = major * 10000 + minor * 100 + patch;
      // Clang uses the older four-digit encoding before macOS 10.10.
      if (astContext.LangOpts.Target.isMacOSX() && major == 10 && minor < 10)
        encodedVersion = major * 100 + minor * 10 + patch;
      os << "#if __ENVIRONMENT_OS_VERSION_MIN_REQUIRED__ >= " << encodedVersion
         << "\n";
    }
    auto endAvailabilityGuard = llvm::make_scope_exit([&] {
      if (emitAvailabilityGuard)
        os << "#endif\n";
    });
    auto typeInfo = *typeMapping.getKnownCxxTypeInfo(
        type->getNominalDecl());

    if (!isCForwardDefinition) {
      os << "template<>\n";
      os << "inline const constexpr bool isUsableInGenericContext<"
         << typeInfo.name << "> = true;\n\n";
    }

    if (embedded)
      continue;

    auto typeMetadataFunc = irgen::LinkEntity::forTypeMetadata(
        type->getCanonicalType(), irgen::TypeMetadataAddress::AddressPoint);
    std::string typeMetadataVarName = typeMetadataFunc.mangleAsString(type->getASTContext());

    if (isCForwardDefinition) {
      os << "// type metadata address for " << type.getString() << ".\n";
      os << "SWIFT_IMPORT_STDLIB_SYMBOL extern size_t " << typeMetadataVarName
         << ";\n";
      continue;
    }

    os << "template<>\nstruct TypeMetadataTrait<" << typeInfo.name << "> {\n"
       << "  static ";
    // This is deliberately not printed as an inline thunk: in debug mode
    // inline thunks are marked as `used`, which would emit all of these
    // accessors even when the program never uses the corresponding type.
    ClangSyntaxPrinter(astContext, os).printInlineForHelperFunction();
    os << "void * _Nonnull getTypeMetadata() {\n"
       << "    return &" << cxx_synthesis::getCxxImplNamespaceName()
       << "::" << typeMetadataVarName << ";\n"
       << "  }\n};\n\n";
  }
}

void swift::printSwiftToClangCoreScaffold(SwiftToClangInteropContext &ctx,
                                          ASTContext &astContext,
                                          PrimitiveTypeMapping &typeMapping,
                                          raw_ostream &os) {
  ClangSyntaxPrinter printer(astContext, os);
  printer.printNamespace(
      cxx_synthesis::getCxxSwiftNamespaceName(),
      [&](raw_ostream &) {
        printer.printNamespace(
            cxx_synthesis::getCxxImplNamespaceName(), [&](raw_ostream &) {
              printer.printExternC([&](raw_ostream &os) {
                printTypeMetadataResponseType(ctx, typeMapping, os);
                os << "\n";
                printPrimitiveGenericTypeTraits(os, astContext, typeMapping,
                                                /*isCForwardDefinition=*/true);
              });
              os << "\n";
            });
        os << "\n";
        // C++ only supports inline variables from C++17.
        ClangSyntaxPrinter(astContext, os)
            .printIgnoredCxx17ExtensionDiagnosticBlock([&]() {
              printPrimitiveGenericTypeTraits(os, astContext, typeMapping,
                                              /*isCForwardDefinition=*/false);
            });
      },
      ClangSyntaxPrinter::NamespaceTrivia::AttributeSwiftPrivate);
}
