//===--- JSON.cpp - Symbol Graph JSON Helpers -----------------------------===//
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
// Adds Symbol Graph JSON serialization to other types.
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "JSON.h"

void swift::symbolgraphgen::serialize(const llvm::VersionTuple &VT,
                                      llvm::json::OStream &OS) {
  OS.object([&](){
    OS.attribute("major", VT.getMajor());
    if (VT.getMinor()) {
      OS.attribute("minor", *VT.getMinor());
    }
    if (VT.getSubminor()) {
      OS.attribute("patch", *VT.getSubminor());
    }
    // Despite the name,
    // this is not Semantic Versioning "build metadata"
    if (VT.getBuild()) {
      OS.attribute("prerelease", *VT.getBuild());
    }
  });
}

void swift::symbolgraphgen::serialize(const llvm::Triple &T,
                                      llvm::json::OStream &OS) {
  OS.object([&](){
    OS.attribute("architecture", T.getArchName());
    if (!T.getEnvironmentName().empty()) {
      OS.attribute("environment", T.getEnvironmentName());
    }
    OS.attribute("vendor", T.getVendorName());
    OS.attributeObject("operatingSystem", [&](){
      OS.attribute("name", T.getOSTypeName(T.getOS()));

      unsigned Major;
      unsigned Minor;
      unsigned Patch;
      T.getOSVersion(Major, Minor, Patch);
      llvm::VersionTuple OSVersion(Major, Minor, Patch);

      OS.attributeBegin("minimumVersion");
      serialize(OSVersion, OS);
      OS.attributeEnd();
    });
  });
}

void swift::symbolgraphgen::serialize(const ExtensionDecl *Extension,
                                      llvm::json::OStream &OS) {
  OS.attributeObject("swiftExtension", [&](){
    if (const auto *ExtendedNominal = Extension->getExtendedNominal()) {
      if (const auto *ExtendedModule = ExtendedNominal->getModuleContext()) {
        OS.attribute("extendedModule", ExtendedModule->getNameStr());
      }
    }

    SmallVector<Requirement, 4> FilteredRequirements;

    filterGenericRequirements(Extension,
                              FilteredRequirements);

    if (!FilteredRequirements.empty()) {
      OS.attributeArray("constraints", [&](){
        for (const auto &Requirement : FilteredRequirements) {
          serialize(Requirement, OS);
        }
      }); // end constraints:
    }
  }); // end swiftExtension:
}

void swift::symbolgraphgen::serialize(const Requirement &Req,
                                      llvm::json::OStream &OS) {
  StringRef Kind;
  switch (Req.getKind()) {
    case RequirementKind::Conformance:
      Kind = "conformance";
      break;
    case RequirementKind::Superclass:
      Kind = "superclass";
      break;
    case RequirementKind::SameType:
      Kind = "sameType";
      break;
    case RequirementKind::Layout:
      return;
  }

  OS.object([&](){
    OS.attribute("kind", Kind);
    OS.attribute("lhs", Req.getFirstType()->getString());
    OS.attribute("rhs", Req.getSecondType()->getString());
  });
}

void swift::symbolgraphgen::serialize(const swift::GenericTypeParamType *Param,
                                      llvm::json::OStream &OS) {
  OS.object([&](){
    OS.attribute("name", Param->getName().str());
    OS.attribute("index", Param->getIndex());
    OS.attribute("depth", Param->getDepth());
  });
}

void swift::symbolgraphgen::filterGenericRequirements(
    ArrayRef<Requirement> Requirements,
    const NominalTypeDecl *Self,
    SmallVectorImpl<Requirement> &FilteredRequirements) {
  for (const auto &Req : Requirements) {
    if (Req.getKind() == RequirementKind::Layout) {
      continue;
    }
    // extension /* protocol */ Q {
    // func foo() {}
    // }
    // ignore Self : Q, obvious
    if (Req.getSecondType()->getAnyNominal() == Self) {
      continue;
    }
    FilteredRequirements.push_back(Req);
  }
}

void
swift::symbolgraphgen::filterGenericRequirements(const ExtensionDecl *Extension,
    SmallVectorImpl<Requirement> &FilteredRequirements) {
  for (const auto &Req : Extension->getGenericRequirements()) {
    if (Req.getKind() == RequirementKind::Layout) {
      continue;
    }

    if (!isa<ProtocolDecl>(Extension->getExtendedNominal()) &&
        Req.getFirstType()->isEqual(Extension->getExtendedType())) {
      continue;
    }

    // extension /* protocol */ Q
    // ignore Self : Q, obvious
    if (Req.getSecondType()->isEqual(Extension->getExtendedType())) {
      continue;
    }
    FilteredRequirements.push_back(Req);
  }
}
