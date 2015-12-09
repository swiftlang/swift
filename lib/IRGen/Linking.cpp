//===--- Linking.cpp - Name mangling for IRGen entities -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements name mangling for IRGen entities with linkage.
//
//===----------------------------------------------------------------------===//

#include "Linking.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/AST/Mangle.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;
using namespace irgen;
using namespace Mangle;

static StringRef mangleValueWitness(ValueWitness witness) {
  // The ones with at least one capital are the composite ops, and the
  // capitals correspond roughly to the positions of buffers (as
  // opposed to objects) in the arguments.  That doesn't serve any
  // direct purpose, but it's neat.
  switch (witness) {
  case ValueWitness::AllocateBuffer: return "al";
  case ValueWitness::AssignWithCopy: return "ca";
  case ValueWitness::AssignWithTake: return "ta";
  case ValueWitness::DeallocateBuffer: return "de";
  case ValueWitness::Destroy: return "xx";
  case ValueWitness::DestroyBuffer: return "XX";
  case ValueWitness::DestroyArray: return "Xx";
  case ValueWitness::InitializeBufferWithCopyOfBuffer: return "CP";
  case ValueWitness::InitializeBufferWithCopy: return "Cp";
  case ValueWitness::InitializeWithCopy: return "cp";
  case ValueWitness::InitializeBufferWithTake: return "Tk";
  case ValueWitness::InitializeWithTake: return "tk";
  case ValueWitness::ProjectBuffer: return "pr";
  case ValueWitness::InitializeBufferWithTakeOfBuffer: return "TK";
  case ValueWitness::InitializeArrayWithCopy: return "Cc";
  case ValueWitness::InitializeArrayWithTakeFrontToBack: return "Tt";
  case ValueWitness::InitializeArrayWithTakeBackToFront: return "tT";
  case ValueWitness::StoreExtraInhabitant: return "xs";
  case ValueWitness::GetExtraInhabitantIndex: return "xg";
  case ValueWitness::GetEnumTag: return "ug";
  case ValueWitness::DestructiveProjectEnumData: return "up";
  case ValueWitness::DestructiveInjectEnumTag: return "ui";
      
  case ValueWitness::Size:
  case ValueWitness::Flags:
  case ValueWitness::Stride:
  case ValueWitness::ExtraInhabitantFlags:
    llvm_unreachable("not a function witness");
  }
  llvm_unreachable("bad witness kind");
}

/// Mangle this entity into the given buffer.
void LinkEntity::mangle(SmallVectorImpl<char> &buffer) const {
  llvm::raw_svector_ostream stream(buffer);
  mangle(stream);
}

/// Use the Clang importer to mangle a Clang declaration.
static void mangleClangDecl(raw_ostream &buffer,
                            const clang::NamedDecl *clangDecl,
                            ASTContext &ctx) {
  auto *importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
  importer->getMangledName(buffer, clangDecl);
}

/// Mangle this entity into the given stream.
void LinkEntity::mangle(raw_ostream &buffer) const {
  // Almost everything below gets the common prefix:
  //   mangled-name ::= '_T' global
  Mangler mangler(buffer);
  switch (getKind()) {
  //   global ::= 'w' value-witness-kind type     // value witness
  case Kind::ValueWitness:
    buffer << "_Tw";
    buffer << mangleValueWitness(getValueWitness());
   
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    return;

  //   global ::= 'WV' type                       // value witness
  case Kind::ValueWitnessTable:
    buffer << "_TWV";
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    return;

  //   global ::= 't' type
  // Abstract type manglings just follow <type>.
  case Kind::TypeMangling:
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    return;

  //   global ::= 'Ma' type               // type metadata access function
  case Kind::TypeMetadataAccessFunction:
    buffer << "_TMa";
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    return;

  //   global ::= 'ML' type               // type metadata lazy cache variable
  case Kind::TypeMetadataLazyCacheVariable:
    buffer << "_TML";
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    return;

  //   global ::= 'Mf' type                       // 'full' type metadata
  //   global ::= 'M' directness type             // type metadata
  //   global ::= 'MP' directness type            // type metadata pattern
  case Kind::TypeMetadata:
    switch (getMetadataAddress()) {
    case TypeMetadataAddress::FullMetadata:
      mangler.mangleTypeFullMetadataFull(getType());
      break;
    case TypeMetadataAddress::AddressPoint:
      mangler.mangleTypeMetadataFull(getType(), isMetadataPattern());
      break;
    }
    return;

  //   global ::= 'M' directness type             // type metadata
  case Kind::ForeignTypeMetadataCandidate:
    mangler.mangleTypeMetadataFull(getType(), /*isPattern=*/false);
    return;

  //   global ::= 'Mm' type                       // class metaclass
  case Kind::SwiftMetaclassStub:
    buffer << "_TMm";
    mangler.mangleNominalType(cast<ClassDecl>(getDecl()),
                              ResilienceExpansion::Minimal,
                              Mangler::BindGenerics::None);
    return;
      
  //   global ::= 'Mn' type                       // nominal type descriptor
  case Kind::NominalTypeDescriptor:
    buffer << "_TMn";
    mangler.mangleNominalType(cast<NominalTypeDecl>(getDecl()),
                              ResilienceExpansion::Minimal,
                              Mangler::BindGenerics::None);
    return;

  //   global ::= 'Mp' type                       // protocol descriptor
  case Kind::ProtocolDescriptor:
    buffer << "_TMp";
    mangler.mangleProtocolName(cast<ProtocolDecl>(getDecl()));
    return;
      
  //   global ::= 'Wo' entity
  case Kind::WitnessTableOffset:
    buffer << "_TWo";

    // Witness table entries for constructors always refer to the allocating
    // constructor.
    if (auto ctor = dyn_cast<ConstructorDecl>(getDecl()))
      mangler.mangleConstructorEntity(ctor, /*isAllocating=*/true,
                                      getResilienceExpansion(),
                                      getUncurryLevel());
    else
      mangler.mangleEntity(getDecl(), getResilienceExpansion(), getUncurryLevel());
    return;

  //   global ::= 'Wv' directness entity
  case Kind::FieldOffset:
    mangler.mangleFieldOffsetFull(getDecl(), isOffsetIndirect());
    return;
      
  //   global ::= 'WP' protocol-conformance
  case Kind::DirectProtocolWitnessTable:
    buffer << "_TWP";
    mangler.mangleProtocolConformance(getProtocolConformance());
    return;

  //   global ::= 'Wa' protocol-conformance
  case Kind::ProtocolWitnessTableAccessFunction:
    buffer << "_TWa";
    mangler.mangleProtocolConformance(getProtocolConformance());
    return;

  //   global ::= 'Wl' type protocol-conformance
  case Kind::ProtocolWitnessTableLazyAccessFunction:
    buffer << "_TWl";
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    mangler.mangleProtocolConformance(getProtocolConformance());
    return;

  //   global ::= 'WL' type protocol-conformance
  case Kind::ProtocolWitnessTableLazyCacheVariable:
    buffer << "_TWL";
    mangler.mangleType(getType(), ResilienceExpansion::Minimal, 0);
    mangler.mangleProtocolConformance(getProtocolConformance());
    return;
      
  //   global ::= 'WD' protocol-conformance
  case Kind::DependentProtocolWitnessTableGenerator:
    buffer << "_TWD";
    mangler.mangleProtocolConformance(getProtocolConformance());
    return;
      
  //   global ::= 'Wd' protocol-conformance
  case Kind::DependentProtocolWitnessTableTemplate:
    buffer << "_TWd";
    mangler.mangleProtocolConformance(getProtocolConformance());
    return;

  // For all the following, this rule was imposed above:
  //   global ::= local-marker? entity            // some identifiable thing

  //   entity ::= declaration                     // other declaration
  case Kind::Function:
    // As a special case, functions can have external asm names.
    if (auto AsmA = getDecl()->getAttrs().getAttribute<SILGenNameAttr>()) {
      buffer << AsmA->Name;
      return;
    }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case Kind::Other:
    // As a special case, Clang functions and globals don't get mangled at all.
    if (auto clangDecl = getDecl()->getClangDecl()) {
      if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
        if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
          buffer << '\01' << asmLabel->getLabel();
        } else if (namedClangDecl->hasAttr<clang::OverloadableAttr>()) {
          // FIXME: When we can import C++, use Clang's mangler all the time.
          mangleClangDecl(buffer, namedClangDecl, getDecl()->getASTContext());
        } else {
          buffer << namedClangDecl->getName();
        }
        return;
      }
    }

    buffer << "_T";
    if (auto type = dyn_cast<NominalTypeDecl>(getDecl())) {
      mangler.mangleNominalType(type, getResilienceExpansion(),
                                Mangler::BindGenerics::None);
    } else if (auto ctor = dyn_cast<ConstructorDecl>(getDecl())) {
      // FIXME: Hack. LinkInfo should be able to refer to the allocating
      // constructor rather than inferring it here.
      mangler.mangleConstructorEntity(ctor, /*isAllocating=*/true,
                                      getResilienceExpansion(),
                                      getUncurryLevel());
    } else {
      mangler.mangleEntity(getDecl(), getResilienceExpansion(), getUncurryLevel());
    }
    return;

  // An Objective-C class reference;  not a swift mangling.
  case Kind::ObjCClass: {
    llvm::SmallString<64> nameBuffer;
    buffer << "OBJC_CLASS_$_" 
           << cast<ClassDecl>(getDecl())->getObjCRuntimeName(nameBuffer);
    return;
  }

  // An Objective-C metaclass reference;  not a swift mangling.
  case Kind::ObjCMetaclass: {
    llvm::SmallString<64> nameBuffer;
    buffer << "OBJC_METACLASS_$_"
           << cast<ClassDecl>(getDecl())->getObjCRuntimeName(nameBuffer);
    return;
  }

  case Kind::SILFunction:
    buffer << getSILFunction()->getName();
    return;
  case Kind::SILGlobalVariable:
    buffer << getSILGlobalVariable()->getName();
    return;
  }
  llvm_unreachable("bad entity kind!");
}
