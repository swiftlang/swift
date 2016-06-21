//===--- Linking.cpp - Name mangling for IRGen entities -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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

/// Mangle this entity as a std::string.
std::string LinkEntity::mangleAsString() const {
  std::string result; {
    llvm::raw_string_ostream stream(result);
    mangle(stream);
  }
  return result;
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
  Mangler mangler;
  switch (getKind()) {
  //   global ::= 'w' value-witness-kind type     // value witness
  case Kind::ValueWitness:
    mangler.append("_Tw");
    mangler.append(mangleValueWitness(getValueWitness()));
    mangler.mangleType(getType(), 0);
    return mangler.finalize(buffer);

  //   global ::= 'WV' type                       // value witness
  case Kind::ValueWitnessTable:
    mangler.append("_TWV");
    mangler.mangleType(getType(), 0);
    return mangler.finalize(buffer);

  //   global ::= 't' type
  // Abstract type manglings just follow <type>.
  case Kind::TypeMangling:
    mangler.mangleType(getType(), 0);
    return mangler.finalize(buffer);

  //   global ::= 'Ma' type               // type metadata access function
  case Kind::TypeMetadataAccessFunction:
    mangler.append("_TMa");
    mangler.mangleType(getType(), 0);
    return mangler.finalize(buffer);

  //   global ::= 'ML' type               // type metadata lazy cache variable
  case Kind::TypeMetadataLazyCacheVariable:
    mangler.append("_TML");
    mangler.mangleType(getType(), 0);
    return mangler.finalize(buffer);

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
    return mangler.finalize(buffer);

  //   global ::= 'M' directness type             // type metadata
  case Kind::ForeignTypeMetadataCandidate:
    mangler.mangleTypeMetadataFull(getType(), /*isPattern=*/false);
    return mangler.finalize(buffer);

  //   global ::= 'Mm' type                       // class metaclass
  case Kind::SwiftMetaclassStub:
    mangler.append("_TMm");
    mangler.mangleNominalType(cast<ClassDecl>(getDecl()));
    return mangler.finalize(buffer);

  //   global ::= 'Mn' type                       // nominal type descriptor
  case Kind::NominalTypeDescriptor:
    mangler.append("_TMn");
    mangler.mangleNominalType(cast<NominalTypeDecl>(getDecl()));
    return mangler.finalize(buffer);

  //   global ::= 'Mp' type                       // protocol descriptor
  case Kind::ProtocolDescriptor:
    mangler.append("_TMp");
    mangler.mangleProtocolName(cast<ProtocolDecl>(getDecl()));
    return mangler.finalize(buffer);

  //   global ::= 'Wo' entity
  case Kind::WitnessTableOffset:
     mangler.append("_TWo");

    // Witness table entries for constructors always refer to the allocating
    // constructor.
    if (auto ctor = dyn_cast<ConstructorDecl>(getDecl()))
      mangler.mangleConstructorEntity(ctor, /*isAllocating=*/true,
                                      getUncurryLevel());
    else
      mangler.mangleEntity(getDecl(), getUncurryLevel());
    return mangler.finalize(buffer);

  //   global ::= 'Wv' directness entity
  case Kind::FieldOffset:
    mangler.mangleFieldOffsetFull(getDecl(), isOffsetIndirect());
    return mangler.finalize(buffer);

  //   global ::= 'WP' protocol-conformance
  case Kind::DirectProtocolWitnessTable:
    mangler.append("_TWP");
    mangler.mangleProtocolConformance(getProtocolConformance());
    return mangler.finalize(buffer);

  //   global ::= 'WG' protocol-conformance
  case Kind::GenericProtocolWitnessTableCache:
    buffer << "_TWG";
    mangler.mangleProtocolConformance(getProtocolConformance());
    return mangler.finalize(buffer);

  //   global ::= 'WI' protocol-conformance
  case Kind::GenericProtocolWitnessTableInstantiationFunction:
    buffer << "_TWI";
    mangler.mangleProtocolConformance(getProtocolConformance());
    return mangler.finalize(buffer);

  //   global ::= 'Wa' protocol-conformance
  case Kind::ProtocolWitnessTableAccessFunction:
    mangler.append("_TWa");
    mangler.mangleProtocolConformance(getProtocolConformance());
    return mangler.finalize(buffer);

  //   global ::= 'Wl' type protocol-conformance
  case Kind::ProtocolWitnessTableLazyAccessFunction:
    mangler.append("_TWl");
    mangler.mangleType(getType(), 0);
    mangler.mangleProtocolConformance(getProtocolConformance());
    return mangler.finalize(buffer);

  //   global ::= 'WL' type protocol-conformance
  case Kind::ProtocolWitnessTableLazyCacheVariable:
    mangler.append("_TWL");
    mangler.mangleType(getType(), 0);
    mangler.mangleProtocolConformance(getProtocolConformance());
    return mangler.finalize(buffer);
      
  //   global ::= 'Wt' protocol-conformance identifier
  case Kind::AssociatedTypeMetadataAccessFunction:
    mangler.append("_TWt");
    mangler.mangleProtocolConformance(getProtocolConformance());
    mangler.mangleIdentifier(getAssociatedType()->getNameStr());
    return mangler.finalize(buffer);

  //   global ::= 'WT' protocol-conformance identifier nominal-type
  case Kind::AssociatedTypeWitnessTableAccessFunction:
    mangler.append("_TWT");
    mangler.mangleProtocolConformance(getProtocolConformance());
    mangler.mangleIdentifier(getAssociatedType()->getNameStr());
    mangler.mangleProtocolDecl(getAssociatedProtocol());
    return mangler.finalize(buffer);

  // For all the following, this rule was imposed above:
  //   global ::= local-marker? entity            // some identifiable thing

  //   entity ::= declaration                     // other declaration
  case Kind::Function:
    // As a special case, functions can have manually mangled names.
    if (auto AsmA = getDecl()->getAttrs().getAttribute<SILGenNameAttr>()) {
      mangler.append(AsmA->Name);
      return mangler.finalize(buffer);
    }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case Kind::Other:
    // As a special case, Clang functions and globals don't get mangled at all.
    if (auto clangDecl = getDecl()->getClangDecl()) {
      if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
        if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
          mangler.append('\01');
          mangler.append(asmLabel->getLabel());
        } else if (namedClangDecl->hasAttr<clang::OverloadableAttr>()) {
          // FIXME: When we can import C++, use Clang's mangler all the time.
          std::string storage;
          llvm::raw_string_ostream SS(storage);
          mangleClangDecl(SS, namedClangDecl, getDecl()->getASTContext());
          mangler.append(SS.str());
        } else {
          mangler.append(namedClangDecl->getName());
        }
        return mangler.finalize(buffer);
      }
    }

    mangler.append("_T");
    if (auto type = dyn_cast<NominalTypeDecl>(getDecl())) {
      mangler.mangleNominalType(type);
    } else if (auto ctor = dyn_cast<ConstructorDecl>(getDecl())) {
      // FIXME: Hack. LinkInfo should be able to refer to the allocating
      // constructor rather than inferring it here.
      mangler.mangleConstructorEntity(ctor, /*isAllocating=*/true,
                                      getUncurryLevel());
    } else {
      mangler.mangleEntity(getDecl(), getUncurryLevel());
    }
      return mangler.finalize(buffer);

  // An Objective-C class reference reference. The symbol is private, so
  // the mangling is unimportant; it should just be readable in LLVM IR.
  case Kind::ObjCClassRef: {
    mangler.append("OBJC_CLASS_REF_$_");
    llvm::SmallString<64> tempBuffer;
    StringRef name = cast<ClassDecl>(getDecl())->getObjCRuntimeName(tempBuffer);
    mangler.append(name);
    return mangler.finalize(buffer);
  }

  // An Objective-C class reference;  not a swift mangling.
  case Kind::ObjCClass: {
    llvm::SmallString<64> TempBuffer;
    mangler.append("OBJC_CLASS_$_");
    StringRef Name = cast<ClassDecl>(getDecl())->getObjCRuntimeName(TempBuffer);
    mangler.append(Name);
    return mangler.finalize(buffer);
  }

  // An Objective-C metaclass reference;  not a swift mangling.
  case Kind::ObjCMetaclass: {
    llvm::SmallString<64> TempBuffer;
    mangler.append("OBJC_METACLASS_$_");
    StringRef Name = cast<ClassDecl>(getDecl())->getObjCRuntimeName(TempBuffer);
    mangler.append(Name);
    return mangler.finalize(buffer);
  }

  case Kind::SILFunction:
    mangler.appendSymbol(getSILFunction()->getName());
    return mangler.finalize(buffer);
  case Kind::SILGlobalVariable:
    mangler.appendSymbol(getSILGlobalVariable()->getName());
    return mangler.finalize(buffer);
  }
  llvm_unreachable("bad entity kind!");
}
