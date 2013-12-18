//===--- Mangle.cpp - Symbol mangling of SILDeclRefs -----------*- C++ -*-===//
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

#include "SILGen.h"
#include "swift/AST/Mangle.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;
using namespace Lowering;
using namespace Mangle;

SILFunction *
SILGenModule::getOrCreateReabstractionThunk(SILLocation loc,
                                            CanSILFunctionType thunkType,
                                            CanSILFunctionType fromType,
                                            CanSILFunctionType toType) {
  // Mangle the reabstraction thunk.
  llvm::SmallString<256> buffer;
  {
    llvm::raw_svector_ostream stream(buffer);
    Mangler mangler(stream);

    // This is actually the SIL helper function.  For now, IR-gen
    // makes the actual thunk.
    stream << "_TTR";
    if (auto generics = thunkType->getGenericParams()) {
      stream << 'G';
      mangler.bindGenericParameters(generics, /*mangle*/ true);
    }
    mangler.mangleType(fromType, ExplosionKind::Minimal, /*uncurry*/ 0);
    mangler.mangleType(toType, ExplosionKind::Minimal, /*uncurry*/ 0);
  }

  return M.getOrCreateSharedFunction(loc, buffer.str(), thunkType,
                                     IsBare, IsTransparent);
}

/// Mangle this entity into the given stream.
void SILGenModule::mangleConstant(SILDeclRef c, SILFunction *f) {
  llvm::raw_string_ostream buffer(f->getMutableName());
  
  Mangler mangler(buffer);

  // Almost everything below gets one of the common prefixes:
  //   mangled-name ::= '_T' global     // Native symbol
  //   mangled-name ::= '_TTo' global   // ObjC interop thunk
  //   mangled-name ::= '_TTO' global   // Foreign function thunk
  char const *introducer = "_T";
  if (c.isForeign)
    introducer = "_TTo";
  else if (c.isForeignThunk())
    introducer = "_TTO";
  
  switch (c.kind) {
  //   entity ::= declaration                     // other declaration
  case SILDeclRef::Kind::Func:
    if (!c.hasDecl() || c.getDecl()->getDeclContext()->isLocalContext()) {
      // FIXME: Generate a more descriptive name for closures.
      buffer << "closure" << anonymousSymbolCounter++;
      return;
    }
    // As a special case, functions can have external asm names.
    // Use the asm name only for the original non-thunked, non-curried entry
    // point.
    if (!c.getDecl()->getAttrs().AsmName.empty()
        && !c.isForeignThunk() && !c.isCurried) {
      buffer << c.getDecl()->getAttrs().AsmName;
      return;
    }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case SILDeclRef::Kind::EnumElement:
    // As a special case, Clang functions and globals don't get mangled at all.
    // FIXME: When we can import C++, use Clang's mangler.
    if (auto clangDecl = c.getDecl()->getClangDecl()) {
      if (!c.isForeignThunk() && !c.isCurried) {
        if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
          if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
            buffer << '\01' << asmLabel->getLabel();
          } else {
            buffer << namedClangDecl->getName();
          }
          return;
        }
      }
    }

    buffer << introducer;
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
      
    mangler.mangleEntity(c.getDecl(), ExplosionKind::Minimal,
                         c.uncurryLevel);
    return;
      
  //   entity ::= context 'D'                     // deallocating destructor
  //   entity ::= context 'd'                     // destroying destructor
  // FIXME: Only the destroying destructor is currently emitted in SIL.
  case SILDeclRef::Kind::Destroyer:
    buffer << introducer;
    mangler.mangleDestructorEntity(cast<ClassDecl>(c.getDecl()),
                                   /*isDeallocating*/ false);
    return;

  //   entity ::= context 'C' type                // allocating constructor
  case SILDeclRef::Kind::Allocator:
    buffer << introducer;
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ true,
                                    ExplosionKind::Minimal,
                                    c.uncurryLevel);
    return;

  //   entity ::= context 'c' type                // initializing constructor
  case SILDeclRef::Kind::Initializer:
    buffer << introducer;
    mangler.mangleConstructorEntity(cast<ConstructorDecl>(c.getDecl()),
                                    /*allocating*/ false,
                                    ExplosionKind::Minimal,
                                    c.uncurryLevel);
    return;

  //   entity ::= declaration 'g'                 // getter
  case SILDeclRef::Kind::Getter:
    if (!c.hasDecl() || c.getDecl()->getDeclContext()->isLocalContext()) {
      // FIXME: Generate a more descriptive name for getters.
      buffer << "closure" << anonymousSymbolCounter++;
      return;
    }

    buffer << introducer;
    mangler.mangleGetterEntity(c.getDecl(), ExplosionKind::Minimal);
    return;

  //   entity ::= declaration 's'                 // setter
  case SILDeclRef::Kind::Setter:
    if (!c.hasDecl() || c.getDecl()->getDeclContext()->isLocalContext()) {
      // FIXME: Generate a more descriptive name for setters.
      buffer << "closure" << anonymousSymbolCounter++;
      return;
    }

    buffer << introducer;
    mangler.mangleSetterEntity(c.getDecl(), ExplosionKind::Minimal);
    return;

  //   entity ::= declaration 'a'                 // addressor
  case SILDeclRef::Kind::GlobalAccessor:
    buffer << introducer;
    mangler.mangleAddressorEntity(c.getDecl());
    return;

  //   entity ::= context 'e' index           // default arg generator
  case SILDeclRef::Kind::DefaultArgGenerator:
    buffer << introducer;
    mangler.mangleDefaultArgumentEntity(cast<AbstractFunctionDecl>(c.getDecl()),
                                        c.defaultArgIndex);
    return;
  }

  llvm_unreachable("bad entity kind!");
}
