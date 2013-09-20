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
#include "swift/SIL/Mangle.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"

using namespace swift;
using namespace Lowering;
using namespace Mangle;

static char mangleConstructorKind(SILDeclRef::Kind kind) {
  switch (kind) {
  case SILDeclRef::Kind::Allocator:
    return 'C';
  case SILDeclRef::Kind::Initializer:
    return 'c';
      
  case SILDeclRef::Kind::Func:
  case SILDeclRef::Kind::Getter:
  case SILDeclRef::Kind::Setter:
  case SILDeclRef::Kind::EnumElement:
  case SILDeclRef::Kind::Destroyer:
  case SILDeclRef::Kind::GlobalAccessor:
  case SILDeclRef::Kind::DefaultArgGenerator:
    llvm_unreachable("not a constructor kind");
  }
}

/// Mangle this entity into the given stream.
void SILGenModule::mangleConstant(SILDeclRef c, SILFunction *f) {
  llvm::raw_string_ostream buffer(f->getMutableName());
  
  Mangler mangler(buffer);

  // Almost everything below gets one of the common prefixes:
  //   mangled-name ::= '_T' global     // Native symbol
  //   mangled-name ::= '_TTo' global   // ObjC interop
  char const *introducer = c.isObjC ? "_TTo" : "_T";
  
  switch (c.kind) {
  //   entity ::= declaration                     // other declaration
  case SILDeclRef::Kind::Func:
    if (!c.hasDecl() || c.getDecl()->getDeclContext()->isLocalContext()) {
      // FIXME: Generate a more descriptive name for closures.
      buffer << "closure" << anonymousFunctionCounter++;
      return;
    }
    // As a special case, functions can have external asm names.
    if (!c.getDecl()->getAttrs().AsmName.empty()) {
      buffer << c.getDecl()->getAttrs().AsmName;
      return;
    }

    // Otherwise, fall through into the 'other decl' case.
    SWIFT_FALLTHROUGH;

  case SILDeclRef::Kind::EnumElement:
    // As a special case, Clang functions and globals don't get mangled at all.
    // FIXME: When we can import C++, use Clang's mangler.
    if (auto clangDecl = c.getDecl()->getClangDecl()) {
      if (auto namedClangDecl = dyn_cast<clang::DeclaratorDecl>(clangDecl)) {
        if (auto asmLabel = namedClangDecl->getAttr<clang::AsmLabelAttr>()) {
          buffer << '\01' << asmLabel->getLabel();
        } else {
          buffer << namedClangDecl->getName();
        }
        return;
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
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
    mangler.mangleDeclContext(cast<ClassDecl>(c.getDecl()));
    buffer << 'd';
    return;

  //   entity ::= context 'C' type                // allocating constructor
  //   entity ::= context 'c' type                // initializing constructor
  case SILDeclRef::Kind::Allocator:
  case SILDeclRef::Kind::Initializer: {
    buffer << introducer;
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
    auto ctor = cast<ConstructorDecl>(c.getDecl());
    mangler.mangleContextOf(ctor);
    buffer << mangleConstructorKind(c.kind);
    mangler.mangleDeclType(ctor, ExplosionKind::Minimal, c.uncurryLevel);
    return;
  }

  //   entity ::= declaration 'g'                 // getter
  case SILDeclRef::Kind::Getter:
    buffer << introducer;
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
    mangler.mangleEntity(c.getDecl(), ExplosionKind::Minimal, 0);
    buffer << 'g';
    return;

  //   entity ::= declaration 's'                 // setter
  case SILDeclRef::Kind::Setter:
    buffer << introducer;
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
    mangler.mangleEntity(c.getDecl(), ExplosionKind::Minimal, 0);
    buffer << 's';
    return;

  //   entity ::= declaration 'a'                 // addressor
  case SILDeclRef::Kind::GlobalAccessor:
    buffer << introducer;
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
    mangler.mangleEntity(c.getDecl(), ExplosionKind::Minimal, 0);
    buffer << 'a';
    return;

  //   entity ::= declaration 'e' index           // default arg generator
  case SILDeclRef::Kind::DefaultArgGenerator:
    buffer << introducer;
    if (f->getLinkage() == SILLinkage::Internal) buffer << 'L';
    mangler.mangleEntity(c.getDecl(), ExplosionKind::Minimal, 0);
    buffer << 'e';
    if (c.defaultArgIndex > 0)
      buffer << (c.defaultArgIndex - 1);
    buffer << '_';
    return;
  }

  llvm_unreachable("bad entity kind!");
}
