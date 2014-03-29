//===--- Attr.cpp - Swift Language Attr ASTs ------------------------------===//
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
//  This file implements routines relating to declaration attributes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Attr.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"

using namespace swift;


// Only allow allocation of attributes using the allocator in ASTContext.
void *AttributeBase::operator new(size_t Bytes, ASTContext &C,
                                  unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

/// A statically-allocated empty set of attributes.
const DeclAttributes Decl::EmptyAttrs;

DeclAttributes &Decl::getMutableAttrs() {
  // If we don't have mutable attribute storage yet, allocate some.
  if (&getAttrs() == &EmptyAttrs)
    AttrsAndIsObjC = {getASTContext().Allocate<DeclAttributes>(),
                      AttrsAndIsObjC.getInt()};
  return *const_cast<DeclAttributes*>(&getAttrs());
}

void DeclAttributes::print(llvm::raw_ostream &OS) const {
  StreamPrinter P(OS);
  PrintOptions PO = PrintOptions::printEverything();
  print(P, PO);
}

void DeclAttributes::print(ASTPrinter &Printer, 
                           const PrintOptions &Options) const {
  if (NumAttrsSet == 0 && !DeclAttrs)
    return;

  for (auto DA : *this) {
    if (Options.SkipImplicit && DA->isImplicit())
      continue;

    DA->print(Printer);
  }

  if (isAssignment())
    Printer << "@assignment ";
  if (isConversion())
    Printer << "@conversion ";
  if (isTransparent())
    Printer << "@transparent ";
  if (isInfix())
    Printer << "@infix ";
  if (isNoReturn())
    Printer << "@noreturn ";
  if (isPostfix())
    Printer << "@postfix ";
  if (requiresStoredPropertyInits())
    Printer << "@requires_stored_property_inits ";
  if (isIBOutlet())
    Printer << "@IBOutlet ";
  if (isIBAction())
    Printer << "@IBAction ";
  if (isClassProtocol())
    Printer << "@class_protocol ";
  if (isExported())
    Printer << "@exported ";
  if (isOptional())
    Printer << "@optional ";
  Optional<bool> MutatingAttr = getMutating();
  if (MutatingAttr && MutatingAttr.getValue())
    Printer << "@mutating ";
  if (MutatingAttr && !MutatingAttr.getValue())
    Printer << "@!mutating ";
  if (isRequired())
    Printer << "@required ";
  if (isOverride())
    Printer << "@override ";
}

void DeclAttribute::print(ASTPrinter &Printer) const {
  switch (getKind()) {
    case DAK_asmname:
      Printer << "@asmname(\"" << cast<AsmnameAttr>(this)->Name << "\")";
      break;
    case DAK_availability: {
      Printer << "@availability(";
      auto Attr = cast<AvailabilityAttr>(this);
      if (!Attr->hasPlatform())
        Printer << '*';
      else
        Printer << Attr->Platform;

      if (!Attr->Message.empty()) {
        Printer << ", message=\""<< Attr->Message << "\"";
      }
      Printer << ')';
      break;
    }
    case DAK_objc: { 
      Printer << "@objc";
      
      auto printId = [&](Identifier name) {
        if (name.empty())
          return;
        
        Printer << name.str();
      };

      auto Attr = cast<ObjCAttr>(this);
      switch (Attr->getKind()) {
      case ObjCAttr::Unnamed:
        break;

      case ObjCAttr::Nullary:
        Printer << "(";
        printId(Attr->getNames().front());
        Printer << ")";
        break;

      case ObjCAttr::Selector:
        Printer << "(";
        for (auto name : Attr->getNames()) {
          printId(name);
          Printer << ":";
        }
        Printer << ")";
        break;
      }
      break;
    }
    case DAK_Count:
      llvm_unreachable("exceed declaration attribute kinds");
  }
  Printer << " ";
}

void DeclAttribute::print(llvm::raw_ostream &OS) const {
  StreamPrinter P(OS);
  print(P);
}

unsigned DeclAttribute::getOptions(DeclAttrKind DK) {
  switch (DK) {
    case DAK_Count:
      llvm_unreachable("getOptions needs a valid attribute");
      break;
#define DECL_ATTR(NAME, OPTIONS)\
case DAK_##NAME: return OPTIONS;
#include "swift/AST/Attr.def"
  }
  return 0;
}

namespace {
  // Private subclass of ObjCAttr used to store location and
  // identifier information for the nullary case.
  struct NullaryObjCAttr : public ObjCAttr {
    Identifier Name;
    SourceLoc NameLoc;
    SourceRange ParenRange;

    NullaryObjCAttr(SourceLoc AtLoc, 
                    SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                    SourceLoc NameLoc, Identifier Name,
                    SourceLoc RParenLoc)
      : ObjCAttr(AtLoc, SourceRange(ObjCLoc, RParenLoc), 1, /*Implicit=*/false),
        Name(Name), NameLoc(NameLoc), ParenRange(LParenLoc, RParenLoc) { }
  };

  // Private subclass of ObjCAttr used to store the identifiers and
  // locations for the selector case.
  struct SelectorObjCAttr : public ObjCAttr {
    SourceRange ParenRange;

    SelectorObjCAttr(SourceLoc AtLoc, SourceLoc ObjCLoc, SourceLoc LParenLoc,
                     ArrayRef<SourceLoc> NameLocs, ArrayRef<Identifier> Names,
                     SourceLoc RParenLoc)
      : ObjCAttr(AtLoc, SourceRange(ObjCLoc, RParenLoc), Names.size() + 1,
                 /*Implicit=*/false),
        ParenRange(LParenLoc, RParenLoc)
    { 
      assert(Names.size() >= 1 && "No names in selector style");
      assert(NameLocs.size() == Names.size() && "# names != # locations");

      std::memcpy(getNames().data(), Names.data(), 
                  Names.size() * sizeof(Identifier));
      std::memcpy(getNameLocs().data(), NameLocs.data(),
                  NameLocs.size() * sizeof(SourceLoc));
    }

    /// Determine the number of names in this selector.
    unsigned getNumNames() const { return getArity() - 1; }

    /// Retrieve the names, which trail this record.
    MutableArrayRef<Identifier> getNames() {
      return MutableArrayRef<Identifier>(reinterpret_cast<Identifier *>(this+1),
                                         getNumNames());
    }

    /// Retrieve the names, which trail this record.
    ArrayRef<Identifier> getNames() const {
      return ArrayRef<Identifier>(reinterpret_cast<const Identifier *>(this+1),
                                  getNumNames());
    }

    /// Retrieve the name locations, which trail the names.
    MutableArrayRef<SourceLoc> getNameLocs() {
      auto afterNames = reinterpret_cast<Identifier *>(this+1) + getNumNames();
      return MutableArrayRef<SourceLoc>(
               reinterpret_cast<SourceLoc *>(afterNames),
               getNumNames());
    }

    /// Retrieve the name locations, which trail the names.
    ArrayRef<SourceLoc> getNameLocs() const {
      auto afterNames 
        = reinterpret_cast<const Identifier *>(this+1) + getNumNames();
      return ArrayRef<SourceLoc>(
               reinterpret_cast<const SourceLoc *>(afterNames),
               getNumNames());
    }
  };
}

ObjCAttr *ObjCAttr::createUnnamed(ASTContext &Ctx, SourceLoc AtLoc, 
                                  SourceLoc ObjCLoc) {
  return new (Ctx) ObjCAttr(AtLoc, SourceRange(ObjCLoc), /*Arity=*/0,
                            /*Implicit=*/false);
}

ObjCAttr *ObjCAttr::createNullary(ASTContext &Ctx, SourceLoc AtLoc, 
                                  SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                  SourceLoc NameLoc, Identifier Name,
                                  SourceLoc RParenLoc) {
  return new (Ctx) NullaryObjCAttr(AtLoc, ObjCLoc, LParenLoc, NameLoc, Name,
                                   RParenLoc);
}

ObjCAttr *ObjCAttr::createSelector(ASTContext &Ctx, SourceLoc AtLoc, 
                                   SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                   ArrayRef<SourceLoc> NameLocs,
                                   ArrayRef<Identifier> Names,
                                   SourceLoc RParenLoc) {
  unsigned size = sizeof(SelectorObjCAttr) 
                + Names.size() * sizeof(Identifier)
                + NameLocs.size() * sizeof(SourceLoc);
  void *mem = Ctx.Allocate(size, alignof(SelectorObjCAttr));
  return new (mem) SelectorObjCAttr(AtLoc, ObjCLoc, LParenLoc, NameLocs, Names,
                                    RParenLoc);
}

ArrayRef<Identifier> ObjCAttr::getNames() const {
  switch (getKind()) {
  case Unnamed:
    return { };

  case Nullary:
    return static_cast<const NullaryObjCAttr *>(this)->Name;

  case Selector:
    return static_cast<const SelectorObjCAttr *>(this)->getNames();
  }
}

ArrayRef<SourceLoc> ObjCAttr::getNameLocs() const {
  switch (getKind()) {
  case Unnamed:
    return { };

  case Nullary:
    return static_cast<const NullaryObjCAttr *>(this)->NameLoc;

  case Selector:
    return static_cast<const SelectorObjCAttr *>(this)->getNameLocs();
  }
}

SourceLoc ObjCAttr::getLParenLoc() const {
  switch (getKind()) {
  case Unnamed:
    return SourceLoc();

  case Nullary:
    return static_cast<const NullaryObjCAttr *>(this)->ParenRange.Start;

  case Selector:
    return static_cast<const SelectorObjCAttr *>(this)->ParenRange.Start;
  }
}

SourceLoc ObjCAttr::getRParenLoc() const {
  switch (getKind()) {
  case Unnamed:
    return SourceLoc();

  case Nullary:
    return static_cast<const NullaryObjCAttr *>(this)->ParenRange.End;

  case Selector:
    return static_cast<const SelectorObjCAttr *>(this)->ParenRange.End;
  }
}

