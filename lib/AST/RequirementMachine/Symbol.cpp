//===--- Symbol.cpp - The generics rewrite system alphabet  ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <vector>
#include "ProtocolGraph.h"
#include "RewriteContext.h"
#include "Symbol.h"
#include "Term.h"

using namespace swift;
using namespace rewriting;

const StringRef Symbol::Kinds[] = {
  "protocol",
  "assocty",
  "generic",
  "name",
  "layout",
  "super",
  "concrete"
};

/// Symbols are uniqued and immutable, stored as a single pointer;
/// the Storage type is the allocated backing storage.
struct Symbol::Storage final
  : public llvm::FoldingSetNode,
    public llvm::TrailingObjects<Storage, const ProtocolDecl *, Term> {
  friend class Symbol;

  unsigned Kind : 3;
  unsigned NumProtocols : 15;
  unsigned NumSubstitutions : 14;

  union {
    Identifier Name;
    CanType ConcreteType;
    LayoutConstraint Layout;
    const ProtocolDecl *Proto;
    GenericTypeParamType *GenericParam;
  };

  explicit Storage(Identifier name) {
    Kind = unsigned(Symbol::Kind::Name);
    NumProtocols = 0;
    NumSubstitutions = 0;
    Name = name;
  }

  explicit Storage(LayoutConstraint layout) {
    Kind = unsigned(Symbol::Kind::Layout);
    NumProtocols = 0;
    NumSubstitutions = 0;
    Layout = layout;
  }

  explicit Storage(const ProtocolDecl *proto) {
    Kind = unsigned(Symbol::Kind::Protocol);
    NumProtocols = 0;
    NumSubstitutions = 0;
    Proto = proto;
  }

  explicit Storage(GenericTypeParamType *param) {
    Kind = unsigned(Symbol::Kind::GenericParam);
    NumProtocols = 0;
    NumSubstitutions = 0;
    GenericParam = param;
  }

  Storage(ArrayRef<const ProtocolDecl *> protos, Identifier name) {
    assert(!protos.empty());

    Kind = unsigned(Symbol::Kind::AssociatedType);
    NumProtocols = protos.size();
    assert(NumProtocols == protos.size() && "Overflow");
    NumSubstitutions = 0;
    Name = name;

    for (unsigned i : indices(protos))
      getProtocols()[i] = protos[i];
  }

  Storage(Symbol::Kind kind, CanType type, ArrayRef<Term> substitutions) {
    assert(kind == Symbol::Kind::Superclass ||
           kind == Symbol::Kind::ConcreteType);
    assert(!type->hasTypeVariable());
    assert(type->hasTypeParameter() != substitutions.empty());

    Kind = unsigned(kind);
    NumProtocols = 0;
    NumSubstitutions = substitutions.size();
    ConcreteType = type;

    for (unsigned i : indices(substitutions))
      getSubstitutions()[i] = substitutions[i];
  }

  size_t numTrailingObjects(OverloadToken<const ProtocolDecl *>) const {
    return NumProtocols;
  }

  size_t numTrailingObjects(OverloadToken<Term>) const {
    return NumSubstitutions;
  }

  MutableArrayRef<const ProtocolDecl *> getProtocols() {
    return {getTrailingObjects<const ProtocolDecl *>(), NumProtocols};
  }

  ArrayRef<const ProtocolDecl *> getProtocols() const {
    return {getTrailingObjects<const ProtocolDecl *>(), NumProtocols};
  }

  MutableArrayRef<Term> getSubstitutions() {
    return {getTrailingObjects<Term>(), NumSubstitutions};
  }

  ArrayRef<Term> getSubstitutions() const {
    return {getTrailingObjects<Term>(), NumSubstitutions};
  }

  void Profile(llvm::FoldingSetNodeID &id) const;
};

Symbol::Kind Symbol::getKind() const {
  return Kind(Ptr->Kind);
}

/// Get the identifier associated with an unbound name symbol or an
/// associated type symbol.
Identifier Symbol::getName() const {
  assert(getKind() == Kind::Name ||
         getKind() == Kind::AssociatedType);
  return Ptr->Name;
}

/// Get the single protocol declaration associated with a protocol symbol.
const ProtocolDecl *Symbol::getProtocol() const {
  assert(getKind() == Kind::Protocol);
  return Ptr->Proto;
}

/// Get the list of protocols associated with a protocol or associated type
/// symbol. Note that if this is a protocol symbol, the return value will have
/// exactly one element.
ArrayRef<const ProtocolDecl *> Symbol::getProtocols() const {
  auto protos = Ptr->getProtocols();
  if (protos.empty()) {
    assert(getKind() == Kind::Protocol);
    return {&Ptr->Proto, 1};
  }
  assert(getKind() == Kind::AssociatedType);
  return protos;
}

/// Get the generic parameter associated with a generic parameter symbol.
GenericTypeParamType *Symbol::getGenericParam() const {
  assert(getKind() == Kind::GenericParam);
  return Ptr->GenericParam;
}

/// Get the layout constraint associated with a layout constraint symbol.
LayoutConstraint Symbol::getLayoutConstraint() const {
  assert(getKind() == Kind::Layout);
  return Ptr->Layout;
}

/// Get the superclass type associated with a superclass symbol.
CanType Symbol::getSuperclass() const {
  assert(getKind() == Kind::Superclass);
  return Ptr->ConcreteType;
}

/// Get the concrete type associated with a concrete type symbol.
CanType Symbol::getConcreteType() const {
  assert(getKind() == Kind::ConcreteType);
  return Ptr->ConcreteType;
}

ArrayRef<Term> Symbol::getSubstitutions() const {
  assert(getKind() == Kind::Superclass ||
         getKind() == Kind::ConcreteType);
  return Ptr->getSubstitutions();
}

/// Creates a new name symbol.
Symbol Symbol::forName(Identifier name,
                   RewriteContext &ctx) {
  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::Name));
  id.AddPointer(name.get());

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(0, 0);
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(name);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::Name));

  return symbol;
}

/// Creates a new protocol symbol.
Symbol Symbol::forProtocol(const ProtocolDecl *proto,
                           RewriteContext &ctx) {
  assert(proto != nullptr);

  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::Protocol));
  id.AddPointer(proto);

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(0, 0);
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(proto);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::Protocol));

  return symbol;
}

/// Creates a new associated type symbol for a single protocol.
Symbol Symbol::forAssociatedType(const ProtocolDecl *proto,
                                 Identifier name,
                                 RewriteContext &ctx) {
  SmallVector<const ProtocolDecl *, 1> protos;
  protos.push_back(proto);

  return forAssociatedType(protos, name, ctx);
}

/// Creates a merged associated type symbol to represent a nested
/// type that conforms to multiple protocols, all of which have
/// an associated type with the same name.
Symbol Symbol::forAssociatedType(ArrayRef<const ProtocolDecl *> protos,
                                 Identifier name,
                                 RewriteContext &ctx) {
  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::AssociatedType));
  id.AddInteger(protos.size());
  for (const auto *proto : protos)
    id.AddPointer(proto);
  id.AddPointer(name.get());

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(
      protos.size(), 0);
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(protos, name);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::AssociatedType));

  return symbol;
}

/// Creates a generic parameter symbol, representing a generic
/// parameter in the top-level generic signature from which the
/// rewrite system is built.
Symbol Symbol::forGenericParam(GenericTypeParamType *param,
                               RewriteContext &ctx) {
  assert(param->isCanonical());

  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::GenericParam));
  id.AddPointer(param);

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(0, 0);
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(param);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::GenericParam));

  return symbol;
}

/// Creates a layout symbol, representing a layout constraint.
Symbol Symbol::forLayout(LayoutConstraint layout,
                         RewriteContext &ctx) {
  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::Layout));
  id.AddPointer(layout.getPointer());

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(0, 0);
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(layout);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::Layout));

  return symbol;
}

/// Creates a superclass symbol, representing a superclass constraint.
Symbol Symbol::forSuperclass(CanType type, ArrayRef<Term> substitutions,
                             RewriteContext &ctx) {
  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::Superclass));
  id.AddPointer(type.getPointer());
  id.AddInteger(unsigned(substitutions.size()));

  for (auto substitution : substitutions)
    id.AddPointer(substitution.getOpaquePointer());

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(
      0, substitutions.size());
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(Kind::Superclass, type, substitutions);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::Superclass));

  return symbol;
}

/// Creates a concrete type symbol, representing a superclass constraint.
Symbol Symbol::forConcreteType(CanType type, ArrayRef<Term> substitutions,
                               RewriteContext &ctx) {
  llvm::FoldingSetNodeID id;
  id.AddInteger(unsigned(Kind::ConcreteType));
  id.AddPointer(type.getPointer());
  id.AddInteger(unsigned(substitutions.size()));
  for (auto substitution : substitutions)
    id.AddPointer(substitution.getOpaquePointer());

  void *insertPos = nullptr;
  if (auto *symbol = ctx.Symbols.FindNodeOrInsertPos(id, insertPos))
    return symbol;

  unsigned size = Storage::totalSizeToAlloc<const ProtocolDecl *, Term>(
      0, substitutions.size());
  void *mem = ctx.Allocator.Allocate(size, alignof(Storage));
  auto *symbol = new (mem) Storage(Kind::ConcreteType, type, substitutions);

#ifndef NDEBUG
  llvm::FoldingSetNodeID newID;
  symbol->Profile(newID);
  assert(id == newID);
#endif

  ctx.Symbols.InsertNode(symbol, insertPos);
  ctx.SymbolHistogram.add(unsigned(Kind::ConcreteType));

  return symbol;
}

/// Given that this symbol is the first symbol of a term, return the
/// "domain" of the term.
///
/// - If the first symbol is a protocol symbol [P], the domain is P.
/// - If the first symbol is an associated type symbol [P1&...&Pn],
///   the domain is {P1, ..., Pn}.
/// - If the first symbol is a generic parameter symbol, the domain is
///   the empty set {}.
/// - Anything else will assert.
ArrayRef<const ProtocolDecl *> Symbol::getRootProtocols() const {
  switch (getKind()) {
  case Symbol::Kind::Protocol:
  case Symbol::Kind::AssociatedType:
    return getProtocols();

  case Symbol::Kind::GenericParam:
    return ArrayRef<const ProtocolDecl *>();

  case Symbol::Kind::Name:
  case Symbol::Kind::Layout:
  case Symbol::Kind::Superclass:
  case Symbol::Kind::ConcreteType:
    break;
  }

  llvm_unreachable("Bad root symbol");
}

/// Linear order on symbols.
///
/// First, we order different kinds as follows, from smallest to largest:
///
/// - Protocol
/// - AssociatedType
/// - GenericParam
/// - Name
/// - Layout
/// - Superclass
/// - ConcreteType
///
/// Then we break ties when both symbols have the same kind as follows:
///
/// * For associated type symbols, we first order the number of protocols,
///   with symbols containing more protocols coming first. This ensures
///   that the following holds:
///
///     [P1&P2:T] < [P1:T]
///     [P1&P2:T] < [P2:T]
///
///   If both symbols have the same number of protocols, we perform a
///   lexicographic comparison on the protocols pair-wise, using the
///   protocol order defined by \p graph (see
///   ProtocolGraph::compareProtocols()).
///
/// * For generic parameter symbols, we first order by depth, then index.
///
/// * For unbound name symbols, we compare identifiers lexicographically.
///
/// * For protocol symbols, we compare the protocols using the protocol
///   linear order on \p graph.
///
/// * For layout symbols, we use LayoutConstraint::compare().
int Symbol::compare(Symbol other, const ProtocolGraph &graph) const {
  // Exit early if the symbols are equal.
  if (Ptr == other.Ptr)
    return 0;

  auto kind = getKind();
  auto otherKind = other.getKind();

  if (kind != otherKind)
    return int(kind) < int(otherKind) ? -1 : 1;

  int result = 0;

  switch (kind) {
  case Kind::Name:
    result = getName().compare(other.getName());
    break;

  case Kind::Protocol:
    result = graph.compareProtocols(getProtocol(), other.getProtocol());
    break;

  case Kind::AssociatedType: {
    auto protos = getProtocols();
    auto otherProtos = other.getProtocols();

    if (getName() != other.getName())
      return getName().compare(other.getName());

    // Symbols with more protocols are 'smaller' than those with fewer.
    if (protos.size() != otherProtos.size())
      return protos.size() > otherProtos.size() ? -1 : 1;

    for (unsigned i : indices(protos)) {
      int result = graph.compareProtocols(protos[i], otherProtos[i]);
      if (result)
        return result;
    }

    break;
  }

  case Kind::GenericParam: {
    auto *param = getGenericParam();
    auto *otherParam = other.getGenericParam();

    if (param->getDepth() != otherParam->getDepth())
      return param->getDepth() < otherParam->getDepth() ? -1 : 1;

    if (param->getIndex() != otherParam->getIndex())
      return param->getIndex() < otherParam->getIndex() ? -1 : 1;

    break;
  }

  case Kind::Layout:
    result = getLayoutConstraint().compare(other.getLayoutConstraint());
    break;

  case Kind::Superclass:
  case Kind::ConcreteType: {
    assert(false && "Cannot compare concrete types yet");
    break;
  }
  }

  assert(result != 0 && "Two distinct symbols should not compare equal");
  return result;
}

/// For a superclass or concrete type symbol
///
///   [concrete: Foo<X1, ..., Xn>]
///   [superclass: Foo<X1, ..., Xn>]
///
/// Return a new symbol where the function fn is applied to each of the
/// substitutions:
///
///   [concrete: Foo<fn(X1), ..., fn(Xn)>]
///   [superclass: Foo<fn(X1), ..., fn(Xn)>]
///
/// Asserts if this is not a superclass or concrete type symbol.
Symbol Symbol::transformConcreteSubstitutions(
    llvm::function_ref<Term(Term)> fn,
    RewriteContext &ctx) const {
  assert(isSuperclassOrConcreteType());

  if (getSubstitutions().empty())
    return *this;

  bool anyChanged = false;
  SmallVector<Term, 2> substitutions;
  for (auto term : getSubstitutions()) {
    auto newTerm = fn(term);
    if (newTerm != term)
      anyChanged = true;

    substitutions.push_back(newTerm);
  }

  if (!anyChanged)
    return *this;

  switch (getKind()) {
  case Kind::Superclass:
    return Symbol::forSuperclass(getSuperclass(), substitutions, ctx);
  case Kind::ConcreteType:
    return Symbol::forConcreteType(getConcreteType(), substitutions, ctx);

  case Kind::GenericParam:
  case Kind::Name:
  case Kind::Protocol:
  case Kind::AssociatedType:
  case Kind::Layout:
    break;
  }

  llvm_unreachable("Bad symbol kind");
}

/// Print the symbol using our mnemonic representation.
void Symbol::dump(llvm::raw_ostream &out) const {
  auto dumpSubstitutions = [&]() {
    if (getSubstitutions().size() > 0) {
      out << " with <";

      bool first = true;
      for (auto substitution : getSubstitutions()) {
        if (first) {
          first = false;
        } else {
          out << ", ";
        }
        substitution.dump(out);
      }

      out << ">";
    }
  };

  switch (getKind()) {
  case Kind::Name:
    out << getName();
    return;

  case Kind::Protocol:
    out << "[" << getProtocol()->getName() << "]";
    return;

  case Kind::AssociatedType: {
    out << "[";
    bool first = true;
    for (const auto *proto : getProtocols()) {
      if (first) {
        first = false;
      } else {
        out << "&";
      }
      out << proto->getName();
    }
    out << ":" << getName() << "]";
    return;
  }

  case Kind::GenericParam:
    out << Type(getGenericParam());
    return;

  case Kind::Layout:
    out << "[layout: ";
    getLayoutConstraint()->print(out);
    out << "]";
    return;

  case Kind::Superclass:
    out << "[superclass: " << getSuperclass();
    dumpSubstitutions();
    out << "]";
    return;

  case Kind::ConcreteType:
    out << "[concrete: " << getConcreteType();
    dumpSubstitutions();
    out << "]";
    return;
  }

  llvm_unreachable("Bad symbol kind");
}

void Symbol::Storage::Profile(llvm::FoldingSetNodeID &id) const {
  id.AddInteger(Kind);

  switch (Symbol::Kind(Kind)) {
  case Symbol::Kind::Name:
    id.AddPointer(Name.get());
    return;

  case Symbol::Kind::Layout:
    id.AddPointer(Layout.getPointer());
    return;

  case Symbol::Kind::Protocol:
    id.AddPointer(Proto);
    return;

  case Symbol::Kind::GenericParam:
    id.AddPointer(GenericParam);
    return;

  case Symbol::Kind::AssociatedType: {
    auto protos = getProtocols();
    id.AddInteger(protos.size());

    for (const auto *proto : protos)
      id.AddPointer(proto);

    id.AddPointer(Name.get());
    return;
  }

  case Symbol::Kind::Superclass:
  case Symbol::Kind::ConcreteType: {
    id.AddPointer(ConcreteType.getPointer());

    id.AddInteger(NumSubstitutions);
    for (auto term : getSubstitutions())
      id.AddPointer(term.getOpaquePointer());

    return;
  }
  }

  llvm_unreachable("Bad symbol kind");
}