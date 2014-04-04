//===--- Attr.h - Swift Language Attribute ASTs -----------------*- C++ -*-===//
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
// This file defines classes related to declaration attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ATTR_H
#define SWIFT_ATTR_H

#include "swift/Basic/Optional.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Ownership.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {
class ASTPrinter;
class ASTContext;
struct PrintOptions;

/// The associativity of a binary operator.
enum class Associativity {
  /// Non-associative operators cannot be written next to other
  /// operators with the same precedence.  Relational operators are
  /// typically non-associative.
  None,

  /// Left-associative operators associate to the left if written next
  /// to other left-associative operators of the same precedence.
  Left,

  /// Right-associative operators associate to the right if written
  /// next to other right-associative operators of the same precedence.
  Right
};

class InfixData {
  unsigned Precedence : 8;

  /// Zero if invalid, or else an Associativity+1.
  unsigned InvalidOrAssoc : 8;
public:
  InfixData() : Precedence(0), InvalidOrAssoc(0) {}
  InfixData(unsigned char prec, Associativity assoc)
    : Precedence(prec), InvalidOrAssoc(unsigned(assoc) + 1) {}

  bool isValid() const { return InvalidOrAssoc != 0; }

  Associativity getAssociativity() const {
    assert(isValid());
    return Associativity(InvalidOrAssoc - 1);
  }
  bool isLeftAssociative() const {
    return getAssociativity() == Associativity::Left;
  }
  bool isRightAssociative() const {
    return getAssociativity() == Associativity::Right;
  }
  bool isNonAssociative() const {
    return getAssociativity() == Associativity::None;
  }

  unsigned getPrecedence() const {
    assert(isValid());
    return Precedence;
  }

  friend bool operator==(InfixData L, InfixData R) {
    return L.Precedence == R.Precedence
        && L.InvalidOrAssoc == R.InvalidOrAssoc;
  }
  friend bool operator!=(InfixData L, InfixData R) {
    return !operator==(L, R);
  }
};

/// ABI resilience.  Language structures are resilient if the details
/// of their implementation may be changed without requiring
/// associated code to be reprocessed.  Different structures are resilient
/// in different ways.  For example:
///   - A resilient type does not have a statically fixed size or layout.
///   - A resilient variable must be accessed with getters and setters, even if
///     none are defined for it now.
///   - A resilient function may not be inlined.
///
/// In general, resilience is inherited from the lexical context.  For
/// example, a variable declared in a fragile struct is implicitly fragile.
///
/// Some language structures, like tuples, are never themselves
/// resilient (although they may be defined in terms of resilient
/// types).  Additionally, code distributed with the component
/// defining a resilient structure need not actually use resilience
/// boundaries.
enum class Resilience : unsigned char {
  Default,
  
  /// Inherently fragile language structures are not only resilient,
  /// but they have never been exposed as resilient.  This permits
  /// certain kinds of optimizations that are not otherwise possible
  /// because of the need for backward compatibility.
  InherentlyFragile,

  /// Fragile language structures are non-resilient.  They may have
  /// been resilient at some point in the past, however.
  Fragile,

  /// Everything else is resilient.  Resilience means different things
  /// on different kinds of objects.
  Resilient
};

  
enum class AbstractCC : unsigned char;

// Define enumerators for each attribute, e.g. AK_weak.
enum AttrKind {
#define ATTR(X) AK_##X,
#include "swift/AST/Attr.def"
  AK_Count
};

// FIXME: DeclAttrKind and AttrKind should eventually be merged, but
// there is currently a representational difference as one set of
// attributes is migrated from one implementation to another.
enum DeclAttrKind : unsigned {
#define DECL_ATTR(NAME, ...) DAK_##NAME,
#include "swift/AST/Attr.def"
  DAK_Count
};

// Define enumerators for each type attribute, e.g. TAK_weak.
enum TypeAttrKind {
#define TYPE_ATTR(X) TAK_##X,
#include "swift/AST/Attr.def"
  TAK_Count
};

/// TypeAttributes - These are attributes that may be applied to types.
class TypeAttributes {
  // Get a SourceLoc for every possible attribute that can be parsed in source.
  // the presence of the attribute is indicated by its location being set.
  SourceLoc AttrLocs[TAK_Count];
public:
  /// AtLoc - This is the location of the first '@' in the attribute specifier.
  /// If this is an empty attribute specifier, then this will be an invalid loc.
  SourceLoc AtLoc;
  Optional<AbstractCC> cc = Nothing;

  // For an opened existential type, the known ID.
  Optional<unsigned> OpenedID;

  TypeAttributes() {}
  
  bool isValid() const { return AtLoc.isValid(); }
  
  void clearAttribute(TypeAttrKind A) {
    AttrLocs[A] = SourceLoc();
  }
  
  bool has(TypeAttrKind A) const {
    return getLoc(A).isValid();
  }
  
  SourceLoc getLoc(TypeAttrKind A) const {
    return AttrLocs[A];
  }
  
  void setAttr(TypeAttrKind A, SourceLoc L) {
    assert(!L.isInvalid() && "Cannot clear attribute with this method");
    AttrLocs[A] = L;
  }

  void getAttrLocs(SmallVectorImpl<SourceLoc> &Locs) const {
    for (auto Loc : AttrLocs) {
      if (Loc.isValid())
        Locs.push_back(Loc);
    }
  }

  // This attribute list is empty if no attributes are specified.  Note that
  // the presence of the leading @ is not enough to tell, because we want
  // clients to be able to remove attributes they process until they get to
  // an empty list.
  bool empty() const {
    for (SourceLoc elt : AttrLocs)
      if (elt.isValid()) return false;
    
    return true;
  }
  
  bool hasCC() const { return cc.hasValue(); }
  AbstractCC getAbstractCC() const { return *cc; }
  
  bool hasOwnership() const { return getOwnership() != Ownership::Strong; }
  Ownership getOwnership() const {
    if (has(TAK_sil_weak)) return Ownership::Weak;
    if (has(TAK_sil_unowned)) return Ownership::Unowned;
    return Ownership::Strong;
  }
  
  void clearOwnership() {
    clearAttribute(TAK_sil_weak);
    clearAttribute(TAK_sil_unowned);
  }

  bool hasOpenedID() const { return OpenedID.hasValue(); }
  unsigned getOpenedID() const { return *OpenedID; }
};

class AttributeBase {
public:
  /// The location of the '@'.
  const SourceLoc AtLoc;

  /// The source range of the attribute.
  const SourceRange Range;

  /// The location of the attribute.
  SourceLoc getLocation() const { return Range.Start; }

  /// Return the source range of the attribute.
  SourceRange getRange() const { return Range; }

  // Only allow allocation of attributes using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(AttributeBase));

  void operator delete(void *Data) throw() { }
  void *operator new(size_t Bytes, void *Mem) throw() { return Mem; }

  // Make vanilla new/delete illegal for attributes.
  void *operator new(size_t Bytes) throw() = delete;

  AttributeBase(const AttributeBase &) = delete;

protected:
  AttributeBase(SourceLoc AtLoc, SourceRange Range)
    : AtLoc(AtLoc), Range(Range) {}
};

class DeclAttributes;

  /// Represents one declaration attribute.
class DeclAttribute : public AttributeBase {
  friend class DeclAttributes;
protected:
  class DeclAttrBitFields {
    friend class DeclAttribute;

    // The kind.
    unsigned Kind : 8;

    // Whether this attribute was implicitly added.
    unsigned Implicit : 1;

    unsigned Invalid : 1;
  };
  enum { NumDeclAttrBits = 9 };
  static_assert(NumDeclAttrBits <= 32, "fits in an unsigned");

  class ObjCAttrBitFields {
    friend class ObjCAttr;
    unsigned : NumDeclAttrBits;

    // The arity of the name.
    //
    // 0 indicates that there is no name.
    // 1 indicates that there is a single identifier as the name.
    // 2+ indicates that there are N-1 identifiers, each of which is followed
    // by a colon.
    unsigned Arity : 8;
  };
  enum { NumObjCAttrBits = NumDeclAttrBits + 8 };
  static_assert(NumObjCAttrBits <= 32, "fits in an unsigned");

  union {
    DeclAttrBitFields DeclAttrBits;
    ObjCAttrBitFields ObjCAttrBits;
  };

  DeclAttribute *Next;

  DeclAttribute(DeclAttrKind DK, SourceLoc AtLoc, SourceRange Range,
                bool Implicit) :
    AttributeBase(AtLoc, Range),
    Next(nullptr)
  {
    DeclAttrBits.Kind = static_cast<unsigned>(DK);
    DeclAttrBits.Implicit = Implicit;
    DeclAttrBits.Invalid = 0;
  }

  enum DeclAttrOptions {
    OnFunc = 0x1,
    OnExtension = 1 << 2,
    OnPatternBinding = 1 << 3,
    OnOperator = 1 << 4,
    OnTypeAlias = 1 << 5,
    OnType = 1 << 6,
    OnStruct = 1 << 7,
    OnEnum = 1 << 8,
    OnClass = 1 << 9,
    OnProtocol = 1 << 10,
    OnVar = 1 << 11,
    OnSubscript = 1 << 12,
    OnConstructor = 1 << 13,
    OnDestructor = 1 << 14,
    AllowMultipleAttributes = 1 << 15
  };

  static unsigned getOptions(DeclAttrKind DK);

  unsigned getOptions() const {
    return getOptions(getKind());
  }

public:

  DeclAttrKind getKind() const {
    return static_cast<DeclAttrKind>(DeclAttrBits.Kind);
  }

  /// Whether this attribute was implicitly added.
  bool isImplicit() const { return DeclAttrBits.Implicit; }

  /// Set whether this attribute was implicitly added.
  void setImplicit(bool Implicit) {
    DeclAttrBits.Implicit = Implicit;
  }

  /// Returns true if this attribute was find to be invalid in some way by
  /// semantic analysis.  In that case, the attribute should not be considered,
  /// the attribute node should be only used to retrieve source information.
  bool isInvalid() const { return DeclAttrBits.Invalid; }
  void setInvalid() { DeclAttrBits.Invalid = true; }

  bool isValid() const { return !isInvalid(); }

  /// Returns the address of the next pointer field.
  /// Used for object deserialization.
  DeclAttribute **getMutableNext() {
    return &Next;
  }

  /// Print the attribute to the provided ASTPrinter.
  void print(ASTPrinter &Printer) const;

  /// Print the attribute to the provided stream.
  void print(llvm::raw_ostream &OS) const;

  /// Returns true if this attribute can appear on a function.
  bool canAppearOnFunc() const {
    return getOptions() & OnFunc;
  }

  /// Returns true if this attribute can appear on an extension.
  bool canAppearOnExtension() const {
    return getOptions() & OnExtension;
  }

  /// Returns true if this attribute can appear on an pattern binding.
  bool canAppearOnPatternBinding() const {
    return getOptions() & OnPatternBinding;
  }

  /// Returns true if this attribute can appear on an operator.
  bool canAppearOnOperator() const {
    return getOptions() & OnOperator;
  }

  /// Returns true if this attribute can appear on a typealias.
  bool canAppearOnTypeAlias() const {
    return getOptions() & OnTypeAlias;
  }

  /// Returns true if this attribute can appear on a type declaration.
  bool canAppearOnType() const {
    return getOptions() & OnType;
  }

  /// Returns true if this attribute can appear on a struct.
  bool canAppearOnStruct() const {
    return getOptions() & OnStruct;
  }

  /// Returns true if this attribute can appear on an enum.
  bool canAppearOnEnum() const {
    return getOptions() & OnEnum;
  }

  /// Returns true if this attribute can appear on a class.
  bool canAppearOnClass() const {
    return getOptions() & OnClass;
  }

  /// Returns true if this attribute can appear on a protocol.
  bool canAppearOnProtocol() const {
    return getOptions() & OnProtocol;
  }

  /// Returns true if this attribute can appear on a var declaration.
  bool canAppearOnVar() const {
    return getOptions() & OnVar;
  }

  /// Returns true if this attribute can appear on a subscript declaration.
  bool canAppearOnSubscript() const {
    return getOptions() & OnSubscript;
  }

  /// Returns true if this attribute can appear on a constructor/initializer
  /// declaration.
  bool canAppearOnConstructor() const {
    return getOptions() & OnConstructor;
  }

  /// Returns true if this attribute can appear on a deinitializer
  /// declaration.
  bool canAppearOnDestructor() const {
    return getOptions() & OnDestructor;
  }

  /// Returns true if multiple instances of an attribute kind
  /// can appear on a delcaration.
  static bool allowMultipleAttributes(DeclAttrKind DK) {
    return getOptions(DK) & AllowMultipleAttributes;
  }
};

/// Describes a "simple" declaration attribute that carries no data.
template<DeclAttrKind Kind>
class SimpleDeclAttr : public DeclAttribute {
public:
  SimpleDeclAttr(bool IsImplicit)
    : DeclAttribute(Kind, SourceLoc(), SourceLoc(), IsImplicit) { }

  SimpleDeclAttr(SourceLoc AtLoc, SourceLoc NameLoc)
    : DeclAttribute(Kind, AtLoc, NameLoc, /*isImplicit=*/false) { }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == Kind;
  }
};

// Declare typedefs for all of the simple declaration attributes.
#define SIMPLE_DECL_ATTR(NAME, CLASS, ...) \
 typedef SimpleDeclAttr<DAK_##NAME> CLASS##Attr;
#include "swift/AST/Attr.def"
  
/// Defines the @asmname attribute.
class AsmnameAttr : public DeclAttribute {
public:
  AsmnameAttr(StringRef Name, SourceLoc AtLoc, SourceRange Range, bool Implicit)
    : DeclAttribute(DAK_asmname, AtLoc, Range, Implicit),
      Name(Name) {}

  AsmnameAttr(StringRef Name)
    : AsmnameAttr(Name, SourceLoc(), SourceRange(), /*Implicit=*/true) {}

  /// The symbol name.
  const StringRef Name;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_asmname;
  }
};

/// Defines the @availability attribute.
class AvailabilityAttr : public DeclAttribute {
public:
  AvailabilityAttr(SourceLoc AtLoc, SourceRange Range,
                   StringRef Platform,
                   StringRef Message,
                   bool IsUnavailable,
                   bool Implicit)
    : DeclAttribute(DAK_availability, AtLoc, Range, Implicit),
     Platform(Platform),
     Message(Message),
     IsUnvailable(IsUnavailable) {}

  /// The platform of the availability.
  const StringRef Platform;

  /// The optional message.
  const StringRef Message;

  /// Indicates if the declaration is unconditionally unavailable.
  const bool IsUnvailable;

  /// Returns true if the availability applies to a specific
  /// platform.
  bool hasPlatform() const {
    return !Platform.empty();
  }

  /// Create an AvailabilityAttr that indicates 'unavailable' for all platforms.
  /// This attribute is considered "implicit".
  static AvailabilityAttr *createImplicitUnavailableAttr(ASTContext &C,
                                                         StringRef Message);

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_availability;
  }
};

/// Indicates that the given declaration is visible to Objective-C.
class ObjCAttr : public DeclAttribute {
protected:
  unsigned getArity() const { return ObjCAttrBits.Arity; }

  ObjCAttr(SourceLoc AtLoc, SourceRange Range, unsigned Arity, bool Implicit) 
    : DeclAttribute(DAK_objc, AtLoc, Range, Implicit) 
  { 
    ObjCAttrBits.Arity = Arity;
  }

public:
  /// Describes the kind of @objc attribute, which indicates how it
  /// was named.
  enum Kind {
    /// A bare @objc attribute that was not provided with a name.
    Unnamed,
    /// An @objc attribute that has a single name with no trailing
    /// colon.
    Nullary,
    /// An @objc attribute that has one or more names with colons
    /// trailing each name.
    Selector
  };

public:
  /// Create an unnamed Objective-C attribute, i.e., @objc.
  static ObjCAttr *createUnnamed(ASTContext &Ctx, SourceLoc AtLoc, 
                                 SourceLoc ObjCLoc);

  /// Create a nullary Objective-C attribute, which has a single name
  /// with no colon following it.
  ///
  /// Note that a nullary Objective-C attribute may represent either a
  /// selector for a zero-parameter function or some other Objective-C
  /// entity, such as a class or protocol.
  static ObjCAttr *createNullary(ASTContext &Ctx, SourceLoc AtLoc, 
                                 SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                 SourceLoc NameLoc, Identifier Name,
                                 SourceLoc RParenLoc);

  /// Create an implicit nullary Objective-C attribute, which has a
  /// single name with no colon following it.
  ///
  /// Note that a nullary Objective-C attribute may represent either a
  /// selector for a zero-parameter function or some other Objective-C
  /// entity, such as a class or protocol.
  static ObjCAttr *createNullary(ASTContext &Ctx, Identifier Name);

  /// Create a "selector" Objective-C attribute, which has some number
  /// of identifiers followed by colons.
  static ObjCAttr *createSelector(ASTContext &Ctx, SourceLoc AtLoc, 
                                  SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                  ArrayRef<SourceLoc> NameLocs,
                                  ArrayRef<Identifier> Names,
                                  SourceLoc RParenLoc);

  /// Create an implicit "selector" Objective-C attribute, which has
  /// some number of identifiers followed by colons.
  static ObjCAttr *createSelector(ASTContext &Ctx, ArrayRef<Identifier> Names);

  /// Determine what kind of @objc attribute this is.
  Kind getKind() const {
    switch (getArity()) {
    case 0:
      return Unnamed;

    case 1:
      return Nullary;

    default:
      return Selector;
    }
  }

  /// Determine whether this attribute has a name associated with it.
  bool hasName() const { return getKind() != Unnamed; }

  /// Retrieve the names for a nullary or selector attribute.
  ArrayRef<Identifier> getNames() const;

  /// Retrieve the source locations for the names in a non-implicit
  /// nullary or selector attribute.
  ArrayRef<SourceLoc> getNameLocs() const;

  /// Retrieve the location of the opening parentheses, if there is one.
  SourceLoc getLParenLoc() const;

  /// Retrieve the location of the closing parentheses, if there is one.
  SourceLoc getRParenLoc() const;

  /// Print the name (if any) to the given stream.
  void printName(llvm::raw_ostream &OS) const;

  /// Get the name associated with this attribute.
  ///
  /// \param buffer A buffer used to store the data for the returned name.
  StringRef getName(llvm::SmallVectorImpl<char> &buffer) const;

  /// Clone the given attribute, producing an implicit copy of the
  /// original without source location information.
  ObjCAttr *clone(ASTContext &context) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_objc;
  }
};

/// \brief Attributes that may be applied to declarations.
class DeclAttributes {
  /// Source locations for every possible attribute that can be parsed in
  /// source.
  SourceLoc AttrLocs[AK_Count];
  bool HasAttr[AK_Count] = { false };

  unsigned NumAttrsSet : 8;
  unsigned NumVirtualAttrsSet : 8;

  /// Linked list of declaration attributes.
  DeclAttribute *DeclAttrs;

public:
  /// The location of the first '@' in the attribute specifier.
  ///
  /// This is an invalid location if the declaration does not have any or has
  /// only virtual attributes.
  ///
  /// This could be a valid location even if none of the attributes are set.
  /// This can happen when the attributes were parsed, but then cleared because
  /// they are not allowed in that context.
  SourceLoc AtLoc;

  /// When the mutating attribute is present (i.e., we have a location for it),
  /// this indicates whether it was inverted (@!mutating) or not (@mutating).
  /// Clients should generally use the getMutating() accessor.
  bool MutatingInverted = false;

  /// Source range of the attached comment.  This comment is located before
  /// the declaration.
  CharSourceRange CommentRange;

  DeclAttributes() : NumAttrsSet(0), NumVirtualAttrsSet(0),
                     DeclAttrs(nullptr) {}

  bool shouldSaveInAST() const {
    return AtLoc.isValid() || NumAttrsSet != 0 || DeclAttrs;
  }

  bool containsTraditionalAttributes() const {
    return NumAttrsSet != 0;
  }

  bool hasNonVirtualAttributes() const {
    return NumAttrsSet - NumVirtualAttrsSet != 0;
  }

  void clearAttribute(AttrKind A) {
    if (!has(A))
      return;

    AttrLocs[A] = SourceLoc();
    HasAttr[A] = false;

    // Update counters.
    NumAttrsSet--;
    switch (A) {
#define ATTR(X)
#define VIRTUAL_ATTR(X) case AK_ ## X:
      NumVirtualAttrsSet--;
      break;
#include "swift/AST/Attr.def"

    default:
      break;
    }
  }
  
  bool has(AttrKind A) const {
    return HasAttr[A];
  }

  bool has(DeclAttrKind DK) const {
    for (auto Attr : *this)
      if (Attr->getKind() == DK)
        return true;
    return false;
  }

  SourceLoc getLoc(AttrKind A) const {
    return AttrLocs[A];
  }
  
  void setAttr(AttrKind A, SourceLoc L) {
    bool HadAttribute = has(A);

    AttrLocs[A] = L;
    HasAttr[A] = true;

    if (HadAttribute)
      return;

    // Update counters.
    NumAttrsSet++;
    switch (A) {
#define ATTR(X)
#define VIRTUAL_ATTR(X) case AK_ ## X:
#include "swift/AST/Attr.def"
      NumVirtualAttrsSet++;
      break;

    default:
      break;
    }
  }

  void getAttrLocs(SmallVectorImpl<SourceLoc> &Locs) const {
    for (auto Loc : AttrLocs) {
      if (Loc.isValid())
        Locs.push_back(Loc);
    }
    for (auto Attr : *this) {
      auto Loc = Attr->getLocation();
      if (Loc.isValid())
        Locs.push_back(Loc);
    }
  }

  bool isNoReturn() const { return has(AK_noreturn); }
  bool isAssignment() const { return has(AK_assignment); }
  bool isConversion() const { return has(AK_conversion); }
  bool isTransparent() const {return has(AK_transparent);}
  bool isPrefix() const { return has(AK_prefix); }
  bool isPostfix() const { return has(AK_postfix); }
  bool isInfix() const { return has(AK_infix); }
  bool isIBOutlet() const { return has(AK_IBOutlet); }
  bool isIBAction() const { return has(AK_IBAction); }
  bool isIBDesignable() const { return has(AK_IBDesignable); }
  bool isIBInspectable() const { return has(AK_IBInspectable); }
  bool isClassProtocol() const { return has(AK_class_protocol); }
  bool isWeak() const { return has(AK_weak); }
  bool isUnowned() const { return has(AK_unowned); }
  bool isExported() const { return has(AK_exported); }
  bool isOptional() const { return has(AK_optional); }

  // FIXME: eventually take a platform argument.
  bool isUnavailable() const { return getUnavailable() != nullptr; }

  /// Returns the first @availability attribute that indicates
  /// a declaration is unavailable, or null otherwise.
  //
  // FIXME: eventually take a platform argument.
  const AvailabilityAttr *getUnavailable() const;

  bool isOverride() const { return has(AK_override); }
  bool requiresStoredPropertyInits() const {
    return has(AK_requires_stored_property_inits);
  }

  bool hasMutating() const { return has(AK_mutating); }
  Optional<bool> getMutating() const {
    if (hasMutating())
      return !MutatingInverted;
    return Nothing;
  }

  bool hasOwnership() const { return isWeak() || isUnowned(); }
  Ownership getOwnership() const {
    if (isWeak()) return Ownership::Weak;
    if (isUnowned()) return Ownership::Unowned;
    return Ownership::Strong;
  }
  
  void clearOwnership() {
    clearAttribute(AK_weak);
    clearAttribute(AK_unowned);
  }

  bool hasRawDocComment() const {
    return CommentRange.isValid();
  }

  void print(llvm::raw_ostream &OS) const;
  void print(ASTPrinter &Printer, const PrintOptions &Options) const;

  template <typename T, typename DERIVED> class iterator_base {
    T* Impl;
  public:
    iterator_base(T* Impl) : Impl(Impl) {}
    DERIVED &operator++() { Impl = Impl->Next; return (DERIVED&)*this; }
    bool operator==(const iterator_base &X) const { return X.Impl == Impl; }
    bool operator!=(const iterator_base &X) const { return X.Impl != Impl; }
    T* operator*() const { return Impl; }
    T& operator->() const { return *Impl; }
  };

  /// Add a constructed DeclAttribute to this list.
  void add(DeclAttribute *Attr) {
    Attr->Next = DeclAttrs;
    DeclAttrs = Attr;
  }

  // Iterator interface over DeclAttribute objects.
  class iterator : public iterator_base<DeclAttribute, iterator> {
  public:
    iterator(DeclAttribute *Impl) : iterator_base(Impl) {}
  };

  class const_iterator : public iterator_base<const DeclAttribute,
                                              const_iterator> {
  public:
    const_iterator(const DeclAttribute *Impl) : iterator_base(Impl) {}
  };

  iterator begin() { return DeclAttrs; }
  iterator end() { return nullptr; }
  const_iterator begin() const { return DeclAttrs; }
  const_iterator end() const { return nullptr; }

  /// Retrieve the first attribute of the given attribute class.
  template <typename ATTR>
  const ATTR* getAttribute() const {
    for (auto Attr : *this)
      if (const ATTR* SpecificAttr = dyn_cast<ATTR>(Attr))
        return SpecificAttr;
    return nullptr;
  }

  /// Determine whether there is an attribute with the given attribute class.
  template <typename ATTR>
  bool hasAttribute() const { return getAttribute<ATTR>() != nullptr; }

  /// Determine whether there is an attribute with the given attribute class.
  template <typename ATTR>
  bool hasValidAttribute() const {
    auto TheAttribute = getAttribute<ATTR>();
    return TheAttribute && TheAttribute->isValid();
  }

  /// Retrieve the first attribute with the given kind.
  const DeclAttribute *getAttribute(DeclAttrKind DK) const {
    for (auto Attr : *this)
      if (Attr->getKind() == DK)
        return Attr;
    return nullptr;
  }

  // Remove the given attribute from the list of attributes. Used when
  // the attribute was semantically invalid.
  void removeAttribute(const DeclAttribute *attr) {
    // If it's the first attribute, remove it.
    if (DeclAttrs == attr) {
      DeclAttrs = attr->Next;
      return;
    }

    // Otherwise, find it in the list. This is inefficient, but rare.
    for (auto **prev = &DeclAttrs; *prev; prev = &(*prev)->Next) {
      if ((*prev)->Next == attr) {
        (*prev)->Next = attr->Next;
        return;
      }
    }
    llvm_unreachable("Attribute not found for removal");
  }

  /// Set the raw chain of attributes.  Used for deserialization.
  void setRawAttributeChain(DeclAttribute *Chain) {
    DeclAttrs = Chain;
  }
};
  
} // end namespace swift

#endif
