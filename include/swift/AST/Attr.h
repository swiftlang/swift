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

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/UUID.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/Ownership.h"
#include "swift/AST/PlatformKind.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "clang/Basic/VersionTuple.h"

namespace swift {
class ASTPrinter;
class ASTContext;
struct PrintOptions;
class Decl;
class ClassDecl;

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

/// The kind of unary operator, if any.
enum class UnaryOperatorKind : uint8_t {
  None,
  Prefix,
  Postfix
};

/// Access control levels.
// These are used in diagnostics, so please do not reorder existing values.
enum class Accessibility : uint8_t {
  /// Private access is limited to the current file.
  Private = 0,
  /// Internal access is limited to the current module.
  Internal,
  /// Public access is not limited.
  Public
};

enum class InlineKind : uint8_t {
  Never = 0,
  Always = 1
};

/// This enum represents the possible values of the @effects attribute.
/// These values are ordered from the strongest guarantee to the weakest,
/// so please do not reorder existing values.
enum class EffectsKind : uint8_t {
  ReadNone,
  ReadOnly,
  ReadWrite,
  Unspecified
};

class InfixData {
  unsigned Precedence : 8;

  /// Zero if invalid, or else an Associativity+1.
  unsigned InvalidOrAssoc : 2;
  
  unsigned Assignment : 1;
  
public:
  InfixData() : Precedence(0), InvalidOrAssoc(0), Assignment(0) {}
  InfixData(unsigned char prec, Associativity assoc, bool isAssignment)
    : Precedence(prec), InvalidOrAssoc(unsigned(assoc) + 1),
      Assignment((unsigned)isAssignment) {}

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
  
  bool isAssignment() const {
    assert(isValid());
    return (bool)Assignment;
  }

  friend bool operator==(InfixData L, InfixData R) {
    return L.Precedence == R.Precedence
        && L.InvalidOrAssoc == R.InvalidOrAssoc
        && L.Assignment == R.Assignment;
  }
  friend bool operator!=(InfixData L, InfixData R) {
    return !operator==(L, R);
  }
};

namespace IntrinsicPrecedences {
  enum : unsigned char {
    MinPrecedence = 0,
    IfExpr = 100, // ?:
    AssignExpr = 90, // =
    ExplicitCastExpr = 132, // 'is' and 'as'
    PrefixUnaryExpr = 254,
    PostfixUnaryExpr = 255,
    MaxPrecedence = 255
  };
}

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

  
enum DeclAttrKind : unsigned {
#define DECL_ATTR(_, NAME, ...) DAK_##NAME,
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
  Optional<StringRef> convention = None;

  // For an opened existential type, the known ID.
  Optional<UUID> OpenedID;

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

  void getAttrRanges(SmallVectorImpl<SourceRange> &Ranges) const {
    for (auto Loc : AttrLocs) {
      if (Loc.isValid())
        Ranges.push_back(Loc);
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
  
  bool hasConvention() const { return convention.hasValue(); }
  StringRef getConvention() const { return *convention; }
  
  bool hasOwnership() const { return getOwnership() != Ownership::Strong; }
  Ownership getOwnership() const {
    if (has(TAK_sil_weak)) return Ownership::Weak;
    if (has(TAK_sil_unowned)) return Ownership::Unowned;
    if (has(TAK_sil_unmanaged)) return Ownership::Unmanaged;
    return Ownership::Strong;
  }
  
  void clearOwnership() {
    clearAttribute(TAK_sil_weak);
    clearAttribute(TAK_sil_unowned);
    clearAttribute(TAK_sil_unmanaged);
  }

  bool hasOpenedID() const { return OpenedID.hasValue(); }
  UUID getOpenedID() const { return *OpenedID; }

  /// Given a name like "autoclosure", return the type attribute ID that
  /// corresponds to it.  This returns TAK_Count on failure.
  ///
  static TypeAttrKind getAttrKindFromString(StringRef Str);
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

  SourceRange getRangeWithAt() const {
    if (AtLoc.isValid())
      return {AtLoc, Range.End};
    return Range;
  }

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
enum class DeclKind : uint8_t;

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
  enum { NumDeclAttrBits = 10 };
  static_assert(NumDeclAttrBits <= 32, "fits in an unsigned");

  class ObjCAttrBitFields {
    friend class ObjCAttr;
    unsigned : NumDeclAttrBits;

    /// Whether this attribute has location information that trails the main
    /// record, which contains the locations of the parentheses and any names.
    unsigned HasTrailingLocationInfo : 1;

    /// Whether the name is implicit, produced as the result of caching.
    unsigned ImplicitName : 1;
  };
  enum { NumObjCAttrBits = NumDeclAttrBits + 2 };
  static_assert(NumObjCAttrBits <= 32, "fits in an unsigned");

  class AccessibilityAttrBitFields {
    friend class AbstractAccessibilityAttr;
    unsigned : NumDeclAttrBits;

    unsigned AccessLevel : 2;
  };
  enum { NumAccessibilityAttrBits = NumDeclAttrBits + 2 };
  static_assert(NumAccessibilityAttrBits <= 32, "fits in an unsigned");

  class AutoClosureAttrBitFields {
    friend class AutoClosureAttr;
    unsigned : NumDeclAttrBits;

    unsigned Escaping : 1;
  };
  enum { NumAutoClosureAttrBits = NumDeclAttrBits + 1 };
  static_assert(NumAutoClosureAttrBits <= 32, "fits in an unsigned");

  union {
    DeclAttrBitFields DeclAttrBits;
    ObjCAttrBitFields ObjCAttrBits;
    AccessibilityAttrBitFields AccessibilityAttrBits;
    AutoClosureAttrBitFields AutoClosureAttrBits;
  };

  DeclAttribute *Next = nullptr;

  DeclAttribute(DeclAttrKind DK, SourceLoc AtLoc, SourceRange Range,
                bool Implicit) : AttributeBase(AtLoc, Range) {
    DeclAttrBits.Kind = static_cast<unsigned>(DK);
    DeclAttrBits.Implicit = Implicit;
    DeclAttrBits.Invalid = 0;
  }

public:
  enum DeclAttrOptions {
    /// True if multiple instances of this attribute are allowed on a single
    /// declaration.
    AllowMultipleAttributes = 1 << 0,

    /// True if this is a decl modifier - i.e., that it should not be spelled
    /// with an @.
    DeclModifier = 1 << 1,

    /// True if this is a long attribute that should be printed on its own line.
    ///
    /// Currently has no effect on DeclModifier attributes.
    LongAttribute = 1 << 2,

    /// True if this shouldn't be serialized.
    NotSerialized = 1 << 3,
    
    /// True if this attribute is only valid when parsing a .sil file.
    SILOnly = 1 << 4,

    /// The attribute should be reported by parser as unknown.
    RejectByParser = 1 << 5,

    /// Whether client code cannot use the attribute.
    UserInaccessible = 1 << 6,

    // There is one entry for each DeclKind here, and some higher level buckets
    // down below.  These are used in Attr.def to control which kinds of
    // declarations an attribute can be attached to.
    OnImport           = 1 << 8,
    OnExtension        = 1 << 9,
    OnPatternBinding   = 1 << 10,
    OnEnumCase         = 1 << 11,
    OnTopLevelCode     = 1 << 12,
    OnIfConfig         = 1 << 13,
    OnInfixOperator    = 1 << 14,  // "infix operator"
    OnPrefixOperator   = 1 << 15,  // "prefix operator"
    OnPostfixOperator  = 1 << 16,  // "postfix operator"

    OnEnum             = 1 << 17,
    OnStruct           = 1 << 18,
    OnClass            = 1 << 19,
    OnProtocol         = 1 << 20,
    OnTypeAlias        = 1 << 21,
    OnVar              = 1 << 22,
    OnSubscript        = 1 << 23,

    OnConstructor      = 1 << 24,
    OnDestructor       = 1 << 25,
    OnFunc             = 1 << 26,
    OnEnumElement      = 1 << 27,

    OnGenericTypeParam = 1 << 28,
    OnAssociatedType   = 1 << 29,
    OnParam            = 1 << 30,
    OnModule           = 1 << 31,

    // More coarse-grained aggregations for use in Attr.def.
    OnOperator = OnInfixOperator|OnPrefixOperator|OnPostfixOperator,

    OnAnyDecl = OnImport|OnExtension|OnPatternBinding|OnEnumCase|
                OnTopLevelCode|OnIfConfig|OnInfixOperator|OnPrefixOperator|
                OnPostfixOperator|OnEnum|OnStruct|OnClass|OnProtocol|
                OnTypeAlias|OnVar|OnSubscript|OnConstructor|OnDestructor|
                OnFunc|OnEnumElement|OnGenericTypeParam|OnAssociatedType|OnParam
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
  void print(ASTPrinter &Printer, const PrintOptions &Options) const;

  /// Print the attribute to the provided stream.
  void print(llvm::raw_ostream &OS) const;

  /// Returns true if this attribute can appear on the specified decl.  This is
  /// controlled by the flags in Attr.def.
  bool canAppearOnDecl(const Decl *D) const {
    return canAttributeAppearOnDecl(getKind(), D);
  }

  static bool canAttributeAppearOnDecl(DeclAttrKind DK, const Decl *D);

  /// Returns true if multiple instances of an attribute kind
  /// can appear on a delcaration.
  static bool allowMultipleAttributes(DeclAttrKind DK) {
    return getOptions(DK) & AllowMultipleAttributes;
  }

  bool isLongAttribute() const {
    return isLongAttribute(getKind());
  }
  static bool isLongAttribute(DeclAttrKind DK) {
    return getOptions(DK) & LongAttribute;
  }

  static bool shouldBeRejectedByParser(DeclAttrKind DK) {
    return getOptions(DK) & RejectByParser;
  }

  static bool isSilOnly(DeclAttrKind DK) {
    return getOptions(DK) & SILOnly;
  }

  static bool isUserInaccessible(DeclAttrKind DK) {
    return getOptions(DK) & UserInaccessible;
  }

  bool isDeclModifier() const {
    return isDeclModifier(getKind());
  }
  static bool isDeclModifier(DeclAttrKind DK) {
    return getOptions(DK) & DeclModifier;
  }

  static bool isOnParam(DeclAttrKind DK) {
    return getOptions(DK) & OnParam;
  }

  static bool isOnFunc(DeclAttrKind DK) {
    return getOptions(DK) & OnFunc;
  }

  static bool isOnClass(DeclAttrKind DK) {
    return getOptions(DK) & OnClass;
  }

  static bool isNotSerialized(DeclAttrKind DK) {
    return getOptions(DK) & NotSerialized;
  }
  bool isNotSerialized() const {
    return isNotSerialized(getKind());
  }

  static bool canAttributeAppearOnDeclKind(DeclAttrKind DAK, DeclKind DK);

  /// Returns the source name of the attribute, without the @ or any arguments.
  StringRef getAttrName() const;

  /// Given a name like "inline", return the decl attribute ID that corresponds
  /// to it.  Note that this is a many-to-one mapping, and that the identifier
  /// passed in may only be the first portion of the attribute (e.g. in the case
  /// of the 'unowned(unsafe)' attribute, the string passed in is 'unowned'.
  ///
  /// Also note that this recognizes both attributes like '@inline' (with no @)
  /// and decl modifiers like 'final'.  This returns DAK_Count on failure.
  ///
  static DeclAttrKind getAttrKindFromString(StringRef Str);
};

/// Describes a "simple" declaration attribute that carries no data.
template<DeclAttrKind Kind>
class SimpleDeclAttr : public DeclAttribute {
public:
  SimpleDeclAttr(bool IsImplicit)
    : DeclAttribute(Kind, SourceLoc(), SourceLoc(), IsImplicit) {}

  SimpleDeclAttr(SourceLoc AtLoc, SourceLoc NameLoc)
    : DeclAttribute(Kind, AtLoc,
                    SourceRange(AtLoc.isValid() ? AtLoc : NameLoc, NameLoc),
                    /*Implicit=*/false) { }
  SimpleDeclAttr(SourceLoc NameLoc)
    : DeclAttribute(Kind, SourceLoc(), SourceRange(NameLoc, NameLoc),
                    /*Implicit=*/false) { }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == Kind;
  }
};

// Declare typedefs for all of the simple declaration attributes.
#define SIMPLE_DECL_ATTR(_, CLASS, ...) \
 typedef SimpleDeclAttr<DAK_##CLASS> CLASS##Attr;
#include "swift/AST/Attr.def"

/// Defines the @asmname attribute.
class AsmnameAttr : public DeclAttribute {
public:
  AsmnameAttr(StringRef Name, SourceLoc AtLoc, SourceRange Range, bool Implicit)
    : DeclAttribute(DAK_Asmname, AtLoc, Range, Implicit),
      Name(Name) {}

  AsmnameAttr(StringRef Name, bool Implicit)
    : AsmnameAttr(Name, SourceLoc(), SourceRange(), /*Implicit=*/true) {}

  /// The symbol name.
  const StringRef Name;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Asmname;
  }
};

/// Defines the @_semantics attribute.
class SemanticsAttr : public DeclAttribute {
public:
  SemanticsAttr(StringRef Value, SourceLoc AtLoc, SourceRange Range,
                bool Implicit)
  : DeclAttribute(DAK_Semantics, AtLoc, Range, Implicit),
  Value(Value) {}

  SemanticsAttr(StringRef Value, bool Implicit)
  : SemanticsAttr(Value, SourceLoc(), SourceRange(), /*Implicit=*/true) {}

  /// The semantics tag value.
  const StringRef Value;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Semantics;
  }
};

/// Defines the @_alignment attribute.
class AlignmentAttr : public DeclAttribute {
public:
  AlignmentAttr(unsigned Value, SourceLoc AtLoc, SourceRange Range,
                bool Implicit)
    : DeclAttribute(DAK_Alignment, AtLoc, Range, Implicit),
      Value(Value) {}
  
  // The alignment value.
  const unsigned Value;
  
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Alignment;
  }
};

/// Defines the @_swift_native_objc_runtime_base attribute.
///
/// This attribute indicates a class that should be treated semantically
/// as a native Swift root class, but which inherits a specific Objective-C
/// class at runtime. For most classes this is the runtime's "SwiftObject"
/// root class. The compiler does not need to know about the class; it's the
/// build system's responsibility to link against the ObjC code that implements
/// the root class, and the ObjC implementation's responsibility to ensure
/// instances begin with a Swift-refcounting-compatible object header and
/// override all the necessary NSObject refcounting methods.
class SwiftNativeObjCRuntimeBaseAttr : public DeclAttribute {
public:
  SwiftNativeObjCRuntimeBaseAttr(Identifier BaseClassName,
                                 SourceLoc AtLoc, SourceRange Range,
                                 bool Implicit)
    : DeclAttribute(DAK_SwiftNativeObjCRuntimeBase, AtLoc, Range, Implicit),
      BaseClassName(BaseClassName) {}
  
  // The base class's name.
  const Identifier BaseClassName;
  
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_SwiftNativeObjCRuntimeBase;
  }
};

/// Determine the result of comparing an availability attribute to a specific
/// minimum platform version.
enum class MinVersionComparison {
  /// The entity is guaranteed to be available.
  Available,

  /// The entity is never available.
  Unavailable,

  /// The entity might be unavailable, because it was introduced after
  /// the minimimum version.
  PotentiallyUnavailable,

  /// The entity has been obsoleted.
  Obsoleted,
};

/// Describes the unconditional availability of a declaration.
enum class UnconditionalAvailabilityKind {
  None,
  Deprecated,
  UnavailableInSwift,
  Unavailable,
};

/// Defines the @available attribute.
class AvailableAttr : public DeclAttribute {
public:
#define INIT_VER_TUPLE(X)\
  X(X.empty() ? Optional<clang::VersionTuple>() : X)

  AvailableAttr(SourceLoc AtLoc, SourceRange Range,
                   PlatformKind Platform,
                   StringRef Message, StringRef Rename,
                   const clang::VersionTuple &Introduced,
                   const clang::VersionTuple &Deprecated,
                   const clang::VersionTuple &Obsoleted,
                   UnconditionalAvailabilityKind Unconditional,
                   bool Implicit)
    : DeclAttribute(DAK_Available, AtLoc, Range, Implicit),
      Message(Message), Rename(Rename),
      INIT_VER_TUPLE(Introduced),
      INIT_VER_TUPLE(Deprecated),
      INIT_VER_TUPLE(Obsoleted),
      Unconditional(Unconditional),
      Platform(Platform)
  {}

#undef INIT_VER_TUPLE

  /// The optional message.
  const StringRef Message;

  /// An optional replacement string to emit in a fixit.  This allows simple
  /// declaration renames to be applied by Xcode.
  const StringRef Rename;

  /// Indicates when the symbol was introduced.
  const Optional<clang::VersionTuple> Introduced;

  /// Indicates when the symbol was deprecated.
  const Optional<clang::VersionTuple> Deprecated;

  /// Indicates when the symbol was obsoleted.
  const Optional<clang::VersionTuple> Obsoleted;

  /// Indicates if the declaration has unconditional availability.
  const UnconditionalAvailabilityKind Unconditional;

  /// The platform of the availability.
  const PlatformKind Platform;

  /// Whether this is an unconditionally unavailable entity.
  bool isUnconditionallyUnavailable() const;

  /// Whether this is an unconditionally deprecated entity.
  bool isUnconditionallyDeprecated() const;

  /// Returns the unconditional unavailability.
  UnconditionalAvailabilityKind getUnconditionalAvailability() const {
    return Unconditional;
  }

  /// Determine if a given declaration should be considered unavailable given
  /// the current settings.
  ///
  /// \returns The attribute responsible for making the declaration unavailable.
  static const AvailableAttr *isUnavailable(const Decl *D);

  /// Returns true if the availability applies to a specific
  /// platform.
  bool hasPlatform() const {
    return Platform != PlatformKind::none;
  }

  /// Returns the string for the platform of the attribute.
  StringRef platformString() const {
    return swift::platformString(Platform);
  }

  /// Returns the human-readable string for the platform of the attribute.
  StringRef prettyPlatformString() const {
    return swift::prettyPlatformString(Platform);
  }

  /// Returns true if this attribute is active given the current platform.
  bool isActivePlatform(const ASTContext &ctx) const;

  /// Compare this attribute's version information against the minimum platform
  /// version (assuming the this attribute pertains to the active platform).
  MinVersionComparison getMinVersionAvailability(
                         clang::VersionTuple minVersion) const;

  /// Create an AvailableAttr that indicates specific availability
  /// for all platforms.
  static AvailableAttr *
  createUnconditional(ASTContext &C, StringRef Message, StringRef Rename = "",
                      UnconditionalAvailabilityKind Reason
                        = UnconditionalAvailabilityKind::Unavailable);

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Available;
  }
};

/// Indicates that the given declaration is visible to Objective-C.
class ObjCAttr : public DeclAttribute {
  /// The Objective-C name associated with this entity, stored in its opaque
  /// representation so that we can use null as an indicator for "no name".
  void *NameData;

  /// Create an implicit @objc attribute with the given (optional) name.
  explicit ObjCAttr(Optional<ObjCSelector> name, bool implicitName)
    : DeclAttribute(DAK_ObjC, SourceLoc(), SourceRange(), /*Implicit=*/true),
      NameData(nullptr)
  {
    ObjCAttrBits.HasTrailingLocationInfo = false;
    ObjCAttrBits.ImplicitName = implicitName;

    if (name) {
      NameData = name->getOpaqueValue();
    }
  }

  /// Create an @objc attribute written in the source.
  ObjCAttr(SourceLoc atLoc, SourceRange baseRange, Optional<ObjCSelector> name,
           SourceRange parenRange, ArrayRef<SourceLoc> nameLocs);

  /// Determine whether this attribute has trailing location information.
  bool hasTrailingLocationInfo() const {
    return ObjCAttrBits.HasTrailingLocationInfo;
  }

  /// Retrieve the trailing location information.
  MutableArrayRef<SourceLoc> getTrailingLocations() {
    assert(hasTrailingLocationInfo() && "No trailing location information");
    unsigned length = 2;
    if (auto name = getName())
      length += name->getNumSelectorPieces();
    return { reinterpret_cast<SourceLoc *>(this + 1), length };
  }

  /// Retrieve the trailing location information.
  ArrayRef<SourceLoc> getTrailingLocations() const {
    assert(hasTrailingLocationInfo() && "No trailing location information");
    unsigned length = 2;
    if (auto name = getName())
      length += name->getNumSelectorPieces();
    return { reinterpret_cast<const SourceLoc *>(this + 1), length };
  }

public:
  /// Create implicit ObjC attribute with a given (optional) name.
  static ObjCAttr *create(ASTContext &Ctx, Optional<ObjCSelector> name,
                          bool implicitName);

  /// Create an unnamed Objective-C attribute, i.e., @objc.
  static ObjCAttr *createUnnamed(ASTContext &Ctx, SourceLoc AtLoc, 
                                 SourceLoc ObjCLoc);

  static ObjCAttr *createUnnamedImplicit(ASTContext &Ctx);

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
  static ObjCAttr *createNullary(ASTContext &Ctx, Identifier Name, 
                                 bool isNameImplicit);

  /// Create a "selector" Objective-C attribute, which has some number
  /// of identifiers followed by colons.
  static ObjCAttr *createSelector(ASTContext &Ctx, SourceLoc AtLoc, 
                                  SourceLoc ObjCLoc, SourceLoc LParenLoc, 
                                  ArrayRef<SourceLoc> NameLocs,
                                  ArrayRef<Identifier> Names,
                                  SourceLoc RParenLoc);

  /// Create an implicit "selector" Objective-C attribute, which has
  /// some number of identifiers followed by colons.
  static ObjCAttr *createSelector(ASTContext &Ctx, ArrayRef<Identifier> Names,
                                  bool isNameImplicit);

  /// Determine whether this attribute has a name associated with it.
  bool hasName() const { return NameData != nullptr; }

  /// Retrieve the name of this entity, if specified.
  Optional<ObjCSelector> getName() const {
    if (!hasName())
      return None;

    return ObjCSelector::getFromOpaqueValue(NameData);
  }

  /// Determine whether the name associated with this attribute was
  /// implicit.
  bool isNameImplicit() const { return ObjCAttrBits.ImplicitName; }

  /// Set the name of this entity.
  void setName(ObjCSelector name, bool implicit) {
    // If we already have a name and we have location information, make sure
    // drop the location information rather than allowing it to corrupt our
    // state
    if (hasTrailingLocationInfo() &&
        (!hasName() ||
         getName()->getNumSelectorPieces() < name.getNumSelectorPieces())) {
      ObjCAttrBits.HasTrailingLocationInfo = false;
    }

    NameData = name.getOpaqueValue();
    ObjCAttrBits.ImplicitName = implicit;
  }

  /// Clear the name of this entity.
  void clearName() {
    NameData = nullptr;
  }

  /// Retrieve the source locations for the names in a non-implicit
  /// nullary or selector attribute.
  ArrayRef<SourceLoc> getNameLocs() const;

  /// Retrieve the location of the opening parentheses, if there is one.
  SourceLoc getLParenLoc() const;

  /// Retrieve the location of the closing parentheses, if there is one.
  SourceLoc getRParenLoc() const;

  /// Clone the given attribute, producing an implicit copy of the
  /// original without source location information.
  ObjCAttr *clone(ASTContext &context) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_ObjC;
  }
};

/// Represents any sort of accessibility modifier.
class AbstractAccessibilityAttr : public DeclAttribute {
protected:
  AbstractAccessibilityAttr(DeclAttrKind DK, SourceLoc atLoc, SourceRange range,
                            Accessibility access, bool implicit)
      : DeclAttribute(DK, atLoc, range, implicit) {
    AccessibilityAttrBits.AccessLevel = static_cast<unsigned>(access);
    assert(getAccess() == access && "not enough bits for accessibility");
  }

public:
  Accessibility getAccess() const {
    return static_cast<Accessibility>(AccessibilityAttrBits.AccessLevel);
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Accessibility ||
           DA->getKind() == DAK_SetterAccessibility;
  }
};

/// Represents a 'private', 'internal', or 'public' marker on a declaration.
class AccessibilityAttr : public AbstractAccessibilityAttr {
public:
  AccessibilityAttr(SourceLoc atLoc, SourceRange range, Accessibility access,
                    bool implicit = false)
      : AbstractAccessibilityAttr(DAK_Accessibility, atLoc, range, access,
                                  implicit) {}

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Accessibility;
  }
};

/// Represents a 'private', 'internal', or 'public' marker for a setter on a
/// declaration.
class SetterAccessibilityAttr : public AbstractAccessibilityAttr {
public:
  SetterAccessibilityAttr(SourceLoc atLoc, SourceRange range,
                          Accessibility access, bool implicit = false)
      : AbstractAccessibilityAttr(DAK_SetterAccessibility, atLoc, range, access,
                                  implicit) {}

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_SetterAccessibility;
  }
};

/// Represents the autoclosure attribute.
class AutoClosureAttr : public DeclAttribute {
public:
  AutoClosureAttr(SourceLoc atLoc, SourceRange range, bool escaping,
                  bool implicit = false)
    : DeclAttribute(DAK_AutoClosure, atLoc, range, implicit)
  {
    AutoClosureAttrBits.Escaping = escaping;
  }

  /// Determine whether this autoclosure is escaping.
  bool isEscaping() const { return AutoClosureAttrBits.Escaping; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_AutoClosure;
  }
};

/// Represents an inline attribute.
class InlineAttr : public DeclAttribute {
  InlineKind Kind;
public:
  InlineAttr(SourceLoc atLoc, SourceRange range, InlineKind kind)
    : DeclAttribute(DAK_Inline, atLoc, range, /*Implicit=*/false), 
      Kind(kind) {}

  InlineAttr(InlineKind kind)
    : InlineAttr(SourceLoc(), SourceRange(), kind) {}

  InlineKind getKind() const { return Kind; }
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Inline;
  }
};

/// Represents the side effects attribute.
class EffectsAttr : public DeclAttribute {
  EffectsKind Kind;
public:
  EffectsAttr(SourceLoc atLoc, SourceRange range, EffectsKind kind)
  : DeclAttribute(DAK_Effects, atLoc, range, /*Implicit=*/false),
  Kind(kind) {}

  EffectsAttr(EffectsKind kind)
  : EffectsAttr(SourceLoc(), SourceRange(), kind) {}

  EffectsKind getKind() const { return Kind; }
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Effects;
  }
};



/// Represents weak/unowned/unowned(unsafe) decl modifiers.
class OwnershipAttr : public DeclAttribute {
  const Ownership ownership;
public:
  OwnershipAttr(SourceRange range, Ownership kind)
    : DeclAttribute(DAK_Ownership, range.Start, range, /*Implicit=*/false),
      ownership(kind) {}

  OwnershipAttr(Ownership kind) : OwnershipAttr(SourceRange(), kind) {}

  Ownership get() const { return ownership; }

  /// Returns a copy of this attribute without any source information.
  OwnershipAttr *clone(ASTContext &context) const {
    return new (context) OwnershipAttr(get());
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_Ownership;
  }
};

/// Defines the attribute that we use to model documentation comments.
class RawDocCommentAttr : public DeclAttribute {
  /// Source range of the attached comment.  This comment is located before
  /// the declaration.
  CharSourceRange CommentRange;

public:
  RawDocCommentAttr(CharSourceRange CommentRange)
      : DeclAttribute(DAK_RawDocComment, SourceLoc(), SourceRange(),
                      /*Implicit=*/false),
        CommentRange(CommentRange) {}

  CharSourceRange getCommentRange() const { return CommentRange; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_RawDocComment;
  }
};

/// An attribute applied to a CoreFoundation class that is toll-free bridged to
/// an Objective-C class.
///
/// This attribute is introduced by the Clang importer, and is therefore always
/// implicit.
class ObjCBridgedAttr : public DeclAttribute {
  ClassDecl *ObjCClass;

public:
  ObjCBridgedAttr(ClassDecl *ObjCClass)
    : DeclAttribute(DAK_ObjCBridged, SourceLoc(), SourceRange(),
                    /*Implicit=*/true),
      ObjCClass(ObjCClass)
  {
  }

  /// Retrieve the Objective-C class to which this foreign class is toll-free
  /// bridged.
  ClassDecl *getObjCClass() const { return ObjCClass; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_ObjCBridged;
  }
};

/// An attribute that specifies a synthesized conformance of a known
/// protocol for the declaration to which it appertains.
///
/// There is no spelling for this particular attribute in source code;
/// rather, it is introduced by the Clang importer to indicate
/// synthesized conformances.
class SynthesizedProtocolAttr : public DeclAttribute {
  KnownProtocolKind ProtocolKind;

public:
  SynthesizedProtocolAttr(KnownProtocolKind protocolKind)
    : DeclAttribute(DAK_SynthesizedProtocol, SourceLoc(), SourceRange(),
                    /*Implicit=*/true),
      ProtocolKind(protocolKind)
  {
  }

  /// Retrieve the known protocol kind naming the protocol to be
  /// synthesized.
  KnownProtocolKind getProtocolKind() const { return ProtocolKind; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_SynthesizedProtocol;
  }
};

/// The @warn_unused_result attribute, which specifies that we should
/// receive a warning if a function is called but its result is
/// unused.
///
/// The @warn_unused_result attribute can optionally be provided with
/// a message and a mutable variant. For example:
///
/// \code
/// struct X {
///   @warn_unused_result(message="this string affects your health")
///   func methodA() -> String { ... }
///
///   @warn_unused_result(mutable_variant="jumpInPlace")
///   func jump() -> X { ... }
///
///   mutating func jumpInPlace() { ... }
/// }
/// \endcode
class WarnUnusedResultAttr : public DeclAttribute {
  /// The optional message.
  const StringRef Message;

  /// An optional name of the mutable variant of the given method,
  /// which will be suggested as a replacement if it can be called.
  const StringRef MutableVariant;

public:
  /// A @warn_unused_result attribute with no options.
  WarnUnusedResultAttr(SourceLoc atLoc, SourceLoc attrLoc, bool implicit)
    : DeclAttribute(DAK_WarnUnusedResult, atLoc, SourceRange(atLoc, attrLoc),
                    implicit),
      Message(""), MutableVariant("") { }

  /// A @warn_unused_result attribute with a message or mutable variant.
  WarnUnusedResultAttr(SourceLoc atLoc, SourceLoc attrLoc, SourceLoc lParenLoc,
                       StringRef message, StringRef mutableVariant,
                       SourceLoc rParenLoc, bool implicit)
    : DeclAttribute(DAK_WarnUnusedResult, attrLoc,
                    SourceRange(atLoc, rParenLoc),
                    implicit),
      Message(message), MutableVariant(mutableVariant) { }

  /// Retrieve the message associated with this attribute.
  StringRef getMessage() const { return Message; }

  /// Retrieve the name of the mutable variant of this method.
  StringRef getMutableVariant() const { return MutableVariant; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DAK_WarnUnusedResult;
  }
};

/// \brief Attributes that may be applied to declarations.
class DeclAttributes {
  /// Linked list of declaration attributes.
  DeclAttribute *DeclAttrs;

public:
  DeclAttributes() : DeclAttrs(nullptr) {}

  bool isEmpty() const {
    return DeclAttrs == nullptr;
  }

  void getAttrRanges(SmallVectorImpl<SourceRange> &Ranges) const {
    for (auto Attr : *this) {
      auto R = Attr->getRangeWithAt();
      if (R.isValid())
        Ranges.push_back(R);
    }
  }

  /// If this attribute set has a prefix/postfix attribute on it, return this.
  UnaryOperatorKind getUnaryOperatorKind() const {
    if (hasAttribute<PrefixAttr>())
      return UnaryOperatorKind::Prefix;
    if (hasAttribute<PostfixAttr>())
      return UnaryOperatorKind::Postfix;
    return UnaryOperatorKind::None;
  }

  bool isUnavailable(const ASTContext &ctx) const {
    return getUnavailable(ctx) != nullptr;
  }

  /// Returns the first @available attribute that indicates
  /// a declaration is unavailable, or null otherwise.
  const AvailableAttr *getUnavailable(const ASTContext &ctx) const;

  /// Returns the first @available attribute that indicates
  /// a declaration is deprecated on all deployment targets, or null otherwise.
  const AvailableAttr *getDeprecated(const ASTContext &ctx) const;

  void dump() const;
  void print(ASTPrinter &Printer, const PrintOptions &Options) const;

  template <typename T, typename DERIVED>
  class iterator_base : public std::iterator<std::forward_iterator_tag, T *> {
    T *Impl;
  public:
    explicit iterator_base(T *Impl) : Impl(Impl) {}
    DERIVED &operator++() { Impl = Impl->Next; return (DERIVED&)*this; }
    bool operator==(const iterator_base &X) const { return X.Impl == Impl; }
    bool operator!=(const iterator_base &X) const { return X.Impl != Impl; }
    T *operator*() const { return Impl; }
    T &operator->() const { return *Impl; }
  };

  /// Add a constructed DeclAttribute to this list.
  void add(DeclAttribute *Attr) {
    Attr->Next = DeclAttrs;
    DeclAttrs = Attr;
  }

  // Iterator interface over DeclAttribute objects.
  class iterator : public iterator_base<DeclAttribute, iterator> {
  public:
    explicit iterator(DeclAttribute *Impl) : iterator_base(Impl) {}
  };

  class const_iterator : public iterator_base<const DeclAttribute,
                                              const_iterator> {
  public:
    explicit const_iterator(const DeclAttribute *Impl)
        : iterator_base(Impl) {}
  };

  iterator begin() { return iterator(DeclAttrs); }
  iterator end() { return iterator(nullptr); }
  const_iterator begin() const { return const_iterator(DeclAttrs); }
  const_iterator end() const { return const_iterator(nullptr); }

  /// Retrieve the first attribute of the given attribute class.
  template <typename ATTR>
  const ATTR *getAttribute(bool AllowInvalid = false) const {
    return const_cast<DeclAttributes *>(this)->getAttribute<ATTR>();
  }

  template <typename ATTR>
  ATTR *getAttribute(bool AllowInvalid = false) {
    for (auto Attr : *this)
      if (ATTR *SpecificAttr = dyn_cast<ATTR>(Attr))
        if (SpecificAttr->isValid() || AllowInvalid)
          return SpecificAttr;
    return nullptr;
  }

  /// Determine whether there is an attribute with the given attribute class.
  template <typename ATTR>
  bool hasAttribute(bool AllowInvalid = false) const {
    return getAttribute<ATTR>(AllowInvalid) != nullptr;
  }

  /// Retrieve the first attribute with the given kind.
  const DeclAttribute *getAttribute(DeclAttrKind DK,
                                    bool AllowInvalid = false) const {
    for (auto Attr : *this)
      if (Attr->getKind() == DK && (Attr->isValid() || AllowInvalid))
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

  SourceLoc getStartLoc(bool forModifiers = false) const;
};

} // end namespace swift

#endif
