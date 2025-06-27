//===--- Attr.h - Swift Language Attribute ASTs -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines classes related to declaration attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ATTR_H
#define SWIFT_ATTR_H
#include "swift/AST/ASTAllocated.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/AutoDiff.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/DeclNameLoc.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/LifetimeDependence.h"
#include "swift/AST/MacroDeclaration.h"
#include "swift/AST/Ownership.h"
#include "swift/AST/PlatformKindUtils.h"
#include "swift/AST/StorageImpl.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/EnumTraits.h"
#include "swift/Basic/Feature.h"
#include "swift/Basic/InlineBitfield.h"
#include "swift/Basic/Located.h"
#include "swift/Basic/OptimizationMode.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/UUID.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/bit.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TrailingObjects.h"
#include "llvm/Support/VersionTuple.h"
#include <optional>

namespace swift {
class ArgumentList;
class ASTPrinter;
class ASTContext;
struct PrintOptions;
class CustomAttr;
class Decl;
class DeclRefTypeRepr;
class AbstractFunctionDecl;
class FuncDecl;
class ClassDecl;
class AccessorDecl;
class CustomAttributeInitializer;
class GenericFunctionType;
class LazyConformanceLoader;
class LazyMemberLoader;
class ModuleDecl;
class PatternBindingInitializer;
class TrailingWhereClause;
class TypeExpr;
class UnqualifiedIdentTypeRepr;

class alignas(1 << AttrAlignInBits) AttributeBase
    : public ASTAllocated<AttributeBase> {
public:
  /// The location of the '@'.
  const SourceLoc AtLoc;

  /// The source range of the attribute.
  const SourceRange Range;

  /// The location of the attribute.
  SourceLoc getLocation() const { return Range.Start; }
  SourceLoc getStartLoc() const { return Range.Start; }
  SourceLoc getEndLoc() const { return Range.End; }

  /// Return the source range of the attribute.
  SourceRange getRange() const { return Range; }

  SourceRange getRangeWithAt() const {
    if (AtLoc.isValid())
      return {AtLoc, Range.End};
    return Range;
  }

  AttributeBase(const AttributeBase &) = delete;

protected:
  AttributeBase(SourceLoc AtLoc, SourceRange Range)
    : AtLoc(AtLoc), Range(Range) {}
};

class DeclAttributes;
enum class DeclKind : uint8_t;

enum : unsigned {
  NumInlineKindBits =
      countBitsUsed(static_cast<unsigned>(InlineKind::Last_InlineKind))
};

enum : unsigned {
  NumEffectsKindBits =
      countBitsUsed(static_cast<unsigned>(EffectsKind::Last_EffectsKind))
};

enum : unsigned {
  NumExposureKindBits =
      countBitsUsed(static_cast<unsigned>(ExposureKind::Last_ExposureKind))
};

enum : unsigned {
  NumExternKindBits =
      countBitsUsed(static_cast<unsigned>(ExternKind::Last_ExternKind))
};

enum : unsigned {
  NumNonIsolatedModifierBits = countBitsUsed(
      static_cast<unsigned>(NonIsolatedModifier::Last_NonIsolatedModifier))
};

enum : unsigned {
  NumInheritActorContextKindBits = countBitsUsed(static_cast<unsigned>(
      InheritActorContextModifier::Last_InheritActorContextKind))
};

enum : unsigned { NumDeclAttrKindBits = countBitsUsed(NumDeclAttrKinds - 1) };

enum : unsigned { NumTypeAttrKindBits = countBitsUsed(NumTypeAttrKinds - 1) };

/// Represents one declaration attribute.
class DeclAttribute : public AttributeBase {
  friend class DeclAttributes;

protected:
  // clang-format off
  union {
    uint64_t OpaqueBits;

    SWIFT_INLINE_BITFIELD_BASE(DeclAttribute, bitmax(NumDeclAttrKindBits,8)+1+1+1,
      Kind : bitmax(NumDeclAttrKindBits,8),
      // Whether this attribute was implicitly added.
      Implicit : 1,

      Invalid : 1,

      /// Whether the attribute was created by an access note.
      AddedByAccessNote : 1
    );

    SWIFT_INLINE_BITFIELD(ObjCAttr, DeclAttribute, 1+1,
      /// Whether this attribute has location information that trails the main
      /// record, which contains the locations of the parentheses and any names.
      HasTrailingLocationInfo : 1,

      /// Whether the name is implicit, produced as the result of caching.
      ImplicitName : 1
    );

    SWIFT_INLINE_BITFIELD(DynamicReplacementAttr, DeclAttribute, 1,
      /// Whether this attribute has location information that trails the main
      /// record, which contains the locations of the parentheses and any names.
      HasTrailingLocationInfo : 1
    );

    SWIFT_INLINE_BITFIELD(AbstractAccessControlAttr, DeclAttribute, 3,
      AccessLevel : 3
    );

    SWIFT_INLINE_BITFIELD_FULL(AlignmentAttr, DeclAttribute, 32,
      : NumPadBits,
      // The alignment value.
      Value : 32
    );

    SWIFT_INLINE_BITFIELD(AvailableAttr, DeclAttribute, 4+1+1+1+1+1+1+1,
      /// An `AvailableAttr::Kind` value.
      Kind : 4,

      /// State storage for `SemanticAvailableAttrRequest`.
      HasComputedSemanticAttr : 1,

      /// State storage for `RenamedDeclRequest`.
      HasComputedRenamedDecl : 1,
      HasRenamedDecl : 1,

      /// Whether this attribute was spelled `@_spi_available`.
      IsSPI : 1,

      /// Whether this attribute belongs to a chain of adjacent `@available`
      /// attributes that were generated from a single attribute written in
      /// source using short form syntax, e.g.
      ///
      ///     @available(macOS 15, iOS 18, *)
      ///
      IsGroupMember : 1,

      /// Whether this attribute is the final one in its group.
      IsGroupTerminator : 1,

      /// Whether any members of the group were written as a wildcard
      /// specification (`*`) in source.
      IsGroupedWithWildcard : 1
    );

    SWIFT_INLINE_BITFIELD(ClangImporterSynthesizedTypeAttr, DeclAttribute, 1,
      kind : 1
    );

    SWIFT_INLINE_BITFIELD(EffectsAttr, DeclAttribute, NumEffectsKindBits,
      kind : NumEffectsKindBits
    );

    SWIFT_INLINE_BITFIELD(InlineAttr, DeclAttribute, NumInlineKindBits,
      kind : NumInlineKindBits
    );

    SWIFT_INLINE_BITFIELD(OptimizeAttr, DeclAttribute, NumOptimizationModeBits,
      mode : NumOptimizationModeBits
    );

    SWIFT_INLINE_BITFIELD(ReferenceOwnershipAttr, DeclAttribute,
                          NumReferenceOwnershipBits,
      ownership : NumReferenceOwnershipBits
    );

    SWIFT_INLINE_BITFIELD(AbstractSpecializeAttr, DeclAttribute, 1+1,
      exported : 1,
      kind : 1
    );

    SWIFT_INLINE_BITFIELD(ExposeAttr, DeclAttribute, NumExposureKindBits,
      kind : NumExposureKindBits
    );

    SWIFT_INLINE_BITFIELD(ExternAttr, DeclAttribute, NumExternKindBits,
      kind : NumExternKindBits
    );

    SWIFT_INLINE_BITFIELD(SynthesizedProtocolAttr, DeclAttribute, 1,
      isUnchecked : 1
    );

    SWIFT_INLINE_BITFIELD(ObjCImplementationAttr, DeclAttribute, 3,
      isCategoryNameInvalid : 1,
      hasInvalidImplicitLangAttrs : 1,
      isEarlyAdopter : 1
    );

    SWIFT_INLINE_BITFIELD(NonisolatedAttr, DeclAttribute, NumNonIsolatedModifierBits,
      Modifier : NumNonIsolatedModifierBits
    );

    SWIFT_INLINE_BITFIELD(InheritActorContextAttr, DeclAttribute, NumInheritActorContextKindBits,
      Modifier : NumInheritActorContextKindBits
    );

    SWIFT_INLINE_BITFIELD_FULL(AllowFeatureSuppressionAttr, DeclAttribute, 1+31,
      : NumPadBits,
      Inverted : 1,

      NumFeatures : 31
    );

    SWIFT_INLINE_BITFIELD(LifetimeAttr, DeclAttribute, 1,
      isUnderscored : 1
    );
  } Bits;
  // clang-format on

  DeclAttribute *Next = nullptr;

  DeclAttribute(DeclAttrKind DK, SourceLoc AtLoc, SourceRange Range,
                bool Implicit) : AttributeBase(AtLoc, Range) {
    Bits.OpaqueBits = 0;
    Bits.DeclAttribute.Kind = static_cast<unsigned>(DK);
    Bits.DeclAttribute.Implicit = Implicit;
    Bits.DeclAttribute.Invalid = false;
    Bits.DeclAttribute.AddedByAccessNote = false;
  }

private:
  // NOTE: We cannot use DeclKind due to layering. Even if we could, there is no
  // guarantee that the first DeclKind starts at zero. This is only used to
  // build "OnXYZ" flags.
  enum class DeclKindIndex : unsigned {
#define DECL(Name, _) Name,
#define LAST_DECL(Name) Last_Decl = Name
#include "swift/AST/DeclNodes.def"
  };

public:
  enum DeclAttrRequirements : uint64_t {
    // There is one entry for each DeclKind, and some higher level buckets
    // below. These are used in Attr.def to control which kinds of declarations
    // an attribute can be attached to.
#define DECL(Name, _) On##Name = 1ull << unsigned(DeclKindIndex::Name),
#include "swift/AST/DeclNodes.def"

    // Abstract class aggregations for use in Attr.def.
    OnValue = 0
#define DECL(Name, _)
#define VALUE_DECL(Name, _) |On##Name
#include "swift/AST/DeclNodes.def"
    ,

    OnNominalType = 0
#define DECL(Name, _)
#define NOMINAL_TYPE_DECL(Name, _) |On##Name
#include "swift/AST/DeclNodes.def"
    ,
    OnConcreteNominalType = OnNominalType & ~OnProtocol,
    OnGenericType = OnNominalType | OnTypeAlias,

    OnAbstractFunction = 0
#define DECL(Name, _)
#define ABSTRACT_FUNCTION_DECL(Name, _) |On##Name
#include "swift/AST/DeclNodes.def"
    ,

    OnOperator = 0
#define DECL(Name, _)
#define OPERATOR_DECL(Name, _) |On##Name
#include "swift/AST/DeclNodes.def"
    ,

    OnAnyDecl = 0
#define DECL(Name, _) |On##Name
#include "swift/AST/DeclNodes.def"
    ,

    /// Whether this attribute is valid on additional decls in ClangImporter.
    OnAnyClangDecl = 1ull << (unsigned(DeclKindIndex::Last_Decl) + 1),
  };

  static_assert(
      (unsigned(DeclKindIndex::Last_Decl) + 1) < 64,
      "Overflow decl attr requirement bitfields");
  
  enum DeclAttrBehaviors : uint64_t {
    /// Whether this attribute is only valid when concurrency is enabled.
    ConcurrencyOnly = 1ull << 0,
    
    /// True if multiple instances of this attribute are allowed on a single
    /// declaration.
    AllowMultipleAttributes = 1ull << 1,

    /// True if this is a decl modifier - i.e., that it should not be spelled
    /// with an @.
    DeclModifier = 1ull << 2,

    /// True if this is a long attribute that should be printed on its own line.
    ///
    /// Currently has no effect on DeclModifier attributes.
    LongAttribute = 1ull << 3,

    /// True if this shouldn't be serialized.
    NotSerialized = 1ull << 4,
    
    /// True if this attribute is only valid when parsing a .sil file.
    SILOnly = 1ull << 5,

    /// The attribute should be reported by parser as unknown.
    RejectByParser = 1ull << 6,

    /// Whether client code cannot use the attribute. Hides it in code completion.
    UserInaccessible = 1ull << 7,

    /// Whether adding this attribute can break API
    APIBreakingToAdd = 1ull << 8,

    /// Whether removing this attribute can break API
    APIBreakingToRemove = 1ull << 9,

    /// Whether adding this attribute can break ABI
    ABIBreakingToAdd = 1ull << 10,

    /// Whether removing this attribute can break ABI
    ABIBreakingToRemove = 1ull << 11,

    /// The opposite of APIBreakingToAdd
    APIStableToAdd = 1ull << 12,

    /// The opposite of APIBreakingToRemove
    APIStableToRemove = 1ull << 13,

    /// The opposite of ABIBreakingToAdd
    ABIStableToAdd = 1ull << 14,

    /// The opposite of ABIBreakingToRemove
    ABIStableToRemove = 1ull << 15,

    /// Attribute should not be used in an \c \@abi attribute. Use for
    /// attributes which cannot affect mangled names, even indirectly, and
    /// which either don't affect ABI or where ABI-only declarations get their
    /// behavior from their API counterpart.
    ForbiddenInABIAttr = 1ull << 16,

    /// Attribute can be used without restrictions in an \c \@abi attribute.
    /// Use for attributes which affect mangled names but otherwise don't alter
    /// the ABI, or ones where the \c ABIDeclChecker manually implements
    /// special checking logic (e.g. because several different attributes
    /// contribute to the same aspect of ABI in some complicated way).
    UnconstrainedInABIAttr = 1ull << 17,

    /// Attribute can be used in an \c \@abi attribute, but must match
    /// equivalent on API decl. Use for attributes which affect both mangled
    /// names and other parts of the ABI such that the declaration can only be
    /// valid if they match. 
    EquivalentInABIAttr = 1ull << 18,

    /// Use for attributes which are \em only valid on declarations that cannot
    /// have an \c @abi attribute, such as \c ImportDecl .
    UnreachableInABIAttr = 1ull << 19,
  };

  enum : uint64_t {
    InABIAttrMask = ForbiddenInABIAttr | UnconstrainedInABIAttr
                  | EquivalentInABIAttr | UnreachableInABIAttr
  };

  LLVM_READNONE
  static uint64_t getRequirements(DeclAttrKind DK);

  uint64_t getRequirements() const {
    return getRequirements(getKind());
  }

  LLVM_READNONE
  static uint64_t getBehaviors(DeclAttrKind DK);

  uint64_t getBehaviors() const {
    return getBehaviors(getKind());
  }

  /// Prints this attribute (if applicable), returning `true` if anything was
  /// printed.
  bool printImpl(ASTPrinter &Printer, const PrintOptions &Options,
                 const Decl *D = nullptr) const;

public:
  DeclAttrKind getKind() const {
    return static_cast<DeclAttrKind>(Bits.DeclAttribute.Kind);
  }

  /// Whether this attribute was implicitly added.
  bool isImplicit() const { return Bits.DeclAttribute.Implicit; }

  /// Set whether this attribute was implicitly added.
  void setImplicit(bool Implicit = true) {
    Bits.DeclAttribute.Implicit = Implicit;
  }

  /// Returns true if this attribute was find to be invalid in some way by
  /// semantic analysis.  In that case, the attribute should not be considered,
  /// the attribute node should be only used to retrieve source information.
  bool isInvalid() const { return Bits.DeclAttribute.Invalid; }
  void setInvalid() { Bits.DeclAttribute.Invalid = true; }

  bool isValid() const { return !isInvalid(); }


  /// Determine whether this attribute was added by an access note. If it was,
  /// the compiler will generally recover from failures involving this attribute
  /// as though it is not present.
  bool getAddedByAccessNote() const {
    return Bits.DeclAttribute.AddedByAccessNote;
  }

  void setAddedByAccessNote(bool accessNote = true) {
    Bits.DeclAttribute.AddedByAccessNote = accessNote;
  }

  /// Returns the address of the next pointer field.
  /// Used for object deserialization.
  DeclAttribute **getMutableNext() {
    return &Next;
  }

  /// Print the attribute to the provided ASTPrinter.
  void print(ASTPrinter &Printer, const PrintOptions &Options,
             const Decl *D = nullptr) const;

  /// Print the attribute to the provided stream.
  void print(llvm::raw_ostream &OS, const Decl *D = nullptr) const;

  /// Returns true if this attribute can appear on the specified decl.  This is
  /// controlled by the flags in Attr.def.
  bool canAppearOnDecl(const Decl *D) const {
    return canAttributeAppearOnDecl(getKind(), D);
  }

  LLVM_READONLY
  static bool canAttributeAppearOnDecl(DeclAttrKind DK, const Decl *D);

  /// Returns true if multiple instances of an attribute kind
  /// can appear on a declaration.
  static bool allowMultipleAttributes(DeclAttrKind DK) {
    return getBehaviors(DK) & AllowMultipleAttributes;
  }

  bool isLongAttribute() const {
    return isLongAttribute(getKind());
  }
  static bool isLongAttribute(DeclAttrKind DK) {
    return getBehaviors(DK) & LongAttribute;
  }

  static bool shouldBeRejectedByParser(DeclAttrKind DK) {
    return getBehaviors(DK) & RejectByParser;
  }

  static bool isSilOnly(DeclAttrKind DK) {
    return getBehaviors(DK) & SILOnly;
  }

  static bool isConcurrencyOnly(DeclAttrKind DK) {
    return getBehaviors(DK) & ConcurrencyOnly;
  }

  static bool isUserInaccessible(DeclAttrKind DK) {
    return getBehaviors(DK) & UserInaccessible;
  }

  static bool isAddingBreakingABI(DeclAttrKind DK) {
    return getBehaviors(DK) & ABIBreakingToAdd;
  }

#define DECL_ATTR(_, CLASS, REQUIREMENTS, BEHAVIORS, ...)                      \
  static constexpr bool hasOneBehaviorFor##CLASS(uint64_t Mask) {              \
    return llvm::has_single_bit((BEHAVIORS) & Mask);                           \
  }
#include "swift/AST/DeclAttr.def"

  static bool isAddingBreakingAPI(DeclAttrKind DK) {
    return getBehaviors(DK) & APIBreakingToAdd;
  }

  static bool isRemovingBreakingABI(DeclAttrKind DK) {
    return getBehaviors(DK) & ABIBreakingToRemove;
  }
  static bool isRemovingBreakingAPI(DeclAttrKind DK) {
    return getBehaviors(DK) & APIBreakingToRemove;
  }

  bool isDeclModifier() const {
    return isDeclModifier(getKind());
  }
  static bool isDeclModifier(DeclAttrKind DK) {
    return getBehaviors(DK) & DeclModifier;
  }

  static bool isOnParam(DeclAttrKind DK) {
    return getRequirements(DK) & OnParam;
  }

  static bool isOnFunc(DeclAttrKind DK) {
    return getRequirements(DK) & OnFunc;
  }

  static bool isOnClass(DeclAttrKind DK) {
    return getRequirements(DK) & OnClass;
  }

  static bool isNotSerialized(DeclAttrKind DK) {
    return getBehaviors(DK) & NotSerialized;
  }
  bool isNotSerialized() const {
    return isNotSerialized(getKind());
  }

  LLVM_READNONE
  static bool canAttributeAppearOnDeclKind(DeclAttrKind DAK, DeclKind DK);

  static std::optional<Feature> getRequiredFeature(DeclAttrKind DK);

  /// Returns the source name of the attribute, without the @ or any arguments.
  StringRef getAttrName() const;

  /// Given a name like "inline", return the decl attribute ID that corresponds
  /// to it.  Note that this is a many-to-one mapping, and that the identifier
  /// passed in may only be the first portion of the attribute (e.g. in the case
  /// of the 'unowned(unsafe)' attribute, the string passed in is 'unowned'.
  ///
  /// Also note that this recognizes both attributes like '@inline' (with no @)
  /// and decl modifiers like 'final'.
  ///
  static std::optional<DeclAttrKind> getAttrKindFromString(StringRef Str);

  SWIFT_DEBUG_DUMPER(dump(const ASTContext &ctx));
  void dump(llvm::raw_ostream &out, const ASTContext &ctx) const;

  SWIFT_DEBUG_DUMPER(dump(const DeclContext *dc));
  void dump(llvm::raw_ostream &out, const DeclContext *dc) const;

  static DeclAttribute *createSimple(const ASTContext &context,
                                     DeclAttrKind kind, SourceLoc atLoc,
                                     SourceLoc attrLoc);

  /// Create a copy of this attribute.
  DeclAttribute *clone(ASTContext &ctx) const;

  /// Determine whether we can clone this attribute.
  bool canClone() const;

  /// Determine if this attribute and \p other are "the same", as in, they
  /// would have the same effect on \p attachedTo were they attached to it. A
  /// clone should always be equivalent to the original.
  bool isEquivalent(const DeclAttribute *other, Decl *attachedTo) const;
};

#define UNIMPLEMENTED_CLONE(AttrType)    \
AttrType *clone(ASTContext &ctx) const { \
    llvm_unreachable("unimplemented");   \
    return nullptr;                      \
  }                                      \
bool canClone() const { return false; }

/// Describes a "simple" declaration attribute that carries no data.
template<DeclAttrKind Kind>
class SimpleDeclAttr : public DeclAttribute {
public:
  SimpleDeclAttr(bool IsImplicit)
    : DeclAttribute(Kind, SourceLoc(), SourceLoc(), IsImplicit) {}

  SimpleDeclAttr(SourceLoc AtLoc, SourceLoc NameLoc, bool Implicit = false)
    : DeclAttribute(Kind, AtLoc,
                    SourceRange(AtLoc.isValid() ? AtLoc : NameLoc, NameLoc),
                    Implicit) { }
  SimpleDeclAttr(SourceLoc NameLoc)
    : DeclAttribute(Kind, SourceLoc(), SourceRange(NameLoc, NameLoc),
                    /*Implicit=*/false) { }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == Kind;
  }

  /// Create a copy of this attribute.
  SimpleDeclAttr<Kind> *clone(ASTContext &ctx) const {
    return new (ctx) SimpleDeclAttr<Kind>(
        AtLoc, Range.Start, isImplicit());
  }

  bool isEquivalent(const SimpleDeclAttr<Kind> *other, Decl *attachedTo) const {
    // True by definition, since there's nothing to this other than its kind.
    return true;
  }
};

// Declare typedefs for all of the simple declaration attributes.
#define SIMPLE_DECL_ATTR(_, CLASS, ...)                                        \
  typedef SimpleDeclAttr<DeclAttrKind::CLASS> CLASS##Attr;
#include "swift/AST/DeclAttr.def"

/// Defines the @_silgen_name attribute.
class SILGenNameAttr : public DeclAttribute {
public:
  SILGenNameAttr(StringRef Name, bool Raw, SourceLoc AtLoc, SourceRange Range,
                 bool Implicit)
      : DeclAttribute(DeclAttrKind::SILGenName, AtLoc, Range, Implicit),
        Name(Name), Raw(Raw) {}

  SILGenNameAttr(StringRef Name, bool Raw, bool Implicit)
    : SILGenNameAttr(Name, Raw, SourceLoc(), SourceRange(), Implicit) {}

  /// The symbol name.
  const StringRef Name;

  /// If true, the name is not to be mangled (underscore prefix on Darwin)
  bool Raw;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::SILGenName;
  }

  /// Create a copy of this attribute.
  SILGenNameAttr *clone(ASTContext &ctx) const {
    return new (ctx) SILGenNameAttr(Name, Raw, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const SILGenNameAttr *other, Decl *attachedTo) const {
    return Name == other->Name && Raw == other->Raw;
  }
};

/// Defines the @_section attribute.
class SectionAttr : public DeclAttribute {
public:
  SectionAttr(StringRef Name, SourceLoc AtLoc, SourceRange Range, bool Implicit)
      : DeclAttribute(DeclAttrKind::Section, AtLoc, Range, Implicit),
        Name(Name) {}

  SectionAttr(StringRef Name, bool Implicit)
    : SectionAttr(Name, SourceLoc(), SourceRange(), Implicit) {}

  /// The section name.
  const StringRef Name;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Section;
  }

  SectionAttr *clone(ASTContext &ctx) const {
    return new (ctx) SectionAttr(Name, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const SectionAttr *other, Decl *attachedTo) const {
    return Name == other->Name;
  }
};

/// Defines the @_cdecl attribute.
class CDeclAttr : public DeclAttribute {
public:
  CDeclAttr(StringRef Name, SourceLoc AtLoc, SourceRange Range, bool Implicit,
            bool Underscored)
      : DeclAttribute(DeclAttrKind::CDecl, AtLoc, Range, Implicit),
        Name(Name), Underscored(Underscored) {
  }

  CDeclAttr(StringRef Name, bool Implicit, bool Underscored)
    : CDeclAttr(Name, SourceLoc(), SourceRange(), Implicit, Underscored) {}

  /// The symbol name.
  const StringRef Name;

  /// Is this the version of the attribute that's underscored?
  /// Used to preserve retro compatibility with early adopters.
  const bool Underscored;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::CDecl;
  }

  CDeclAttr *clone(ASTContext &ctx) const {
    return new (ctx) CDeclAttr(Name, AtLoc, Range, isImplicit(),
                               Underscored);
  }

  bool isEquivalent(const CDeclAttr *other, Decl *attachedTo) const {
    return Name == other->Name;
  }
};

/// Defines the @_semantics attribute.
class SemanticsAttr : public DeclAttribute {
public:
  SemanticsAttr(StringRef Value, SourceLoc AtLoc, SourceRange Range,
                bool Implicit)
      : DeclAttribute(DeclAttrKind::Semantics, AtLoc, Range, Implicit),
        Value(Value) {}

  SemanticsAttr(StringRef Value, bool Implicit)
  : SemanticsAttr(Value, SourceLoc(), SourceRange(), Implicit) {}

  /// The semantics tag value.
  const StringRef Value;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Semantics;
  }

  SemanticsAttr *clone(ASTContext &ctx) const {
    return new (ctx) SemanticsAttr(Value, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const SemanticsAttr *other, Decl *attachedTo) const {
    return Value == other->Value;
  }
};

/// Defines the @_alignment attribute.
class AlignmentAttr : public DeclAttribute {
public:
  AlignmentAttr(unsigned Value, SourceLoc AtLoc, SourceRange Range,
                bool Implicit)
      : DeclAttribute(DeclAttrKind::Alignment, AtLoc, Range, Implicit) {
    Bits.AlignmentAttr.Value = Value;
  }

  unsigned getValue() const { return Bits.AlignmentAttr.Value; }
  
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Alignment;
  }

  AlignmentAttr *clone(ASTContext &ctx) const {
    return new (ctx) AlignmentAttr(getValue(), AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const AlignmentAttr *other, Decl *attachedTo) const {
    return getValue() == other->getValue();
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
  SwiftNativeObjCRuntimeBaseAttr(Identifier BaseClassName, SourceLoc AtLoc,
                                 SourceRange Range, bool Implicit)
      : DeclAttribute(DeclAttrKind::SwiftNativeObjCRuntimeBase, AtLoc, Range,
                      Implicit),
        BaseClassName(BaseClassName) {}

  // The base class's name.
  const Identifier BaseClassName;
  
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::SwiftNativeObjCRuntimeBase;
  }

  SwiftNativeObjCRuntimeBaseAttr *clone(ASTContext &ctx) const {
    return new (ctx) SwiftNativeObjCRuntimeBaseAttr(
        BaseClassName, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const SwiftNativeObjCRuntimeBaseAttr *other,
                    Decl *attachedTo) const {
    return BaseClassName == other->BaseClassName;
  }
};

/// Defines the @available attribute.
class AvailableAttr : public DeclAttribute {
public:
  enum class Kind : uint8_t {
    /// The attribute does not specify `deprecated`, `unavailable`,
    /// or `noasync`. Instead, it may specify `introduced:`, `deprecated:`, or
    /// `obsoleted:` versions or it may simply have a `rename:` field.
    Default,
    /// The attribute specifies unconditional deprecation.
    Deprecated,
    /// The attribute specifies unconditional unavailability.
    Unavailable,
    /// The attribute specifies unavailability in asynchronous contexts.
    NoAsync,
  };

  AvailableAttr(SourceLoc AtLoc, SourceRange Range,
                AvailabilityDomainOrIdentifier DomainOrIdentifier,
                SourceLoc DomainLoc, Kind Kind, StringRef Message,
                StringRef Rename, const llvm::VersionTuple &Introduced,
                SourceRange IntroducedRange,
                const llvm::VersionTuple &Deprecated,
                SourceRange DeprecatedRange,
                const llvm::VersionTuple &Obsoleted, SourceRange ObsoletedRange,
                bool Implicit, bool IsSPI);

private:
  friend class SemanticAvailableAttr;
  friend class SemanticAvailableAttrRequest;

  AvailabilityDomainOrIdentifier DomainOrIdentifier;
  const SourceLoc DomainLoc;

  const StringRef Message;
  const StringRef Rename;

  const llvm::VersionTuple Introduced;
  const SourceRange IntroducedRange;
  const llvm::VersionTuple Deprecated;
  const SourceRange DeprecatedRange;
  const llvm::VersionTuple Obsoleted;
  const SourceRange ObsoletedRange;

public:
  /// Returns true if the `AvailabilityDomain` associated with the attribute
  /// has been resolved successfully.
  bool hasCachedDomain() const { return DomainOrIdentifier.isDomain(); }

  AvailabilityDomainOrIdentifier getDomainOrIdentifier() const {
    return DomainOrIdentifier;
  }

  SourceLoc getDomainLoc() const { return DomainLoc; }

  /// Returns the parsed version for `introduced:`.
  std::optional<llvm::VersionTuple> getRawIntroduced() const {
    if (Introduced.empty())
      return std::nullopt;
    return Introduced;
  }

  /// Returns the parsed version for `deprecated:`.
  std::optional<llvm::VersionTuple> getRawDeprecated() const {
    if (Deprecated.empty())
      return std::nullopt;
    return Deprecated;
  }

  /// Returns the parsed version for `obsoleted:`.
  std::optional<llvm::VersionTuple> getRawObsoleted() const {
    if (Obsoleted.empty())
      return std::nullopt;
    return Obsoleted;
  }

  /// Returns the parsed string for `message:`, which will be presented with
  /// diagnostics about the availability of the decl.
  StringRef getMessage() const { return Message; }

  /// Returns the parsed string for `rename:`, which is an optional replacement
  /// string to emit in a fixit.  This allows simple declaration renames to be
  /// applied by Xcode.
  ///
  /// This should take the form of an operator, identifier, or full function
  /// name, optionally with a prefixed type, similar to the syntax used for
  /// the `NS_SWIFT_NAME` annotation in Objective-C.
  ///
  /// \c ValueDecl::getRenamedDecl() can be used to look up the declaration
  /// referred to by this string. Note that this attribute can have a rename
  /// target that was provided directly when synthesized and therefore can have
  /// a rename decl even when this string is empty.
  StringRef getRename() const { return Rename; }

  bool hasCachedRenamedDecl() const {
    return Bits.AvailableAttr.HasRenamedDecl;
  }

  /// Whether this is an unconditionally unavailable entity.
  bool isUnconditionallyUnavailable() const;

  /// Whether this is an unconditionally deprecated entity.
  bool isUnconditionallyDeprecated() const;

  /// Whether this is a noasync attribute.
  bool isNoAsync() const;

  /// Whether this attribute was spelled `@_spi_available`.
  bool isSPI() const { return Bits.AvailableAttr.IsSPI; }

  /// Returns the next attribute in the chain of adjacent `@available`
  /// attributes that were generated from a single attribute written in source
  /// using short form syntax e.g. (`@available(macOS 15, iOS 18, *)`).
  const AvailableAttr *getNextGroupedAvailableAttr() const {
    if (Bits.AvailableAttr.IsGroupMember && !isGroupTerminator())
      return dyn_cast_or_null<AvailableAttr>(Next);
    return nullptr;
  }

  bool isGroupMember() const { return Bits.AvailableAttr.IsGroupMember; }
  void setIsGroupMember() { Bits.AvailableAttr.IsGroupMember = true; }

  /// Whether this attribute is the final one in its group.
  bool isGroupTerminator() const {
    return Bits.AvailableAttr.IsGroupTerminator;
  }
  void setIsGroupTerminator() { Bits.AvailableAttr.IsGroupTerminator = true; }

  /// Whether any members of the group were written as a wildcard specification
  /// (`*`) in source.
  bool isGroupedWithWildcard() const {
    return Bits.AvailableAttr.IsGroupedWithWildcard;
  }
  void setIsGroupedWithWildcard() {
    Bits.AvailableAttr.IsGroupedWithWildcard = true;
  }

  /// Returns the kind of availability the attribute specifies.
  Kind getKind() const { return static_cast<Kind>(Bits.AvailableAttr.Kind); }

  /// Create an `AvailableAttr` that specifies universal unavailability, e.g.
  /// `@available(*, unavailable)`.
  static AvailableAttr *createUniversallyUnavailable(ASTContext &C,
                                                     StringRef Message,
                                                     StringRef Rename = "");

  /// Create an `AvailableAttr` that specifies universal deprecation, e.g.
  /// `@available(*, deprecated)`.
  static AvailableAttr *createUniversallyDeprecated(ASTContext &C,
                                                    StringRef Message,
                                                    StringRef Rename = "");

  /// Create an `AvailableAttr` that specifies unavailability in Swift, e.g.
  /// `@available(swift, unavailable)`.
  static AvailableAttr *createUnavailableInSwift(ASTContext &C,
                                                 StringRef Message,
                                                 StringRef Rename = "");

  /// Create an `AvailableAttr` that specifies availability associated with
  /// Swift language modes, e.g. `@available(swift, obsoleted: 6)`.
  static AvailableAttr *createSwiftLanguageModeVersioned(
      ASTContext &C, StringRef Message, StringRef Rename,
      llvm::VersionTuple Introduced, llvm::VersionTuple Obsoleted);

  /// Create an `AvailableAttr` that specifies versioned availability for a
  /// particular platform, e.g. `@available(macOS, introduced: 13)`.
  static AvailableAttr *createPlatformVersioned(
      ASTContext &C, PlatformKind Platform, StringRef Message, StringRef Rename,
      llvm::VersionTuple Introduced, llvm::VersionTuple Deprecated,
      llvm::VersionTuple Obsoleted);

  /// Create an `AvailableAttr` for `@_unavailableInEmbedded`.
  static AvailableAttr *createUnavailableInEmbedded(ASTContext &ctx,
                                                    SourceLoc AtLoc,
                                                    SourceRange Range);

  AvailableAttr *clone(ASTContext &C, bool implicit) const;
  AvailableAttr *clone(ASTContext &C) const {
    return clone(C, isImplicit());
  }

  bool isEquivalent(const AvailableAttr *other, Decl *attachedTo) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Available;
  }

private:
  friend class RenamedDeclRequest;

  bool hasComputedRenamedDecl() const {
    return Bits.AvailableAttr.HasComputedRenamedDecl;
  }

  void setComputedRenamedDecl(bool hasRenamedDecl) {
    Bits.AvailableAttr.HasComputedRenamedDecl = true;
    Bits.AvailableAttr.HasRenamedDecl = hasRenamedDecl;
  }

private:
  friend class SemanticAvailableAttrRequest;

  bool hasComputedSemanticAttr() const {
    return Bits.AvailableAttr.HasComputedSemanticAttr;
  }

  void setComputedSemanticAttr() {
    Bits.AvailableAttr.HasComputedSemanticAttr = true;
  }
};

/// Indicates that the given declaration is visible to Objective-C.
class ObjCAttr final : public DeclAttribute,
    private llvm::TrailingObjects<ObjCAttr, SourceLoc> {
  friend TrailingObjects;

  /// The Objective-C name associated with this entity, stored in its opaque
  /// representation so that we can use null as an indicator for "no name".
  void *NameData;

  /// Create an implicit @objc attribute with the given (optional) name.
  explicit ObjCAttr(std::optional<ObjCSelector> name, bool implicitName)
      : DeclAttribute(DeclAttrKind::ObjC, SourceLoc(), SourceRange(),
                      /*Implicit=*/true),
        NameData(nullptr) {
    Bits.ObjCAttr.HasTrailingLocationInfo = false;
    Bits.ObjCAttr.ImplicitName = implicitName;

    if (name) {
      NameData = name->getOpaqueValue();
    }
  }

  /// Create an @objc attribute written in the source.
  ObjCAttr(SourceLoc atLoc, SourceRange baseRange,
           std::optional<ObjCSelector> name, SourceRange parenRange,
           ArrayRef<SourceLoc> nameLocs);

  /// Determine whether this attribute has trailing location information.
  bool hasTrailingLocationInfo() const {
    return Bits.ObjCAttr.HasTrailingLocationInfo;
  }

  /// Retrieve the trailing location information.
  MutableArrayRef<SourceLoc> getTrailingLocations() {
    assert(hasTrailingLocationInfo() && "No trailing location information");
    unsigned length = 2;
    if (auto name = getName())
      length += name->getNumSelectorPieces();
    return {getTrailingObjects<SourceLoc>(), length};
  }

  /// Retrieve the trailing location information.
  ArrayRef<SourceLoc> getTrailingLocations() const {
    assert(hasTrailingLocationInfo() && "No trailing location information");
    unsigned length = 2;
    if (auto name = getName())
      length += name->getNumSelectorPieces();
    return {getTrailingObjects<SourceLoc>(), length};
  }

public:
  /// Create implicit ObjC attribute with a given (optional) name.
  static ObjCAttr *create(ASTContext &Ctx, std::optional<ObjCSelector> name,
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
  std::optional<ObjCSelector> getName() const {
    if (!hasName())
      return std::nullopt;

    return ObjCSelector::getFromOpaqueValue(NameData);
  }

  /// Determine whether the name associated with this attribute was
  /// implicit.
  bool isNameImplicit() const { return Bits.ObjCAttr.ImplicitName; }
  void setNameImplicit(bool newValue) { Bits.ObjCAttr.ImplicitName = newValue; }

  /// Set the name of this entity.
  void setName(ObjCSelector name, bool implicit) {
    // If we already have a name and we have location information, make sure
    // drop the location information rather than allowing it to corrupt our
    // state
    if (hasTrailingLocationInfo() &&
        (!hasName() ||
         getName()->getNumSelectorPieces() < name.getNumSelectorPieces())) {
      Bits.ObjCAttr.HasTrailingLocationInfo = false;
    }

    NameData = name.getOpaqueValue();
    Bits.ObjCAttr.ImplicitName = implicit;
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

  bool isEquivalent(const ObjCAttr *other, Decl *attachedTo) const {
    std::optional<ObjCSelector> thisName, otherName;
    if (hasName() && !isNameImplicit())
      thisName = getName();
    if (other->hasName() && !other->isNameImplicit())
      otherName = other->getName();

    return thisName == otherName;
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ObjC;
  }
};

class PrivateImportAttr final
: public DeclAttribute {
  StringRef SourceFile;

  PrivateImportAttr(SourceLoc atLoc, SourceRange baseRange,
                    StringRef sourceFile, SourceRange parentRange,
                    bool implicit = false);

public:
  static PrivateImportAttr *create(ASTContext &Ctxt, SourceLoc AtLoc,
                                   SourceLoc PrivateLoc, SourceLoc LParenLoc,
                                   StringRef sourceFile, SourceLoc RParenLoc);

  StringRef getSourceFile() const {
    return SourceFile;
  }
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::PrivateImport;
  }

  /// Create a copy of this attribute.
  PrivateImportAttr *clone(ASTContext &ctx) const {
    return new (ctx) PrivateImportAttr(
        AtLoc, Range, SourceFile, SourceRange(), isImplicit());
  }

  bool isEquivalent(const PrivateImportAttr *other, Decl *attachedTo) const {
    return getSourceFile() == other->getSourceFile();
  }
};

/// The @_dynamicReplacement(for:) attribute.
class DynamicReplacementAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<DynamicReplacementAttr, SourceLoc> {
  friend TrailingObjects;
  friend class DynamicallyReplacedDeclRequest;

  DeclNameRef ReplacedFunctionName;
  LazyMemberLoader *Resolver = nullptr;
  uint64_t ResolverContextData;

  /// Create an @_dynamicReplacement(for:) attribute written in the source.
  DynamicReplacementAttr(SourceLoc atLoc, SourceRange baseRange,
                         DeclNameRef replacedFunctionName,
                         SourceRange parenRange);

  DynamicReplacementAttr(DeclNameRef name, AbstractFunctionDecl *f)
      : DeclAttribute(DeclAttrKind::DynamicReplacement, SourceLoc(),
                      SourceRange(),
                      /*Implicit=*/false),
        ReplacedFunctionName(name), Resolver(nullptr), ResolverContextData(0) {
    Bits.DynamicReplacementAttr.HasTrailingLocationInfo = false;
  }

  DynamicReplacementAttr(DeclNameRef name, LazyMemberLoader *Resolver = nullptr,
                         uint64_t Data = 0)
      : DeclAttribute(DeclAttrKind::DynamicReplacement, SourceLoc(),
                      SourceRange(),
                      /*Implicit=*/false),
        ReplacedFunctionName(name), Resolver(Resolver),
        ResolverContextData(Data) {
    Bits.DynamicReplacementAttr.HasTrailingLocationInfo = false;
  }

  /// Retrieve the trailing location information.
  MutableArrayRef<SourceLoc> getTrailingLocations() {
    assert(Bits.DynamicReplacementAttr.HasTrailingLocationInfo);
    unsigned length = 2;
    return {getTrailingObjects<SourceLoc>(), length};
  }

  /// Retrieve the trailing location information.
  ArrayRef<SourceLoc> getTrailingLocations() const {
    assert(Bits.DynamicReplacementAttr.HasTrailingLocationInfo);
    unsigned length = 2; // lParens, rParens
    return {getTrailingObjects<SourceLoc>(), length};
  }

public:
  static DynamicReplacementAttr *
  create(ASTContext &Context, SourceLoc AtLoc, SourceLoc DynReplLoc,
         SourceLoc LParenLoc, DeclNameRef replacedFunction, SourceLoc RParenLoc);

  static DynamicReplacementAttr *create(ASTContext &ctx,
                                        DeclNameRef replacedFunction,
                                        AbstractFunctionDecl *replacedFuncDecl);

  static DynamicReplacementAttr *create(ASTContext &ctx,
                                        DeclNameRef replacedFunction,
                                        LazyMemberLoader *Resolver,
                                        uint64_t Data);

  DeclNameRef getReplacedFunctionName() const {
    return ReplacedFunctionName;
  }

  /// Retrieve the location of the opening parentheses, if there is one.
  SourceLoc getLParenLoc() const;

  /// Retrieve the location of the closing parentheses, if there is one.
  SourceLoc getRParenLoc() const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::DynamicReplacement;
  }

  UNIMPLEMENTED_CLONE(DynamicReplacementAttr)

  bool isEquivalent(const DynamicReplacementAttr *other, Decl *attachedTo) const {
    return getReplacedFunctionName() == other->getReplacedFunctionName();
  }
};

/// The \c @_typeEraser(TypeEraserType) attribute.
class TypeEraserAttr final : public DeclAttribute {
  TypeExpr *TypeEraserExpr;
  LazyMemberLoader *Resolver;
  uint64_t ResolverContextData;

  friend class ResolveTypeEraserTypeRequest;

  TypeEraserAttr(SourceLoc atLoc, SourceRange range, TypeExpr *typeEraserExpr,
                 LazyMemberLoader *Resolver, uint64_t Data)
      : DeclAttribute(DeclAttrKind::TypeEraser, atLoc, range,
                      /*Implicit=*/false),
        TypeEraserExpr(typeEraserExpr), Resolver(Resolver),
        ResolverContextData(Data) {}

public:
  static TypeEraserAttr *create(ASTContext &ctx,
                                SourceLoc atLoc, SourceRange range,
                                TypeExpr *typeEraserRepr);

  static TypeEraserAttr *create(ASTContext &ctx,
                                LazyMemberLoader *Resolver,
                                uint64_t Data);

  /// Retrieve the parsed type repr for this attribute, if it
  /// was parsed. Else returns \c nullptr.
  TypeRepr *getParsedTypeEraserTypeRepr() const;

  /// Retrieve the parsed location for this attribute, if it was parsed.
  SourceLoc getLoc() const;

  /// Retrieve the resolved type of this attribute if it has been resolved by a
  /// successful call to \c getResolvedType(). Otherwise,
  /// returns \c Type()
  ///
  /// This entrypoint is only suitable for syntactic clients like the
  /// AST printer. Semantic clients should use \c getResolvedType() instead.
  Type getTypeWithoutResolving() const;

  /// Returns \c true if the type eraser type has a valid implementation of the
  /// erasing initializer for the given protocol.
  bool hasViableTypeEraserInit(ProtocolDecl *protocol) const;

  /// Resolves the type of this attribute.
  ///
  /// This entrypoint is suitable for semantic clients like the
  /// expression checker. Syntactic clients should use
  /// \c getTypeWithoutResolving() instead.
  Type getResolvedType(const ProtocolDecl *PD) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::TypeEraser;
  }

  UNIMPLEMENTED_CLONE(TypeEraserAttr)

  bool isEquivalent(const TypeEraserAttr *other, Decl *attachedTo) const;
};

/// Represents any sort of access control modifier.
class AbstractAccessControlAttr : public DeclAttribute {
protected:
  AbstractAccessControlAttr(DeclAttrKind DK, SourceLoc atLoc, SourceRange range,
                            AccessLevel access, bool implicit)
      : DeclAttribute(DK, atLoc, range, implicit) {
    Bits.AbstractAccessControlAttr.AccessLevel = static_cast<unsigned>(access);
    assert(getAccess() == access && "not enough bits for access control");
  }

public:
  AccessLevel getAccess() const {
    return static_cast<AccessLevel>(Bits.AbstractAccessControlAttr.AccessLevel);
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::AccessControl ||
           DA->getKind() == DeclAttrKind::SetterAccess;
  }
};

/// Represents a 'private', 'internal', or 'public' marker on a declaration.
class AccessControlAttr : public AbstractAccessControlAttr {
public:
  AccessControlAttr(SourceLoc atLoc, SourceRange range, AccessLevel access,
                    bool implicit = false)
      : AbstractAccessControlAttr(DeclAttrKind::AccessControl, atLoc, range,
                                  access, implicit) {}

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::AccessControl;
  }

  /// Create a copy of this attribute.
  AccessControlAttr *clone(ASTContext &ctx) const {
    return new (ctx) AccessControlAttr(AtLoc, Range, getAccess(), isImplicit());
  }

  bool isEquivalent(const AccessControlAttr *other, Decl *attachedTo) const {
    return getAccess() == other->getAccess();
  }
};

/// Represents a 'private', 'internal', or 'public' marker for a setter on a
/// declaration.
class SetterAccessAttr : public AbstractAccessControlAttr {
public:
  SetterAccessAttr(SourceLoc atLoc, SourceRange range, AccessLevel access,
                   bool implicit = false)
      : AbstractAccessControlAttr(DeclAttrKind::SetterAccess, atLoc, range,
                                  access, implicit) {}

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::SetterAccess;
  }

  /// Create a copy of this attribute.
  SetterAccessAttr *clone(ASTContext &ctx) const {
    return new (ctx) SetterAccessAttr(AtLoc, Range, getAccess(), isImplicit());
  }

  bool isEquivalent(const SetterAccessAttr *other, Decl *attachedTo) const {
    return getAccess() == other->getAccess();
  }
};

/// SPI attribute applied to both decls and imports.
class SPIAccessControlAttr final : public DeclAttribute,
                                   private llvm::TrailingObjects<SPIAccessControlAttr, Identifier> {
  friend TrailingObjects;

  SPIAccessControlAttr(SourceLoc atLoc, SourceRange range,
                       ArrayRef<Identifier> spiGroups);

  // Number of trailing SPI group identifiers.
  size_t numSPIGroups;

public:
  static SPIAccessControlAttr *create(ASTContext &context, SourceLoc atLoc,
                                      SourceRange range,
                                      ArrayRef<Identifier> spiGroups);

  SPIAccessControlAttr *clone(ASTContext &C, bool implicit) const;

  SPIAccessControlAttr *clone(ASTContext &ctx) const {
    return clone(ctx, isImplicit());
  }

  /// Name of SPIs declared by the attribute.
  ///
  /// Note: A single SPI name per attribute is currently supported but this
  /// may change with the syntax change.
  ArrayRef<Identifier> getSPIGroups() const {
    return { this->template getTrailingObjects<Identifier>(),
             numSPIGroups };
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::SPIAccessControl;
  }

   bool isEquivalent(const SPIAccessControlAttr *other, Decl *attachedTo) const;
};

/// Represents an inline attribute.
class InlineAttr : public DeclAttribute {
public:
  InlineAttr(SourceLoc atLoc, SourceRange range, InlineKind kind,
             bool implicit = false)
      : DeclAttribute(DeclAttrKind::Inline, atLoc, range, implicit) {
    Bits.InlineAttr.kind = unsigned(kind);
  }

  InlineAttr(InlineKind kind)
    : InlineAttr(SourceLoc(), SourceRange(), kind) {}

  InlineKind getKind() const { return InlineKind(Bits.InlineAttr.kind); }
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Inline;
  }

  InlineAttr *clone(ASTContext &ctx) const {
    return new (ctx) InlineAttr(AtLoc, Range, getKind(), isImplicit());
  }

  bool isEquivalent(const InlineAttr *other, Decl *attachedTo) const {
    return getKind() == other->getKind();
  }
};

/// Represents the optimize attribute.
class OptimizeAttr : public DeclAttribute {
public:
  OptimizeAttr(SourceLoc atLoc, SourceRange range, OptimizationMode mode,
               bool implicit = false)
      : DeclAttribute(DeclAttrKind::Optimize, atLoc, range, implicit) {
    Bits.OptimizeAttr.mode = unsigned(mode);
  }

  OptimizeAttr(OptimizationMode mode)
    : OptimizeAttr(SourceLoc(), SourceRange(), mode) {}

  OptimizationMode getMode() const {
    return OptimizationMode(Bits.OptimizeAttr.mode);
  }
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Optimize;
  }

  OptimizeAttr *clone(ASTContext &ctx) const {
    return new (ctx) OptimizeAttr(AtLoc, Range, getMode(), isImplicit());
  }

  bool isEquivalent(const OptimizeAttr *other, Decl *attachedTo) const {
    return getMode() == other->getMode();
  }
};

/// Represents the exclusivity attribute.
class ExclusivityAttr : public DeclAttribute {
public:
  enum Mode {
    Checked,
    Unchecked
  };

private:
  Mode mode;

public:
  ExclusivityAttr(SourceLoc atLoc, SourceRange range, Mode mode,
                  bool implicit = false)
      : DeclAttribute(DeclAttrKind::Exclusivity, atLoc, range, implicit),
        mode(mode) {}

  ExclusivityAttr(Mode mode)
    : ExclusivityAttr(SourceLoc(), SourceRange(), mode) {}

  Mode getMode() const { return mode; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Exclusivity;
  }

  ExclusivityAttr *clone(ASTContext &ctx) const {
    return new (ctx) ExclusivityAttr(AtLoc, Range, getMode(), isImplicit());
  }

  bool isEquivalent(const ExclusivityAttr *other, Decl *attachedTo) const {
    return getMode() == other->getMode();
  }
};

/// Represents the side effects attribute.
class EffectsAttr : public DeclAttribute {
  StringRef customString;
  SourceLoc customStringLoc;

public:
  EffectsAttr(SourceLoc atLoc, SourceRange range, EffectsKind kind)
      : DeclAttribute(DeclAttrKind::Effects, atLoc, range, /*Implicit=*/false) {
    Bits.EffectsAttr.kind = unsigned(kind);
  }

  EffectsAttr(SourceLoc atLoc, SourceRange range, StringRef customString,
              SourceLoc customStringLoc)
      : DeclAttribute(DeclAttrKind::Effects, atLoc, range, /*Implicit=*/false),
        customString(customString), customStringLoc(customStringLoc) {
    Bits.EffectsAttr.kind = unsigned(EffectsKind::Custom);
  }

  EffectsAttr(EffectsKind kind)
  : EffectsAttr(SourceLoc(), SourceRange(), kind) {}

  EffectsAttr(StringRef customString)
  : EffectsAttr(SourceLoc(), SourceRange(), customString, SourceLoc()) {}

  StringRef getCustomString() const {
    assert(getKind() == EffectsKind::Custom);
    return customString;
  }
  
  SourceLoc getCustomStringLocation() const {
    return customStringLoc;
  }

  EffectsKind getKind() const { return EffectsKind(Bits.EffectsAttr.kind); }
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Effects;
  }

  EffectsAttr *clone(ASTContext &ctx) const {
    if (getKind() == EffectsKind::Custom) {
      return new (ctx) EffectsAttr(customString);
    }
    return new (ctx) EffectsAttr(getKind());
  }

  bool isEquivalent(const EffectsAttr *other, Decl *attachedTo) const {
    if (getKind() != other->getKind())
      return false;
    if (getKind() == EffectsKind::Custom)
      return getCustomString() == other->getCustomString();
    return true;
  }
};



/// Represents weak/unowned/unowned(unsafe) decl modifiers.
class ReferenceOwnershipAttr : public DeclAttribute {
public:
  ReferenceOwnershipAttr(SourceRange range, ReferenceOwnership kind)
      : DeclAttribute(DeclAttrKind::ReferenceOwnership, range.Start, range,
                      /*Implicit=*/false) {
    Bits.ReferenceOwnershipAttr.ownership = unsigned(kind);
  }

  ReferenceOwnershipAttr(ReferenceOwnership kind)
      : ReferenceOwnershipAttr(SourceRange(), kind) {}

  ReferenceOwnership get() const {
    return ReferenceOwnership(Bits.ReferenceOwnershipAttr.ownership);
  }

  /// Returns a copy of this attribute without any source information.
  ReferenceOwnershipAttr *clone(ASTContext &context) const {
    return new (context) ReferenceOwnershipAttr(get());
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ReferenceOwnership;
  }

  bool isEquivalent(const ReferenceOwnershipAttr *other,
                    Decl *attachedTo) const {
    return get() == other->get();
  }
};

/// Defines the attribute that we use to model documentation comments.
class RawDocCommentAttr : public DeclAttribute {
  /// Source range of the attached comment.  This comment is located before
  /// the declaration.
  CharSourceRange CommentRange;

public:
  RawDocCommentAttr(CharSourceRange CommentRange, bool Implicit = false)
      : DeclAttribute(DeclAttrKind::RawDocComment, SourceLoc(), SourceRange(),
                      Implicit),
        CommentRange(CommentRange) {}

  CharSourceRange getCommentRange() const { return CommentRange; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::RawDocComment;
  }

  RawDocCommentAttr *clone(ASTContext &ctx) const {
    return new (ctx) RawDocCommentAttr(CommentRange, isImplicit());
  }

  bool isEquivalent(const RawDocCommentAttr *other, Decl *attachedTo) const {
    return getCommentRange() == other->getCommentRange();
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
      : DeclAttribute(DeclAttrKind::ObjCBridged, SourceLoc(), SourceRange(),
                      /*Implicit=*/true),
        ObjCClass(ObjCClass) {}

  /// Retrieve the Objective-C class to which this foreign class is toll-free
  /// bridged.
  ClassDecl *getObjCClass() const { return ObjCClass; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ObjCBridged;
  }

  ObjCBridgedAttr *clone(ASTContext &ctx) const {
    return new (ctx) ObjCBridgedAttr(ObjCClass);
  }

  bool isEquivalent(const ObjCBridgedAttr *other, Decl *attachedTo) const {
    return getObjCClass() == other->getObjCClass();
  }
};

/// An attribute that specifies a synthesized conformance of a known
/// protocol for the declaration to which it appertains.
///
/// There is no spelling for this particular attribute in source code;
/// rather, it is introduced by the Clang importer to indicate
/// synthesized conformances.
class SynthesizedProtocolAttr : public DeclAttribute {
  LazyConformanceLoader *Loader;
  ProtocolDecl *protocol;

public:
  SynthesizedProtocolAttr(ProtocolDecl *protocol, LazyConformanceLoader *Loader,
                          bool isUnchecked)
      : DeclAttribute(DeclAttrKind::SynthesizedProtocol, SourceLoc(),
                      SourceRange(),
                      /*Implicit=*/true),
        Loader(Loader), protocol(protocol) {
    Bits.SynthesizedProtocolAttr.isUnchecked = unsigned(isUnchecked);
  }

  /// Retrieve the known protocol kind naming the protocol to be
  /// synthesized.
  ProtocolDecl *getProtocol() const {
    return protocol;
  }

  bool isUnchecked() const {
    return bool(Bits.SynthesizedProtocolAttr.isUnchecked);
  }

  /// Retrieve the lazy loader that will be used to populate the
  /// synthesized conformance.
  LazyConformanceLoader *getLazyLoader() const { return Loader; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::SynthesizedProtocol;
  }

  SynthesizedProtocolAttr *clone(ASTContext &ctx) const {
    return new (ctx) SynthesizedProtocolAttr(
        protocol, getLazyLoader(), isUnchecked());
  }

  bool isEquivalent(const SynthesizedProtocolAttr *other,
                    Decl *attachedTo) const {
    return isUnchecked() == other->isUnchecked()
            && getProtocol() == other->getProtocol()
            && getLazyLoader() == other->getLazyLoader();
  }
};

/// The @_specialize/@specialize attribute, which forces specialization on the
/// specified type list.
template<typename Base, typename...AdditionalTrailingObjects>
using SpecializeAttrTrailingObjects = llvm::TrailingObjects<Base,
  Identifier, AvailableAttr *, Type , AdditionalTrailingObjects...>;

class AbstractSpecializeAttr
    : public DeclAttribute,
      private llvm::trailing_objects_internal::TrailingObjectsBase {
  friend class SpecializeAttrTargetDeclRequest;
  friend class SerializeAttrGenericSignatureRequest;

public:
  // NOTE: When adding new kinds, you must update the inline bitfield macro.
  enum class SpecializationKind {
    Full,
    Partial
  };

private:
  TrailingWhereClause *trailingWhereClause;
  GenericSignature specializedSignature;

  DeclNameRef targetFunctionName;
  LazyMemberLoader *resolver = nullptr;
  uint64_t resolverContextData;
  size_t numSPIGroups;
  size_t numAvailableAttrs;
  size_t numTypeErasedParams;
  bool typeErasedParamsInitialized;

protected:
  AbstractSpecializeAttr(DeclAttrKind DK, SourceLoc atLoc, SourceRange Range,
                 TrailingWhereClause *clause,
                 bool exported,
                 SpecializationKind kind, GenericSignature specializedSignature,
                 DeclNameRef targetFunctionName, ArrayRef<Identifier> spiGroups,
                 ArrayRef<AvailableAttr *> availabilityAttrs,
                 size_t typeErasedParamsCount);

public:
  size_t numTrailingObjects(OverloadToken<Identifier>) const {
    return numSPIGroups;
  }

  size_t numTrailingObjects(OverloadToken<AvailableAttr *>) const {
    return numAvailableAttrs;
  }
  // Helper to get the trailing objects of one of the subclasses.
  template<typename Type>
  const Type *getSubclassTrailingObjects() const;

  template<typename Type>
  Type *getSubclassTrailingObjects() {
    const auto *constThis = this;
    return const_cast<Type*>(constThis->getSubclassTrailingObjects<Type>());
  }

  /// Name of SPIs declared by the attribute.
  ///
  /// Note: A single SPI name per attribute is currently supported but this
  /// may change with the syntax change.
  ArrayRef<Identifier> getSPIGroups() const {
    return { getSubclassTrailingObjects<Identifier>(),
             numSPIGroups };
  }

  ArrayRef<AvailableAttr *> getAvailableAttrs() const {
    return {getSubclassTrailingObjects<AvailableAttr *>(),
            numAvailableAttrs};
  }

  ArrayRef<Type> getTypeErasedParams() const {
    if (!typeErasedParamsInitialized)
      return {};

    return {getSubclassTrailingObjects<Type>(),
            numTypeErasedParams};
  }

  void setTypeErasedParams(const ArrayRef<Type> typeErasedParams) {
    assert(typeErasedParams.size() == numTypeErasedParams);
    if (!typeErasedParamsInitialized) {
      std::uninitialized_copy(typeErasedParams.begin(), typeErasedParams.end(),
                              getSubclassTrailingObjects<Type>());
      typeErasedParamsInitialized = true;
    }
  }

  void setResolver(LazyMemberLoader *resolver, uint64_t resolverContextData) {
    this->resolver = resolver;
    this->resolverContextData = resolverContextData;
  }

  TrailingWhereClause *getTrailingWhereClause() const;

  bool isPublic() const {
    return getKind() == DeclAttrKind::Specialized;
  }

  bool isExported() const {
    return Bits.AbstractSpecializeAttr.exported;
  }

  SpecializationKind getSpecializationKind() const {
    return SpecializationKind(Bits.AbstractSpecializeAttr.kind);
  }

  bool isFullSpecialization() const {
    return getSpecializationKind() == SpecializationKind::Full;
  }

  bool isPartialSpecialization() const {
    return getSpecializationKind() == SpecializationKind::Partial;
  }

  DeclNameRef getTargetFunctionName() const {
    return targetFunctionName;
  }

  /// \p forDecl is the value decl that the attribute belongs to.
  ValueDecl *getTargetFunctionDecl(const ValueDecl *forDecl) const;

  /// \p forDecl is the value decl that the attribute belongs to.
  GenericSignature
  getSpecializedSignature(const AbstractFunctionDecl *forDecl) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Specialize ||
      DA->getKind() == DeclAttrKind::Specialized;
  }

  UNIMPLEMENTED_CLONE(AbstractSpecializeAttr)

  bool isEquivalent(const AbstractSpecializeAttr *other, Decl *attachedTo) const;
};

/// The @_specialize attribute.
class SpecializeAttr final : public AbstractSpecializeAttr,
   private SpecializeAttrTrailingObjects<SpecializeAttr> {
  friend TrailingObjects;
  friend AbstractSpecializeAttr;

  // WARNING: Do not add storage here. The base class uses TrailingObjects.
private:
  SpecializeAttr(SourceLoc atLoc, SourceRange Range,
                 TrailingWhereClause *clause,
                 bool exported,
                 SpecializationKind kind, GenericSignature specializedSignature,
                 DeclNameRef targetFunctionName, ArrayRef<Identifier> spiGroups,
                 ArrayRef<AvailableAttr *> availabilityAttrs,
                 size_t typeErasedParamsCount) :
    AbstractSpecializeAttr(DeclAttrKind::Specialize, atLoc, Range, clause,
                   exported, kind, specializedSignature, targetFunctionName,
                   spiGroups, availabilityAttrs, typeErasedParamsCount) {}

public:
  static SpecializeAttr *
  create(ASTContext &Ctx, SourceLoc atLoc, SourceRange Range,
         TrailingWhereClause *clause, bool exported,
         SpecializationKind kind,
         DeclNameRef targetFunctionName, ArrayRef<Identifier> spiGroups,
         ArrayRef<AvailableAttr *> availabilityAttrs,
         GenericSignature specializedSignature = nullptr);

  static SpecializeAttr *create(ASTContext &ctx, bool exported,
                                SpecializationKind kind,
                                ArrayRef<Identifier> spiGroups,
                                ArrayRef<AvailableAttr *> availabilityAttrs,
                                GenericSignature specializedSignature,
                                DeclNameRef replacedFunction);

  static SpecializeAttr *create(ASTContext &ctx, bool exported,
                                SpecializationKind kind,
                                ArrayRef<Identifier> spiGroups,
                                ArrayRef<AvailableAttr *> availabilityAttrs,
                                ArrayRef<Type> typeErasedParams,
                                GenericSignature specializedSignature,
                                DeclNameRef replacedFunction,
                                LazyMemberLoader *resolver, uint64_t data);

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Specialize;
  }

  UNIMPLEMENTED_CLONE(SpecializeAttr)

  bool isEquivalent(const SpecializeAttr *other, Decl *attachedTo) const {
    return AbstractSpecializeAttr::isEquivalent(other, attachedTo);
  }
};

/// The @specialized attribute.
class SpecializedAttr final : public AbstractSpecializeAttr ,
   private SpecializeAttrTrailingObjects<SpecializeAttr> {
  friend TrailingObjects;
  friend AbstractSpecializeAttr;

  // WARNING: Do not add storage here. The base class uses TrailingObjects.
private:

  SpecializedAttr(SourceLoc atLoc, SourceRange Range,
                 TrailingWhereClause *clause,
                 bool exported,
                 SpecializationKind kind, GenericSignature specializedSignature,
                 DeclNameRef targetFunctionName, ArrayRef<Identifier> spiGroups,
                 ArrayRef<AvailableAttr *> availabilityAttrs,
                 size_t typeErasedParamsCount) :
    AbstractSpecializeAttr(DeclAttrKind::Specialized, atLoc, Range, clause,
                   exported, kind, specializedSignature, targetFunctionName,
                   spiGroups, availabilityAttrs, typeErasedParamsCount) {}

public:
  static SpecializedAttr *
  create(ASTContext &Ctx, SourceLoc atLoc, SourceRange Range,
         TrailingWhereClause *clause, bool exported,
         SpecializationKind kind,
         DeclNameRef targetFunctionName, ArrayRef<Identifier> spiGroups,
         ArrayRef<AvailableAttr *> availabilityAttrs,
         GenericSignature specializedSignature = nullptr);

  static SpecializedAttr *create(ASTContext &ctx, bool exported,
                                SpecializationKind kind,
                                ArrayRef<Identifier> spiGroups,
                                ArrayRef<AvailableAttr *> availabilityAttrs,
                                GenericSignature specializedSignature,
                                DeclNameRef replacedFunction);

  static SpecializedAttr *create(ASTContext &ctx, bool exported,
                                SpecializationKind kind,
                                ArrayRef<Identifier> spiGroups,
                                ArrayRef<AvailableAttr *> availabilityAttrs,
                                ArrayRef<Type> typeErasedParams,
                                GenericSignature specializedSignature,
                                DeclNameRef replacedFunction,
                                LazyMemberLoader *resolver, uint64_t data);

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Specialized;
  }

  UNIMPLEMENTED_CLONE(SpecializedAttr)

  bool isEquivalent(const SpecializedAttr *other, Decl *attachedTo) const {
    return AbstractSpecializeAttr::isEquivalent(other, attachedTo);
  }
};

template<typename Type>
const Type *AbstractSpecializeAttr::getSubclassTrailingObjects() const {
  if (auto attr = dyn_cast<SpecializedAttr>(this)) {
    return attr->getTrailingObjects<Type>();
  }
  if (auto attr = dyn_cast<SpecializeAttr>(this)) {
    return attr->getTrailingObjects<Type>();
  }
  llvm_unreachable("unhandled AbstractSpecializeAttr subclass?");
}



class StorageRestrictionsAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<StorageRestrictionsAttr, Identifier> {
  friend TrailingObjects;

  size_t NumInitializes;
  size_t NumAccesses;

  size_t numTrailingObjects(OverloadToken<Identifier>) const {
    return NumInitializes + NumAccesses;
  }

public:
  StorageRestrictionsAttr(SourceLoc AtLoc, SourceRange Range,
                          ArrayRef<Identifier> initializes,
                          ArrayRef<Identifier> accesses, bool Implicit);

  unsigned getNumInitializesProperties() const { return NumInitializes; }

  unsigned getNumAccessesProperties() const { return NumAccesses; }

  ArrayRef<Identifier> getInitializesNames() const {
    return {getTrailingObjects<Identifier>(), NumInitializes};
  }

  ArrayRef<Identifier> getAccessesNames() const {
    return {getTrailingObjects<Identifier>() + NumInitializes, NumAccesses};
  }

  ArrayRef<VarDecl *> getInitializesProperties(AccessorDecl *attachedTo) const;
  ArrayRef<VarDecl *> getAccessesProperties(AccessorDecl *attachedTo) const;

  static StorageRestrictionsAttr *create(ASTContext &ctx, SourceLoc atLoc,
                                         SourceRange range,
                                         ArrayRef<Identifier> initializes,
                                         ArrayRef<Identifier> accesses);

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::StorageRestrictions;
  }

  UNIMPLEMENTED_CLONE(StorageRestrictionsAttr)

  bool isEquivalent(const StorageRestrictionsAttr *other,
                    Decl *attachedTo) const;
};

/// The @_implements attribute, which treats a decl as the implementation for
/// some named protocol requirement (but otherwise not-visible by that name).
class ImplementsAttr : public DeclAttribute {
  /// If constructed by the \c create() variant with a TypeRepr, the TypeRepr;
  /// if constructed by the \c create() variant with a DeclContext and
  /// ProtocolDecl, the DeclContext.
  llvm::PointerUnion<TypeRepr *, DeclContext *> TyROrDC;
  DeclName MemberName;
  DeclNameLoc MemberNameLoc;

  ImplementsAttr(SourceLoc atLoc, SourceRange Range,
                 llvm::PointerUnion<TypeRepr *, DeclContext *> TyROrDC,
                 DeclName MemberName, DeclNameLoc MemberNameLoc);

public:
  static ImplementsAttr *create(ASTContext &Ctx, SourceLoc atLoc,
                                SourceRange Range,
                                TypeRepr *TyR,
                                DeclName MemberName,
                                DeclNameLoc MemberNameLoc);

  static ImplementsAttr *create(DeclContext *DC,
                                ProtocolDecl *Proto,
                                DeclName MemberName);

  ProtocolDecl *getProtocol(DeclContext *dc) const;

  /// Returns the protocol declaration containing the requirement being
  /// implemented by the attributed declaration if it has already been computed,
  /// otherwise `nullopt`. This should only be used for dumping.
  std::optional<ProtocolDecl *> getCachedProtocol(DeclContext *dc) const;

  TypeRepr *getProtocolTypeRepr() const {
    return TyROrDC.dyn_cast<TypeRepr *>();
  }

  DeclName getMemberName() const { return MemberName; }
  DeclNameLoc getMemberNameLoc() const { return MemberNameLoc; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Implements;
  }

  /// Create a copy of this attribute.
  ImplementsAttr *clone(ASTContext &ctx) const {
    if (auto tyR = getProtocolTypeRepr()) {
      return create(ctx, AtLoc, Range, tyR, getMemberName(),
                    getMemberNameLoc());
    }
    auto dc = TyROrDC.dyn_cast<DeclContext *>();
    return create(dc, getProtocol(dc), getMemberName());
  }

  bool isEquivalent(const ImplementsAttr *other, Decl *attachedTo) const;
};

/// A limited variant of \c \@objc that's used for classes with generic ancestry.
class ObjCRuntimeNameAttr : public DeclAttribute {
  static StringRef getSimpleName(const ObjCAttr &Original) {
    assert(Original.hasName());
    return Original.getName()->getSimpleName().str();
  }
public:
  ObjCRuntimeNameAttr(StringRef Name, SourceLoc AtLoc, SourceRange Range,
                      bool Implicit)
      : DeclAttribute(DeclAttrKind::ObjCRuntimeName, AtLoc, Range, Implicit),
        Name(Name) {}

  explicit ObjCRuntimeNameAttr(const ObjCAttr &Original)
    : ObjCRuntimeNameAttr(getSimpleName(Original), Original.AtLoc,
                          Original.Range, Original.isImplicit()) {}

  const StringRef Name;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ObjCRuntimeName;
  }

  /// Create a copy of this attribute.
  ObjCRuntimeNameAttr *clone(ASTContext &ctx) const {
    return new (ctx) ObjCRuntimeNameAttr(Name, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const ObjCRuntimeNameAttr *other, Decl *attachedTo) const {
    return Name == other->Name;
  }
};

/// Attribute that specifies a protocol conformance that has been restated
/// (i.e., is redundant) but should still be emitted in Objective-C metadata.
class RestatedObjCConformanceAttr : public DeclAttribute {
public:
  explicit RestatedObjCConformanceAttr(ProtocolDecl *proto)
      : DeclAttribute(DeclAttrKind::RestatedObjCConformance, SourceLoc(),
                      SourceRange(),
                      /*Implicit=*/true),
        Proto(proto) {}

  /// The protocol to which this type conforms.
  ProtocolDecl * const Proto;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::RestatedObjCConformance;
  }

  /// Create a copy of this attribute.
  RestatedObjCConformanceAttr *clone(ASTContext &ctx) const {
    return new (ctx) RestatedObjCConformanceAttr(Proto);
  }

  bool isEquivalent(const RestatedObjCConformanceAttr *other,
                    Decl *attachedTo) const {
    return Proto == other->Proto;
  }
};

/// Attached to type declarations synthesized by the Clang importer.
///
/// Used to control manglings.
class ClangImporterSynthesizedTypeAttr : public DeclAttribute {
public:
  // NOTE: When adding new kinds, you must update the inline bitfield macro.
  enum class Kind : char {
    /// A struct synthesized by the importer to represent an NSError with a
    /// particular domain, as specified by an enum with the \c ns_error_domain
    /// Clang attribute.
    ///
    /// This one is for enums with names.
    NSErrorWrapper,

    /// A struct synthesized by the importer to represent an NSError with a
    /// particular domain, as specified by an enum with the \c ns_error_domain
    /// Clang attribute.
    ///
    /// This one is for anonymous enums that are immediately typedef'd, giving
    /// them a unique name for linkage purposes according to the C++ standard.
    NSErrorWrapperAnon,
  };

  /// The (Clang) name of the declaration that caused this type declaration to
  /// be synthesized.
  ///
  /// Must be a valid Swift identifier as well, for mangling purposes.
  const StringRef originalTypeName;

  explicit ClangImporterSynthesizedTypeAttr(StringRef originalTypeName,
                                            Kind kind)
      : DeclAttribute(DeclAttrKind::ClangImporterSynthesizedType, SourceLoc(),
                      SourceRange(), /*Implicit=*/true),
        originalTypeName(originalTypeName) {
    assert(!originalTypeName.empty());
    Bits.ClangImporterSynthesizedTypeAttr.kind = unsigned(kind);
  }

  Kind getKind() const {
    return Kind(Bits.ClangImporterSynthesizedTypeAttr.kind);
  }

  StringRef getManglingName() const {
    return manglingNameForKind(getKind());
  }

  static StringRef manglingNameForKind(Kind kind) {
    switch (kind) {
    case Kind::NSErrorWrapper:
      return "e";
    case Kind::NSErrorWrapperAnon:
      return "E";
    }
    llvm_unreachable("unhandled kind");
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ClangImporterSynthesizedType;
  }

  /// Create a copy of this attribute.
  ClangImporterSynthesizedTypeAttr *clone(ASTContext &ctx) const {
    return new (ctx) ClangImporterSynthesizedTypeAttr(
        originalTypeName, getKind());
  }

  bool isEquivalent(const ClangImporterSynthesizedTypeAttr *other,
                    Decl *attachedTo) const {
    return getKind() == other->getKind()
            && originalTypeName == other->originalTypeName;
  }
};

/// Defines a custom attribute.
class CustomAttr final : public DeclAttribute {
  TypeExpr *typeExpr;
  ArgumentList *argList;
  CustomAttributeInitializer *initContext;
  Expr *semanticInit = nullptr;

  mutable unsigned isArgUnsafeBit : 1;

  CustomAttr(SourceLoc atLoc, SourceRange range, TypeExpr *type,
             CustomAttributeInitializer *initContext, ArgumentList *argList,
             bool implicit);

public:
  static CustomAttr *create(ASTContext &ctx, SourceLoc atLoc, TypeExpr *type,
                            bool implicit = false) {
    return create(ctx, atLoc, type, /*initContext*/ nullptr,
                  /*argList*/ nullptr, implicit);
  }

  static CustomAttr *create(ASTContext &ctx, SourceLoc atLoc, TypeExpr *type,
                            CustomAttributeInitializer *initContext,
                            ArgumentList *argList, bool implicit = false);

  TypeExpr *getTypeExpr() const { return typeExpr; }
  TypeRepr *getTypeRepr() const;
  Type getType() const;

  /// Destructure an attribute's type repr for a macro reference.
  ///
  /// For a 1-level member type repr whose base and member are both identifier
  /// types, e.g. `Foo.Bar`, return a pair of the base and the member.
  ///
  /// For an identifier type repr, return a pair of `nullptr` and the
  /// identifier.
  std::pair<UnqualifiedIdentTypeRepr *, DeclRefTypeRepr *>
  destructureMacroRef();

  /// Whether the attribute has any arguments.
  bool hasArgs() const { return argList != nullptr; }

  /// The argument list of the attribute if it has any arguments, \c nullptr
  /// otherwise.
  ArgumentList *getArgs() const { return argList; }
  void setArgs(ArgumentList *newArgs) { argList = newArgs; }

  /// Determine whether the argument is '(unsafe)', a special subexpression
  /// used by global actors.
  bool isArgUnsafe() const;
  void setArgIsUnsafe(bool unsafe) { isArgUnsafeBit = unsafe; }

  Expr *getSemanticInit() const { return semanticInit; }
  void setSemanticInit(Expr *expr) { semanticInit = expr; }

  CustomAttributeInitializer *getInitContext() const { return initContext; }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Custom;
  }

  /// Create a copy of this attribute.
  CustomAttr *clone(ASTContext &ctx) const {
    assert(argList == nullptr &&
           "Cannot clone custom attribute with an argument list");
    return create(ctx, AtLoc, getTypeExpr(), initContext, argList, isImplicit());
  }

  bool canClone() const { return argList == nullptr; }

  bool isEquivalent(const CustomAttr *other, Decl *attachedTo) const;

private:
  friend class CustomAttrNominalRequest;
  void resetTypeInformation(TypeExpr *repr);

private:
  friend class CustomAttrTypeRequest;
  void setType(Type ty);
};

/// Relates a property to its projection value property, as described by a property wrapper. For
/// example, given
/// \code
/// @A var foo: Int
/// \endcode
///
/// Where \c A is a property wrapper that has a \c projectedValue property, the compiler
/// synthesizes a declaration $foo an attaches the attribute
/// \c _projectedValuePropertyAttr($foo) to \c foo to record the link.
class ProjectedValuePropertyAttr : public DeclAttribute {
public:
  ProjectedValuePropertyAttr(Identifier PropertyName, SourceLoc AtLoc,
                             SourceRange Range, bool Implicit)
      : DeclAttribute(DeclAttrKind::ProjectedValueProperty, AtLoc, Range,
                      Implicit),
        ProjectionPropertyName(PropertyName) {}

  // The projection property name.
  const Identifier ProjectionPropertyName;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ProjectedValueProperty;
  }

  /// Create a copy of this attribute.
  ProjectedValuePropertyAttr *clone(ASTContext &ctx) const {
    return new (ctx) ProjectedValuePropertyAttr(
        ProjectionPropertyName, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const ProjectedValuePropertyAttr *other,
                    Decl *attachedTo) const {
    return ProjectionPropertyName == other->ProjectionPropertyName;
  }
};

/// Describes a symbol that was originally defined in another module. For
/// example, given the following declaration:
///
/// \code
/// @_originallyDefinedIn(module: "Original", OSX 10.15) var foo: Int
/// \endcode
///
/// The variable \p foo was originally defined in another module called
/// \p Original prior to OSX 10.15
class OriginallyDefinedInAttr: public DeclAttribute {
public:
  OriginallyDefinedInAttr(SourceLoc AtLoc, SourceRange Range,
                          StringRef OriginalModuleName,
                          PlatformKind Platform,
                          const llvm::VersionTuple MovedVersion, bool Implicit);

  OriginallyDefinedInAttr(SourceLoc AtLoc, SourceRange Range,
                          StringRef ManglingModuleName,
                          StringRef LinkerModuleName,
                          PlatformKind Platform,
                          const llvm::VersionTuple MovedVersion, bool Implicit)
      : DeclAttribute(DeclAttrKind::OriginallyDefinedIn, AtLoc, Range,
                      Implicit),
        ManglingModuleName(ManglingModuleName),
        LinkerModuleName(LinkerModuleName),
        Platform(Platform),
        MovedVersion(MovedVersion) {}

  OriginallyDefinedInAttr *clone(ASTContext &C, bool implicit) const;

  // The original module name for mangling.
  const StringRef ManglingModuleName;

  // The original module name for linker directives.
  const StringRef LinkerModuleName;

  /// The platform of the symbol.
  const PlatformKind Platform;

  /// Indicates when the symbol was moved here.
  const llvm::VersionTuple MovedVersion;

  struct ActiveVersion {
    StringRef ManglingModuleName;
    StringRef LinkerModuleName;
    PlatformKind Platform;
    llvm::VersionTuple Version;
    bool ForTargetVariant = false;
  };

  /// Returns non-optional if this attribute is active given the current platform.
  /// The value provides more details about the active platform.
  std::optional<ActiveVersion> isActivePlatform(const ASTContext &ctx) const;
  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::OriginallyDefinedIn;
  }

  /// Create a copy of this attribute.
  OriginallyDefinedInAttr *clone(ASTContext &ctx) const {
    return new (ctx) OriginallyDefinedInAttr(
        AtLoc, Range, ManglingModuleName, LinkerModuleName,
        Platform, MovedVersion, isImplicit());
  }

  bool isEquivalent(const OriginallyDefinedInAttr *other,
                    Decl *attachedTo) const {
    return ManglingModuleName == other->ManglingModuleName
            && LinkerModuleName == other->LinkerModuleName
            && Platform == other->Platform
            && MovedVersion == other->MovedVersion;
  }
};

/// Attribute that marks a function as differentiable.
///
/// Examples:
///   @differentiable(reverse)
///   @differentiable(reverse, wrt: (self, x, y))
///   @differentiable(reverse, wrt: (self, x, y) where T : FloatingPoint)
class DifferentiableAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<DifferentiableAttr,
                                    ParsedAutoDiffParameter> {
  friend TrailingObjects;
  friend class DifferentiableAttributeTypeCheckRequest;

  /// The declaration on which the `@differentiable` attribute is declared.
  /// May not be a valid declaration for `@differentiable` attributes.
  /// Resolved during parsing and deserialization.
  Decl *OriginalDeclaration = nullptr;
  /// The differentiability kind.
  DifferentiabilityKind DifferentiabilityKind;
  /// The number of parsed differentiability parameters specified in 'wrt:'.
  unsigned NumParsedParameters = 0;
  /// The differentiability parameter indices, resolved by the type checker.
  /// The bit stores whether the parameter indices have been computed.
  ///
  /// Note: it is necessary to use a bit instead of `nullptr` parameter indices
  /// to represent "parameter indices not yet type-checked" because invalid
  /// attributes have `nullptr` parameter indices but have been type-checked.
  llvm::PointerIntPair<IndexSubset *, 1, bool> ParameterIndicesAndBit;
  /// The trailing where clause (optional).
  TrailingWhereClause *WhereClause = nullptr;
  /// The generic signature for autodiff associated functions. Resolved by the
  /// type checker based on the original function's generic signature and the
  /// attribute's where clause requirements. This is set only if the attribute
  /// has a where clause.
  GenericSignature DerivativeGenericSignature;
  /// The source location of the implicitly inherited protocol requirement
  /// `@differentiable` attribute. Used for diagnostics, not serialized.
  ///
  /// This is set during conformance type-checking, only for implicit
  /// `@differentiable` attributes created for non-public protocol witnesses of
  /// protocol requirements with `@differentiable` attributes.
  SourceLoc ImplicitlyInheritedDifferentiableAttrLocation;

  explicit DifferentiableAttr(bool implicit, SourceLoc atLoc,
                              SourceRange baseRange,
                              enum DifferentiabilityKind diffKind,
                              ArrayRef<ParsedAutoDiffParameter> parameters,
                              TrailingWhereClause *clause);

  explicit DifferentiableAttr(Decl *original, bool implicit, SourceLoc atLoc,
                              SourceRange baseRange,
                              enum DifferentiabilityKind diffKind,
                              IndexSubset *parameterIndices,
                              GenericSignature derivativeGenericSignature);

public:
  static DifferentiableAttr *create(ASTContext &context, bool implicit,
                                    SourceLoc atLoc, SourceRange baseRange,
                                    enum DifferentiabilityKind diffKind,
                                    ArrayRef<ParsedAutoDiffParameter> params,
                                    TrailingWhereClause *clause);

  static DifferentiableAttr *create(AbstractFunctionDecl *original,
                                    bool implicit, SourceLoc atLoc,
                                    SourceRange baseRange,
                                    enum DifferentiabilityKind diffKind,
                                    IndexSubset *parameterIndices,
                                    GenericSignature derivativeGenSig);

  Decl *getOriginalDeclaration() const { return OriginalDeclaration; }

  /// Sets the original declaration on which this attribute is declared.
  /// Should only be used by parsing and deserialization.
  void setOriginalDeclaration(Decl *originalDeclaration);

private:
  /// Returns true if the given `@differentiable` attribute has been
  /// type-checked.
  bool hasBeenTypeChecked() const;

public:
  IndexSubset *getParameterIndices() const;
  void setParameterIndices(IndexSubset *parameterIndices);

  /// The parsed differentiability parameters, i.e. the list of parameters
  /// specified in 'wrt:'.
  ArrayRef<ParsedAutoDiffParameter> getParsedParameters() const {
    return {getTrailingObjects<ParsedAutoDiffParameter>(), NumParsedParameters};
  }
  MutableArrayRef<ParsedAutoDiffParameter> getParsedParameters() {
    return {getTrailingObjects<ParsedAutoDiffParameter>(), NumParsedParameters};
  }
  size_t numTrailingObjects(OverloadToken<ParsedAutoDiffParameter>) const {
    return NumParsedParameters;
  }

  enum DifferentiabilityKind getDifferentiabilityKind() const {
    return DifferentiabilityKind;
  }

  bool isNormalDifferentiability() const {
    return DifferentiabilityKind == DifferentiabilityKind::Normal;
  }

  bool isLinearDifferentiability() const {
    return DifferentiabilityKind == DifferentiabilityKind::Linear;
  }

  bool isForwardDifferentiability() const {
    return DifferentiabilityKind == DifferentiabilityKind::Forward;
  }

  bool isReverseDifferentiability() const {
    return DifferentiabilityKind == DifferentiabilityKind::Reverse;
  }

  TrailingWhereClause *getWhereClause() const { return WhereClause; }

  GenericSignature getDerivativeGenericSignature() const {
    return DerivativeGenericSignature;
  }
  void setDerivativeGenericSignature(GenericSignature derivativeGenSig) {
    DerivativeGenericSignature = derivativeGenSig;
  }

  SourceLoc getImplicitlyInheritedDifferentiableAttrLocation() const {
    return ImplicitlyInheritedDifferentiableAttrLocation;
  }
  void getImplicitlyInheritedDifferentiableAttrLocation(SourceLoc loc) {
    assert(isImplicit());
    ImplicitlyInheritedDifferentiableAttrLocation = loc;
  }

  /// Get the derivative generic environment for the given `@differentiable`
  /// attribute and original function.
  GenericEnvironment *
  getDerivativeGenericEnvironment(AbstractFunctionDecl *original) const;

  // Print the attribute to the given stream.
  // If `omitWrtClause` is true, omit printing the `wrt:` clause.
  void print(llvm::raw_ostream &OS, const Decl *D, bool omitWrtClause = false) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Differentiable;
  }

  UNIMPLEMENTED_CLONE(DifferentiableAttr)

  bool isEquivalent(const DifferentiableAttr *other, Decl *attachedTo) const {
    // Not properly implemented (very complex and not currently needed)
    return false;
  }
};

/// A declaration name with location.
struct DeclNameRefWithLoc {
  /// The declaration name.
  DeclNameRef Name;
  /// The declaration name location.
  DeclNameLoc Loc;
  /// An optional accessor kind.
  std::optional<AccessorKind> AccessorKind;

  void print(ASTPrinter &Printer) const;
};

/// The `@derivative(of:)` attribute registers a function as a derivative of
/// another function-like declaration: a 'func', 'init', 'subscript', or 'var'
/// computed property declaration.
///
/// The `@derivative(of:)` attribute also has an optional `wrt:` clause
/// specifying the parameters that are differentiated "with respect to", i.e.
/// the differentiability parameters. The differentiability parameters must
/// conform to the `Differentiable` protocol.
///
/// If the `wrt:` clause is unspecified, the differentiability parameters are
/// inferred to be all parameters that conform to `Differentiable`.
///
/// `@derivative(of:)` attribute type-checking verifies that the type of the
/// derivative function declaration is consistent with the type of the
/// referenced original declaration and the differentiability parameters.
///
/// Examples:
///   @derivative(of: sin(_:))
///   @derivative(of: +, wrt: (lhs, rhs))
class DerivativeAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<DerivativeAttr, ParsedAutoDiffParameter> {
  friend TrailingObjects;
  friend class DerivativeAttrOriginalDeclRequest;

  /// The declaration on which the `@derivative` attribute is declared.
  /// May not be a valid declaration for `@derivative` attributes.
  /// Resolved during parsing and deserialization.
  Decl *OriginalDeclaration = nullptr;
  /// The base type for the referenced original declaration. This field is
  /// non-null only for parsed attributes that reference a qualified original
  /// declaration. This field is not serialized; type-checking uses it to
  /// resolve the original declaration, which is serialized.
  TypeRepr *BaseTypeRepr;
  /// The original function name.
  DeclNameRefWithLoc OriginalFunctionName;
  /// The original function.
  ///
  /// The states are:
  /// - nullptr:
  ///   The original function is unknown. The typechecker is responsible for
  ///   eventually resolving it.
  /// - AbstractFunctionDecl:
  ///   The original function is known to be this `AbstractFunctionDecl`.
  /// - LazyMemberLoader:
  ///   This `LazyMemberLoader` knows how to resolve the original function.
  ///   `ResolverContextData` is an additional piece of data that the
  ///   `LazyMemberLoader` needs.
  // TODO(TF-1235): Making `DerivativeAttr` immutable will simplify this by
  // removing the `AbstractFunctionDecl` state.
  llvm::PointerUnion<AbstractFunctionDecl *, LazyMemberLoader *> OriginalFunction;
  /// Data representing the original function declaration. See doc comment for
  /// `OriginalFunction`.
  uint64_t ResolverContextData = 0;
  /// The number of parsed differentiability parameters specified in 'wrt:'.
  unsigned NumParsedParameters = 0;
  /// The differentiability parameter indices, resolved by the type checker.
  IndexSubset *ParameterIndices = nullptr;
  /// The derivative function kind (JVP or VJP), resolved by the type checker.
  std::optional<AutoDiffDerivativeFunctionKind> Kind = std::nullopt;

  explicit DerivativeAttr(bool implicit, SourceLoc atLoc, SourceRange baseRange,
                          TypeRepr *baseTypeRepr, DeclNameRefWithLoc original,
                          ArrayRef<ParsedAutoDiffParameter> params);

  explicit DerivativeAttr(bool implicit, SourceLoc atLoc, SourceRange baseRange,
                          TypeRepr *baseTypeRepr, DeclNameRefWithLoc original,
                          IndexSubset *parameterIndices);

public:
  static DerivativeAttr *create(ASTContext &context, bool implicit,
                                SourceLoc atLoc, SourceRange baseRange,
                                TypeRepr *baseTypeRepr,
                                DeclNameRefWithLoc original,
                                ArrayRef<ParsedAutoDiffParameter> params);

  static DerivativeAttr *create(ASTContext &context, bool implicit,
                                SourceLoc atLoc, SourceRange baseRange,
                                TypeRepr *baseTypeRepr,
                                DeclNameRefWithLoc original,
                                IndexSubset *parameterIndices);

  Decl *getOriginalDeclaration() const { return OriginalDeclaration; }

  /// Sets the original declaration on which this attribute is declared.
  /// Should only be used by parsing and deserialization.
  void setOriginalDeclaration(Decl *originalDeclaration);

  TypeRepr *getBaseTypeRepr() const { return BaseTypeRepr; }
  DeclNameRefWithLoc getOriginalFunctionName() const {
    return OriginalFunctionName;
  }
  AbstractFunctionDecl *getOriginalFunction(ASTContext &context) const;
  void setOriginalFunction(AbstractFunctionDecl *decl);
  void setOriginalFunctionResolver(LazyMemberLoader *resolver,
                                   uint64_t resolverContextData);

  AutoDiffDerivativeFunctionKind getDerivativeKind() const {
    assert(Kind && "Derivative function kind has not yet been resolved");
    return *Kind;
  }
  void setDerivativeKind(AutoDiffDerivativeFunctionKind kind) { Kind = kind; }

  /// The parsed differentiability parameters, i.e. the list of parameters
  /// specified in 'wrt:'.
  ArrayRef<ParsedAutoDiffParameter> getParsedParameters() const {
    return {getTrailingObjects<ParsedAutoDiffParameter>(), NumParsedParameters};
  }
  MutableArrayRef<ParsedAutoDiffParameter> getParsedParameters() {
    return {getTrailingObjects<ParsedAutoDiffParameter>(), NumParsedParameters};
  }
  size_t numTrailingObjects(OverloadToken<ParsedAutoDiffParameter>) const {
    return NumParsedParameters;
  }

  IndexSubset *getParameterIndices() const {
    return ParameterIndices;
  }
  void setParameterIndices(IndexSubset *parameterIndices) {
    ParameterIndices = parameterIndices;
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Derivative;
  }

  UNIMPLEMENTED_CLONE(DerivativeAttr)

  bool isEquivalent(const DerivativeAttr *other, Decl *attachedTo) const {
    // Not properly implemented (very complex and not currently needed)
    return false;
  }
};

/// The `@transpose(of:)` attribute registers a function as a transpose of
/// another function-like declaration: a 'func', 'init', 'subscript', or 'var'
/// computed property declaration.
///
/// The `@transpose(of:)` attribute also has a `wrt:` clause specifying the
/// parameters that are transposed "with respect to", i.e. the linearity
/// parameters.
///
/// Examples:
///   @transpose(of: foo)
///   @transpose(of: +, wrt: (0, 1))
class TransposeAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<TransposeAttr, ParsedAutoDiffParameter> {
  friend TrailingObjects;

  /// The base type for the referenced original declaration. This field is
  /// non-null only for parsed attributes that reference a qualified original
  /// declaration. This field is not serialized; type-checking uses it to
  /// resolve the original declaration, which is serialized.
  TypeRepr *BaseTypeRepr;
  /// The original function name.
  DeclNameRefWithLoc OriginalFunctionName;
  /// The original function declaration, resolved by the type checker.
  AbstractFunctionDecl *OriginalFunction = nullptr;
  /// The number of parsed linearity parameters specified in 'wrt:'.
  unsigned NumParsedParameters = 0;
  /// The linearity parameter indices, resolved by the type checker.
  IndexSubset *ParameterIndices = nullptr;

  explicit TransposeAttr(bool implicit, SourceLoc atLoc, SourceRange baseRange,
                         TypeRepr *baseType, DeclNameRefWithLoc original,
                         ArrayRef<ParsedAutoDiffParameter> params);

  explicit TransposeAttr(bool implicit, SourceLoc atLoc, SourceRange baseRange,
                         TypeRepr *baseType, DeclNameRefWithLoc original,
                         IndexSubset *parameterIndices);

public:
  static TransposeAttr *create(ASTContext &context, bool implicit,
                               SourceLoc atLoc, SourceRange baseRange,
                               TypeRepr *baseType, DeclNameRefWithLoc original,
                               ArrayRef<ParsedAutoDiffParameter> params);

  static TransposeAttr *create(ASTContext &context, bool implicit,
                               SourceLoc atLoc, SourceRange baseRange,
                               TypeRepr *baseType, DeclNameRefWithLoc original,
                               IndexSubset *parameterIndices);

  TypeRepr *getBaseTypeRepr() const { return BaseTypeRepr; }
  DeclNameRefWithLoc getOriginalFunctionName() const {
    return OriginalFunctionName;
  }
  AbstractFunctionDecl *getOriginalFunction() const {
    return OriginalFunction;
  }
  void setOriginalFunction(AbstractFunctionDecl *decl) {
    OriginalFunction = decl;
  }

  /// The parsed linearity parameters, i.e. the list of parameters specified in
  /// 'wrt:'.
  ArrayRef<ParsedAutoDiffParameter> getParsedParameters() const {
    return {getTrailingObjects<ParsedAutoDiffParameter>(), NumParsedParameters};
  }
  MutableArrayRef<ParsedAutoDiffParameter> getParsedParameters() {
    return {getTrailingObjects<ParsedAutoDiffParameter>(), NumParsedParameters};
  }
  size_t numTrailingObjects(OverloadToken<ParsedAutoDiffParameter>) const {
    return NumParsedParameters;
  }

  IndexSubset *getParameterIndices() const {
    return ParameterIndices;
  }
  void setParameterIndices(IndexSubset *parameterIndices) {
    ParameterIndices = parameterIndices;
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Transpose;
  }

  UNIMPLEMENTED_CLONE(TransposeAttr)

  bool isEquivalent(const TransposeAttr *other, Decl *attachedTo) const {
    // Not properly implemented (very complex and not currently needed)
    return false;
  }
};

enum class NonSendableKind : uint8_t {
  /// A plain '@_nonSendable' attribute. Should be applied directly to
  /// particular declarations; overrides even an explicit 'Sendable'
  /// conformance.
  Specific,

  /// A '@_nonSendable(_assumed)' attribute. Should be applied to large swaths
  /// of declarations; does not override explicit 'Sendable' conformances.
  Assumed
};

/// Marks a declaration as explicitly non-Sendable.
class NonSendableAttr : public DeclAttribute {
public:
  NonSendableAttr(SourceLoc AtLoc, SourceRange Range,
                  NonSendableKind Specificity, bool Implicit = false)
      : DeclAttribute(DeclAttrKind::NonSendable, AtLoc, Range, Implicit),
        Specificity(Specificity) {}

  NonSendableAttr(NonSendableKind Specificity, bool Implicit = false)
    : NonSendableAttr(SourceLoc(), SourceRange(), Specificity, Implicit) {}

  /// Was this '@_nonSendable(_assumed)'?
  const NonSendableKind Specificity;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::NonSendable;
  }

  /// Create a copy of this attribute.
  NonSendableAttr *clone(ASTContext &ctx) const {
    return new (ctx) NonSendableAttr(AtLoc, Range, Specificity, isImplicit());
  }

  bool isEquivalent(const NonSendableAttr *other, Decl *attachedTo) const {
    return Specificity == other->Specificity;
  }
};

/// The @_unavailableFromAsync attribute, used to make function declarations
/// unavailable from async contexts.
class UnavailableFromAsyncAttr : public DeclAttribute {
public:
  UnavailableFromAsyncAttr(StringRef Message, SourceLoc AtLoc,
                           SourceRange Range, bool Implicit)
      : DeclAttribute(DeclAttrKind::UnavailableFromAsync, AtLoc, Range,
                      Implicit),
        Message(Message) {}
  UnavailableFromAsyncAttr(StringRef Message, bool Implicit)
      : UnavailableFromAsyncAttr(Message, SourceLoc(), SourceRange(),
                                 Implicit) {}
  const StringRef Message;

  bool hasMessage() const { return !Message.empty(); }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::UnavailableFromAsync;
  }

  /// Create a copy of this attribute.
  UnavailableFromAsyncAttr *clone(ASTContext &ctx) const {
    return new (ctx) UnavailableFromAsyncAttr(
        Message, AtLoc, Range, isImplicit());
  }

  bool isEquivalent(const UnavailableFromAsyncAttr *other,
                    Decl *attachedTo) const {
    return Message == other->Message;
  }
};

/// The `@backDeployed(...)` attribute, used to make function declarations
/// available for back deployment to older OSes via emission into the client
/// binary.
class BackDeployedAttr : public DeclAttribute {
public:
  BackDeployedAttr(SourceLoc AtLoc, SourceRange Range, PlatformKind Platform,
                   const llvm::VersionTuple &Version, bool Implicit)
      : DeclAttribute(DeclAttrKind::BackDeployed, AtLoc, Range, Implicit),
        Platform(Platform), Version(Version) {}

  /// The platform the symbol is available for back deployment on.
  const PlatformKind Platform;

  /// The earliest platform version that may use the back deployed implementation.
  const llvm::VersionTuple Version;

  /// Returns true if this attribute is active given the current platform.
  bool isActivePlatform(const ASTContext &ctx, bool forTargetVariant) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::BackDeployed;
  }

  /// Create a copy of this attribute.
  BackDeployedAttr *clone(ASTContext &ctx) const {
    return new (ctx) BackDeployedAttr(
        AtLoc, Range, Platform, Version, isImplicit());
  }

  bool isEquivalent(const BackDeployedAttr *other, Decl *attachedTo) const {
    return Platform == other->Platform && Version == Version;
  }
};

/// Defines the `@_expose` attribute, used to expose declarations in the
/// header used by C/C++ to interoperate with Swift.
class ExposeAttr : public DeclAttribute {
public:
  ExposeAttr(StringRef Name, SourceLoc AtLoc, SourceRange Range,
             ExposureKind Kind, bool Implicit)
      : DeclAttribute(DeclAttrKind::Expose, AtLoc, Range, Implicit),
        Name(Name) {
    Bits.ExposeAttr.kind = static_cast<unsigned>(Kind);
  }

  ExposeAttr(StringRef Name, ExposureKind Kind, bool Implicit)
      : ExposeAttr(Name, SourceLoc(), SourceRange(), Kind, Implicit) {}

  /// The exposed declaration name.
  const StringRef Name;

  /// Returns the kind of exposure.
  ExposureKind getExposureKind() const {
    return static_cast<ExposureKind>(Bits.ExposeAttr.kind);
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Expose;
  }

  /// Create a copy of this attribute.
  ExposeAttr *clone(ASTContext &ctx) const {
    return new (ctx) ExposeAttr(
        Name, AtLoc, Range, getExposureKind(), isImplicit());
  }

  bool isEquivalent(const ExposeAttr *other, Decl *attachedTo) const {
    return Name == other->Name && getExposureKind() == other->getExposureKind();
  }
};

/// Define the `@_extern` attribute, used to import external declarations in
/// the specified way to interoperate with Swift.
class ExternAttr : public DeclAttribute {
  SourceLoc LParenLoc, RParenLoc;

public:
  ExternAttr(std::optional<StringRef> ModuleName, std::optional<StringRef> Name,
             SourceLoc AtLoc, SourceLoc LParenLoc, SourceLoc RParenLoc,
             SourceRange Range, ExternKind Kind, bool Implicit)
      : DeclAttribute(DeclAttrKind::Extern, AtLoc, Range, Implicit),
        LParenLoc(LParenLoc), RParenLoc(RParenLoc), ModuleName(ModuleName),
        Name(Name) {
    Bits.ExternAttr.kind = static_cast<unsigned>(Kind);
  }

  ExternAttr(std::optional<StringRef> ModuleName, std::optional<StringRef> Name,
             ExternKind Kind, bool Implicit)
      : ExternAttr(ModuleName, Name, SourceLoc(), SourceLoc(), SourceLoc(),
                   SourceRange(), Kind, Implicit) {}

  /// The module name to import the named declaration in it
  /// Used for Wasm import declaration.
  const std::optional<StringRef> ModuleName;

  /// The declaration name to import
  /// std::nullopt if the declaration name is not specified with @_extern(c)
  const std::optional<StringRef> Name;

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  /// Returns the kind of extern.
  ExternKind getExternKind() const {
    return static_cast<ExternKind>(Bits.ExternAttr.kind);
  }

  /// Returns the C name of the given declaration.
  /// \p forDecl is the func decl that the attribute belongs to.
  StringRef getCName(const FuncDecl *forDecl) const;

  /// Find an ExternAttr with the given kind in the given DeclAttributes.
  static ExternAttr *find(DeclAttributes &attrs, ExternKind kind);

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Extern;
  }

  /// Create a copy of this attribute.
  ExternAttr *clone(ASTContext &ctx) const {
    return new (ctx) ExternAttr(
        ModuleName, Name, AtLoc, getLParenLoc(), getRParenLoc(), Range,
        getExternKind(), isImplicit());
  }

  bool isEquivalent(const ExternAttr *other, Decl *attachedTo) const {
    return getExternKind() == other->getExternKind()
            && ModuleName == other->ModuleName && Name == other->Name;
  }
};

/// The `@_documentation(...)` attribute, used to override a symbol's visibility
/// in symbol graphs, and/or adding arbitrary metadata to it.
class DocumentationAttr: public DeclAttribute {
public:
  DocumentationAttr(SourceLoc AtLoc, SourceRange Range, StringRef Metadata,
                    std::optional<AccessLevel> Visibility, bool Implicit)
      : DeclAttribute(DeclAttrKind::Documentation, AtLoc, Range, Implicit),
        Metadata(Metadata), Visibility(Visibility) {}

  DocumentationAttr(StringRef Metadata, std::optional<AccessLevel> Visibility,
                    bool Implicit)
      : DocumentationAttr(SourceLoc(), SourceRange(), Metadata, Visibility,
                          Implicit) {}

  const StringRef Metadata;
  const std::optional<AccessLevel> Visibility;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Documentation;
  }

  /// Create a copy of this attribute.
  DocumentationAttr *clone(ASTContext &ctx) const {
    return new (ctx) DocumentationAttr(
        AtLoc, Range, Metadata, Visibility, isImplicit());
  }

  bool isEquivalent(const DocumentationAttr *other, Decl *attachedTo) const {
    return Metadata == other->Metadata && Visibility == other->Visibility;
  }
};

class ObjCImplementationAttr final : public DeclAttribute {
public:
  /// Name of the category being implemented. This should only be used with
  /// the early adopter \@\_objcImplementation syntax, but we support it there
  /// for backwards compatibility.
  Identifier CategoryName;

  ObjCImplementationAttr(Identifier CategoryName, SourceLoc AtLoc,
                         SourceRange Range, bool isEarlyAdopter = false,
                         bool Implicit = false,
                         bool isCategoryNameInvalid = false)
      : DeclAttribute(DeclAttrKind::ObjCImplementation, AtLoc, Range, Implicit),
        CategoryName(CategoryName) {
    Bits.ObjCImplementationAttr.isCategoryNameInvalid = isCategoryNameInvalid;
    Bits.ObjCImplementationAttr.hasInvalidImplicitLangAttrs = false;
    Bits.ObjCImplementationAttr.isEarlyAdopter = isEarlyAdopter;
  }

  /// Early adopters use the \c \@_objcImplementation spelling. For backwards
  /// compatibility, issues with them are diagnosed as warnings, not errors.
  bool isEarlyAdopter() const {
    return Bits.ObjCImplementationAttr.isEarlyAdopter;
  }

  bool isCategoryNameInvalid() const {
    return Bits.ObjCImplementationAttr.isCategoryNameInvalid;
  }

  void setCategoryNameInvalid(bool newValue = true) {
    Bits.ObjCImplementationAttr.isCategoryNameInvalid = newValue;
  }

  /// Has at least one implicitly ObjC member failed to validate? If so,
  /// diagnostics that might be duplicative will be suppressed.
  bool hasInvalidImplicitLangAttrs() const {
    return Bits.ObjCImplementationAttr.hasInvalidImplicitLangAttrs;
  }

  void setHasInvalidImplicitLangAttrs(bool newValue = true) {
    Bits.ObjCImplementationAttr.hasInvalidImplicitLangAttrs = newValue;
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ObjCImplementation;
  }

  /// Create a copy of this attribute.
  ObjCImplementationAttr *clone(ASTContext &ctx) const {
    return new (ctx) ObjCImplementationAttr(
        CategoryName, AtLoc, Range, isEarlyAdopter(), isImplicit(),
        isCategoryNameInvalid());
  }

  bool isEquivalent(const ObjCImplementationAttr *other,
                    Decl *attachedTo) const {
    // Ignoring `CategoryName` as it gets copied to @objc
    return isEarlyAdopter() == other->isEarlyAdopter();
  }
};

/// Represents nonisolated modifier.
class NonisolatedAttr final : public DeclAttribute {
public:
  NonisolatedAttr(SourceLoc atLoc, SourceRange range,
                  NonIsolatedModifier modifier, bool implicit)
      : DeclAttribute(DeclAttrKind::Nonisolated, atLoc, range, implicit) {
    Bits.NonisolatedAttr.Modifier = static_cast<unsigned>(modifier);
    assert((getModifier() == modifier) && "not enough bits for modifier");
  }

  NonIsolatedModifier getModifier() const {
    return static_cast<NonIsolatedModifier>(Bits.NonisolatedAttr.Modifier);
  }

  bool isUnsafe() const { return getModifier() == NonIsolatedModifier::Unsafe; }
  bool isNonSending() const {
    return getModifier() == NonIsolatedModifier::NonSending;
  }

  static NonisolatedAttr *
  createImplicit(ASTContext &ctx,
                 NonIsolatedModifier modifier = NonIsolatedModifier::None) {
    return new (ctx) NonisolatedAttr(/*atLoc*/ {}, /*range*/ {}, modifier,
                                     /*implicit=*/true);
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Nonisolated;
  }

  /// Create a copy of this attribute.
  NonisolatedAttr *clone(ASTContext &ctx) const {
    return new (ctx) NonisolatedAttr(AtLoc, Range, getModifier(), isImplicit());
  }

  bool isEquivalent(const NonisolatedAttr *other, Decl *attachedTo) const {
    return getModifier() == other->getModifier();
  }
};

/// Represents @_inheritActorContext modifier.
class InheritActorContextAttr final : public DeclAttribute {
public:
  InheritActorContextAttr(SourceLoc atLoc, SourceRange range,
                          InheritActorContextModifier modifier, bool implicit)
      : DeclAttribute(DeclAttrKind::InheritActorContext, atLoc, range,
                      implicit) {
    Bits.InheritActorContextAttr.Modifier = static_cast<unsigned>(modifier);
    assert((getModifier() == modifier) && "not enough bits for modifier");
  }

  InheritActorContextModifier getModifier() const {
    return static_cast<InheritActorContextModifier>(
        Bits.InheritActorContextAttr.Modifier);
  }

  bool isAlways() const {
    return getModifier() == InheritActorContextModifier::Always;
  }

  static InheritActorContextAttr *
  createImplicit(ASTContext &ctx, InheritActorContextModifier modifier =
                                      InheritActorContextModifier::None) {
    return new (ctx)
        InheritActorContextAttr(/*atLoc*/ {}, /*range*/ {}, modifier,
                                /*implicit=*/true);
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::InheritActorContext;
  }

  /// Create a copy of this attribute.
  InheritActorContextAttr *clone(ASTContext &ctx) const {
    return new (ctx)
        InheritActorContextAttr(AtLoc, Range, getModifier(), isImplicit());
  }

  bool isEquivalent(const InheritActorContextAttr *other,
                    Decl *attachedTo) const {
    return getModifier() == other->getModifier();
  }
};

/// A macro role attribute, spelled with either @attached or @freestanding,
/// which declares one of the roles that a given macro can inhabit.
class MacroRoleAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<MacroRoleAttr, MacroIntroducedDeclName,
                                    Expr *> {
  friend TrailingObjects;

  MacroSyntax syntax;
  MacroRole role;
  unsigned numNames;
  unsigned numConformances;
  SourceLoc lParenLoc, rParenLoc;

  MacroRoleAttr(SourceLoc atLoc, SourceRange range, MacroSyntax syntax,
                SourceLoc lParenLoc, MacroRole role,
                ArrayRef<MacroIntroducedDeclName> names,
                ArrayRef<Expr *> conformances, SourceLoc rParenLoc,
                bool implicit);

public:
  static MacroRoleAttr *create(ASTContext &ctx, SourceLoc atLoc,
                               SourceRange range, MacroSyntax syntax,
                               SourceLoc lParenLoc, MacroRole role,
                               ArrayRef<MacroIntroducedDeclName> names,
                               ArrayRef<Expr *> conformances,
                               SourceLoc rParenLoc, bool implicit);

  size_t numTrailingObjects(OverloadToken<MacroIntroducedDeclName>) const {
    return numNames;
  }

  size_t numTrailingObjects(OverloadToken<Expr *>) const {
    return numConformances;
  }

  SourceLoc getLParenLoc() const { return lParenLoc; }
  SourceLoc getRParenLoc() const { return rParenLoc; }

  MacroSyntax getMacroSyntax() const { return syntax; }
  MacroRole getMacroRole() const { return role; }
  ArrayRef<MacroIntroducedDeclName> getNames() const;
  ArrayRef<Expr *> getConformances() const;
  MutableArrayRef<Expr *> getConformances();
  bool hasNameKind(MacroIntroducedDeclNameKind kind) const;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::MacroRole;
  }

  UNIMPLEMENTED_CLONE(MacroRoleAttr)

  bool isEquivalent(const MacroRoleAttr *other, Decl *attachedTo) const {
    // Unimplemented: complex and not immediately needed.
    return true;
  }
};

/// Specifies the raw storage used by a type.
class RawLayoutAttr final : public DeclAttribute {
  /// The element type to share size and alignment with, if any.
  TypeRepr *LikeType = nullptr;
  /// The number of elements in an array to share stride and alignment with,
  /// or nullptr if no such size was specified.
  TypeRepr *CountType = nullptr;
  /// The size in bytes of the raw storage.
  unsigned Size = 0;
  /// If `LikeType` is null, the alignment in bytes to use for the raw storage.
  unsigned Alignment = 0;
  /// If a value of this raw layout type should move like its `LikeType`.
  bool MovesAsLike = false;
  /// The resolved like type.
  mutable Type CachedResolvedLikeType = Type();
  /// The resolved count type.
  mutable Type CachedResolvedCountType = Type();

  friend class ResolveRawLayoutTypeRequest;

public:
  /// Construct a `@_rawLayout(like: T)` attribute.
  RawLayoutAttr(TypeRepr *LikeType, bool movesAsLike, SourceLoc AtLoc,
                SourceRange Range)
      : DeclAttribute(DeclAttrKind::RawLayout, AtLoc, Range,
                      /*implicit*/ false),
        LikeType(LikeType), MovesAsLike(movesAsLike) {}

  /// Construct a `@_rawLayout(likeArrayOf: T, count: N)` attribute.
  RawLayoutAttr(TypeRepr *LikeType, TypeRepr *CountType, bool movesAsLike,
                SourceLoc AtLoc, SourceRange Range)
      : DeclAttribute(DeclAttrKind::RawLayout, AtLoc, Range,
                      /*implicit*/ false),
        LikeType(LikeType), CountType(CountType), MovesAsLike(movesAsLike) {}

  /// Construct a `@_rawLayout(size: N, alignment: M)` attribute.
  RawLayoutAttr(unsigned Size, unsigned Alignment, SourceLoc AtLoc,
                SourceRange Range)
      : DeclAttribute(DeclAttrKind::RawLayout, AtLoc, Range,
                      /*implicit*/ false),
        Size(Size), Alignment(Alignment) {}

  /// Return the type whose single-element layout the attribute type should get
  /// its layout from. Returns null if the attribute specifies an array or manual
  /// layout.
  TypeRepr *getScalarLikeType() const {
    if (!LikeType || CountType)
      return nullptr;
    return LikeType;
  }
  
  /// Return the type whose array layout the attribute type should get its
  /// layout from, along with the size of that array. Returns None if the
  /// attribute specifies scalar or manual layout.
  std::optional<std::pair<TypeRepr *, TypeRepr *>>
  getArrayLikeTypeAndCount() const {
    if (!LikeType || !CountType)
      return std::nullopt;
    return std::make_pair(LikeType, CountType);
  }

  /// Return the size and alignment of the attributed type. Returns
  /// None if the attribute specifies layout like some other type.
  std::optional<std::pair<unsigned, unsigned>> getSizeAndAlignment() const {
    if (LikeType)
      return std::nullopt;
    return std::make_pair(Size, Alignment);
  }

  Type getResolvedLikeType(StructDecl *sd) const;
  Type getResolvedCountType(StructDecl *sd) const;

  /// Return the type whose single-element layout the attribute type should get
  /// its layout from. Returns None if the attribute specifies an array or manual
  /// layout.
  std::optional<Type> getResolvedScalarLikeType(StructDecl *sd) const {
    if (!LikeType || CountType)
      return std::nullopt;
    return getResolvedLikeType(sd);
  }

  /// Return the type whose array layout the attribute type should get its
  /// layout from, along with the size of that array. Returns None if the
  /// attribute specifies scalar or manual layout.
  std::optional<std::pair<Type, Type>>
  getResolvedArrayLikeTypeAndCount(StructDecl *sd) const {
    if (!LikeType || !CountType)
      return std::nullopt;
    return std::make_pair(getResolvedLikeType(sd), getResolvedCountType(sd));
  }

  /// Whether a value of this raw layout should move like its `LikeType`.
  bool shouldMoveAsLikeType() const {
    return MovesAsLike;
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::RawLayout;
  }

  UNIMPLEMENTED_CLONE(RawLayoutAttr)

  bool isEquivalent(const RawLayoutAttr *other, Decl *attachedTo) const;
};

class LifetimeAttr final : public DeclAttribute {
  LifetimeEntry *entry;

  LifetimeAttr(SourceLoc atLoc, SourceRange baseRange, bool implicit,
               LifetimeEntry *entry, bool isUnderscored)
      : DeclAttribute(DeclAttrKind::Lifetime, atLoc, baseRange, implicit),
        entry(entry) {
    Bits.LifetimeAttr.isUnderscored = isUnderscored;
  }

public:
  static LifetimeAttr *create(ASTContext &context, SourceLoc atLoc,
                              SourceRange baseRange, bool implicit,
                              LifetimeEntry *entry, bool isUnderscored);

  LifetimeEntry *getLifetimeEntry() const { return entry; }

  bool isUnderscored() const { return bool(Bits.LifetimeAttr.isUnderscored); }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::Lifetime;
  }

  /// Create a copy of this attribute.
  LifetimeAttr *clone(ASTContext &ctx) const {
    return new (ctx)
        LifetimeAttr(AtLoc, Range, isImplicit(), entry, isUnderscored());
  }

  std::string getString() const;

  bool isEquivalent(const LifetimeAttr *other, Decl *attachedTo) const;
};

/// Predicate used to filter MatchingAttributeRange.
template <typename ATTR, bool AllowInvalid> struct ToAttributeKind {
  ToAttributeKind() {}

  std::optional<const ATTR *> operator()(const DeclAttribute *Attr) const {
    if (isa<ATTR>(Attr) && (Attr->isValid() || AllowInvalid))
      return cast<ATTR>(Attr);
    return std::nullopt;
  }

  std::optional<ATTR *> operator()(DeclAttribute *Attr) {
    if (isa<ATTR>(Attr) && (Attr->isValid() || AllowInvalid))
      return cast<ATTR>(Attr);
    return std::nullopt;
  }
};

/// The @_allowFeatureSuppression(Foo, Bar) attribute.  The feature
/// names are intentionally not validated, and the attribute itself is
/// not printed when rendering a module interface.
class AllowFeatureSuppressionAttr final
    : public DeclAttribute,
      private llvm::TrailingObjects<AllowFeatureSuppressionAttr, Identifier> {
  friend TrailingObjects;

  AllowFeatureSuppressionAttr(SourceLoc atLoc, SourceRange range, bool implicit,
                              bool inverted, ArrayRef<Identifier> features);

public:
  static AllowFeatureSuppressionAttr *create(ASTContext &ctx, SourceLoc atLoc,
                                             SourceRange range, bool implicit,
                                             bool inverted,
                                             ArrayRef<Identifier> features);

  bool getInverted() const { return Bits.AllowFeatureSuppressionAttr.Inverted; }

  ArrayRef<Identifier> getSuppressedFeatures() const {
    return {getTrailingObjects<Identifier>(),
            static_cast<size_t>(Bits.AllowFeatureSuppressionAttr.NumFeatures)};
  }

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::AllowFeatureSuppression;
  }

  UNIMPLEMENTED_CLONE(AllowFeatureSuppressionAttr)

  bool isEquivalent(const AllowFeatureSuppressionAttr *other,
                    Decl *attachedTo) const;
};

/// Defines the @abi attribute.
class ABIAttr : public DeclAttribute {
public:
  ABIAttr(Decl *abiDecl, SourceLoc AtLoc, SourceRange Range, bool Implicit)
      : DeclAttribute(DeclAttrKind::ABI, AtLoc, Range, Implicit),
        abiDecl(abiDecl)
  { }

  ABIAttr(Decl *abiDecl, bool Implicit)
      : ABIAttr(abiDecl, SourceLoc(), SourceRange(), Implicit) {}

  /// The declaration which will be used to compute a mangled name.
  ///
  /// \note For a \c VarDecl with a parent \c PatternBindingDecl , this should
  /// point to the parent \c PatternBindingDecl . (That accommodates the way
  /// sibling \c VarDecl s share attributes.)
  Decl *abiDecl;

  static bool classof(const DeclAttribute *DA) {
    return DA->getKind() == DeclAttrKind::ABI;
  }

  UNIMPLEMENTED_CLONE(ABIAttr)

  bool isEquivalent(const ABIAttr *other, Decl *attachedTo) const {
    // Unsupported: tricky to implement and unneeded.
    return true;
  }
};

/// The kind of unary operator, if any.
enum class UnaryOperatorKind : uint8_t { None, Prefix, Postfix };

/// Attributes that may be applied to declarations.
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

  /// Returns the `@backDeployed` attribute that is active for the current
  /// platform.
  const BackDeployedAttr *getBackDeployed(const ASTContext &ctx,
                                          bool forTargetVariant) const;

  SWIFT_DEBUG_DUMPER(dump(const ASTContext &ctx));
  SWIFT_DEBUG_DUMPER(dump(const DeclContext *dc));

  SWIFT_DEBUG_DUMPER(print(const Decl *D = nullptr));
  void print(ASTPrinter &Printer, const PrintOptions &Options,
             const Decl *D = nullptr) const;
  static void print(ASTPrinter &Printer, const PrintOptions &Options,
                    ArrayRef<const DeclAttribute *> FlattenedAttrs,
                    const Decl *D = nullptr);

  template <typename T, typename DERIVED>
  class iterator_base {
    T *Impl;
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = T*;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;    

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

  /// Add multiple constructed DeclAttributes to this list.
  void add(DeclAttributes &Attrs) {
    for (auto attr : Attrs) {
      add(attr);
    }
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
    return const_cast<DeclAttributes *>(this)->getAttribute<ATTR>(AllowInvalid);
  }

  template <typename ATTR>
  ATTR *getAttribute(bool AllowInvalid = false) {
    for (auto Attr : *this)
      if (auto *SpecificAttr = dyn_cast<ATTR>(Attr))
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

  /// Retrieve the first attribute with the given kind.
  DeclAttribute *getAttribute(DeclAttrKind DK,
                              bool AllowInvalid = false) {
    for (auto Attr : *this)
      if (Attr->getKind() == DK && (Attr->isValid() || AllowInvalid))
        return Attr;
    return nullptr;
  }

  /// Returns the "winning" \c NonSendableAttr or \c SendableAttr in this
  /// attribute list, or \c nullptr if there are none.
  const DeclAttribute *getEffectiveSendableAttr() const;

  DeclAttribute *getEffectiveSendableAttr() {
    return const_cast<DeclAttribute *>(
         const_cast<const DeclAttributes *>(this)->getEffectiveSendableAttr());
  }

public:
  template <typename ATTR, typename Iterator, bool AllowInvalid>
  using AttributeKindRange =
      OptionalTransformRange<iterator_range<Iterator>,
                             ToAttributeKind<ATTR, AllowInvalid>,
                             Iterator>;

  /// Return a range with all attributes in DeclAttributes with AttrKind
  /// ATTR.
  template <typename ATTR, bool AllowInvalid = false>
  AttributeKindRange<ATTR, const_iterator, AllowInvalid> getAttributes() const {
    return AttributeKindRange<ATTR, const_iterator, AllowInvalid>(
        make_range(begin(), end()), ToAttributeKind<ATTR, AllowInvalid>());
  }

  /// Return a range with all attributes in DeclAttributes with AttrKind
  /// ATTR.
  template <typename ATTR, bool AllowInvalid = false>
  AttributeKindRange<ATTR, iterator, AllowInvalid> getAttributes() {
    return AttributeKindRange<ATTR, iterator, AllowInvalid>(
        make_range(begin(), end()), ToAttributeKind<ATTR, AllowInvalid>());
  }

  /// Return the range of semantics attributes attached to this attribute set.
  auto getSemanticsAttrs() const
      -> decltype(getAttributes<SemanticsAttr>()) {
    return getAttributes<SemanticsAttr>();
  }

  /// Return whether this attribute set includes the given semantics attribute.
  bool hasSemanticsAttr(StringRef attrValue) const {
    return llvm::any_of(getSemanticsAttrs(), [&](const SemanticsAttr *attr) {
      return attrValue == attr->Value;
    });
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

  DeclAttribute *getRawAttributeChain() const { return DeclAttrs; }

  SourceLoc getStartLoc(bool forModifiers = false) const;
};

/// Predicate used to filter attributes to only the parsed attributes.
class ParsedDeclAttrFilter {
  const Decl *decl;

public:
  ParsedDeclAttrFilter() : decl(nullptr) {}

  ParsedDeclAttrFilter(const Decl *decl) : decl(decl) {}

  std::optional<const DeclAttribute *>
  operator()(const DeclAttribute *Attr) const;
};

/// Attributes written in source on a declaration.
///
/// We should really just have \c DeclAttributes and \c SemanticDeclAttributes,
/// but currently almost all callers expect the latter. Instead of changing all
/// callers of \c getAttrs, instead provide a way to retrieve the original
/// attributes.
class ParsedDeclAttributes {
public:
  using ParsedFilteredRange =
      OptionalTransformRange<iterator_range<DeclAttributes::const_iterator>,
                             ParsedDeclAttrFilter>;

private:
  ParsedFilteredRange parsedRange;

public:
  ParsedDeclAttributes()
      : parsedRange(make_range(DeclAttributes::const_iterator(nullptr),
                               DeclAttributes::const_iterator(nullptr)),
                    ParsedDeclAttrFilter()) {}

  ParsedDeclAttributes(const DeclAttributes &allAttrs, const Decl *decl)
      : parsedRange(make_range(allAttrs.begin(), allAttrs.end()),
                    ParsedDeclAttrFilter(decl)) {}

  ParsedFilteredRange::iterator begin() const { return parsedRange.begin(); }
  ParsedFilteredRange::iterator end() const { return parsedRange.end(); }

  template <typename AttrType, bool AllowInvalid>
  using AttributeKindRange =
      OptionalTransformRange<ParsedFilteredRange,
                             ToAttributeKind<AttrType, AllowInvalid>>;

  template <typename AttrType, bool AllowInvalid = false>
  AttributeKindRange<AttrType, AllowInvalid> getAttributes() const {
    return AttributeKindRange<AttrType, AllowInvalid>(
        parsedRange, ToAttributeKind<AttrType, AllowInvalid>());
  }

  /// Retrieve the first attribute of the given attribute class.
  template <typename AttrType>
  const AttrType *getAttribute(bool allowInvalid = false) const {
    for (auto *attr : parsedRange) {
      if (auto *specificAttr = dyn_cast<AttrType>(attr)) {
        if (specificAttr->isValid() || allowInvalid)
          return specificAttr;
      }
    }
    return nullptr;
  }

  /// Determine whether there is an attribute with the given attribute class.
  template <typename AttrType>
  bool hasAttribute(bool allowInvalid = false) const {
    return getAttribute<AttrType>(allowInvalid) != nullptr;
  }
};

/// A wrapper for `AvailableAttr` that enriches it with additional semantic
/// informaton that can only be determined using the `AvailabilityDomain`
/// associated with the attribute. A `SemanticAvailableAttr` can only be
/// constructed with an `AvailableAttr` that has a resolved
/// `AvailabilityDomain`.
class SemanticAvailableAttr final {
  const AvailableAttr *attr;

public:
  SemanticAvailableAttr(const AvailableAttr *attr) : attr(attr) {
    assert(attr);
    assert(attr->getDomainOrIdentifier().isDomain());
  }

  const AvailableAttr *getParsedAttr() const { return attr; }
  const AvailabilityDomain getDomain() const {
    return attr->getDomainOrIdentifier().getAsDomain().value();
  }

  /// The version tuple for the `introduced:` component.
  std::optional<llvm::VersionTuple> getIntroduced() const;

  /// The source range of the `introduced:` version component.
  SourceRange getIntroducedSourceRange() const { return attr->IntroducedRange; }

  /// See `getIntroducedDomainAndRange()`.
  std::optional<AvailabilityRange>
  getIntroducedRange(const ASTContext &ctx) const {
    if (auto domainAndRange = getIntroducedDomainAndRange(ctx))
      return domainAndRange->getRange();
    return std::nullopt;
  }

  /// Returns the effective introduction range indicated by this attribute,
  /// along with the domain that it applies to (which may be different than the
  /// domain which the attribute was written with if a remap is required). This
  /// may correspond to the version specified by the `introduced:` component
  /// (remapped or canonicalized if necessary) or it may be "always" for an
  /// attribute indicating availability in a version-less domain. Returns
  /// `std::nullopt` if the attribute does not indicate introduction.
  std::optional<AvailabilityDomainAndRange>
  getIntroducedDomainAndRange(const ASTContext &ctx) const;

  /// The version tuple for the `deprecated:` component.
  std::optional<llvm::VersionTuple> getDeprecated() const;

  /// The source range of the `deprecated:` version component.
  SourceRange getDeprecatedSourceRange() const { return attr->DeprecatedRange; }

  /// See `getDeprecatedDomainAndRange()`.
  std::optional<AvailabilityRange>
  getDeprecatedRange(const ASTContext &ctx) const {
    if (auto domainAndRange = getDeprecatedDomainAndRange(ctx))
      return domainAndRange->getRange();
    return std::nullopt;
  }

  /// Returns the effective deprecation range indicated by this attribute, along
  /// with the domain that it applies to (which may be different than the domain
  /// which the attribute was written with if a remap is required). This may
  /// correspond to the version specified by the `deprecated:` component
  /// (remapped or canonicalized if necessary) or it may be "always" for an
  /// unconditional deprecation attribute. Returns `std::nullopt` if the
  /// attribute does not indicate deprecation.
  std::optional<AvailabilityDomainAndRange>
  getDeprecatedDomainAndRange(const ASTContext &ctx) const;

  /// The version tuple for the `obsoleted:` component.
  std::optional<llvm::VersionTuple> getObsoleted() const;

  /// The source range of the `obsoleted:` version component.
  SourceRange getObsoletedSourceRange() const { return attr->ObsoletedRange; }

  /// See `getObsoletedDomainAndRange()`.
  std::optional<AvailabilityRange>
  getObsoletedRange(const ASTContext &ctx) const {
    if (auto domainAndRange = getObsoletedDomainAndRange(ctx))
      return domainAndRange->getRange();
    return std::nullopt;
  }

  /// Returns the effective obsoletion range indicated by this attribute, along
  /// with the domain that it applies to (which may be different than the domain
  /// which the attribute was written with if a remap is required). This always
  /// corresponds to the version specified by the `obsoleted:` component
  /// (remapped or canonicalized if necessary). Returns `std::nullopt` if the
  /// attribute does not indicate obsoletion (note that unavailability is
  /// separate from obsoletion.
  std::optional<AvailabilityDomainAndRange>
  getObsoletedDomainAndRange(const ASTContext &ctx) const;

  /// Returns the `message:` field of the attribute, or an empty string.
  StringRef getMessage() const { return attr->Message; }

  /// Returns the `rename:` field of the attribute, or an empty string.
  StringRef getRename() const { return attr->Rename; }

  /// Returns the platform kind that the attribute applies to, or
  /// `PlatformKind::none` if the attribute is not platform specific.
  bool isPlatformSpecific() const { return getDomain().isPlatform(); }

  /// Returns the platform kind that the attribute applies to, or
  /// `PlatformKind::none` if the attribute is not platform specific.
  PlatformKind getPlatform() const { return getDomain().getPlatformKind(); }

  /// Whether this is attribute indicates unavailability in all versions.
  bool isUnconditionallyUnavailable() const {
    return attr->isUnconditionallyUnavailable();
  }

  /// Whether this is attribute indicates deprecation in all versions.
  bool isUnconditionallyDeprecated() const {
    return attr->isUnconditionallyDeprecated();
  }

  /// Whether this is a `noasync` attribute.
  bool isNoAsync() const { return attr->isNoAsync(); }

  /// Whether this attribute has an introduced, deprecated, or obsoleted
  /// version.
  bool isVersionSpecific() const {
    return getIntroduced().has_value() || getDeprecated().has_value() ||
           getObsoleted().has_value();
  }

  /// Whether this is a language mode specific attribute.
  bool isSwiftLanguageModeSpecific() const {
    return getDomain().isSwiftLanguage() && isVersionSpecific();
  }

  /// Whether this is a PackageDescription version specific attribute.
  bool isPackageDescriptionVersionSpecific() const {
    return getDomain().isPackageDescription() && isVersionSpecific();
  }

  /// Whether this attribute an attribute that is specific to Embedded Swift.
  bool isEmbeddedSpecific() const { return getDomain().isEmbedded(); }

  /// Returns true if this attribute is considered active in the current
  /// compilation context.
  bool isActive(ASTContext &ctx) const;

  /// Whether this attribute was spelled `@_spi_available`.
  bool isSPI() const { return attr->isSPI(); }

  bool operator==(const SemanticAvailableAttr &other) const {
    return other.attr == attr;
  }

  bool operator!=(const SemanticAvailableAttr &other) const {
    return other.attr != attr;
  }
};

/// An iterable range of `SemanticAvailableAttr`s.
class SemanticAvailableAttributes {
public:
  class Filter final {
    const Decl *decl;
    bool includeInactive;

  public:
    Filter(const Decl *decl, bool includeInactive)
        : decl(decl), includeInactive(includeInactive) {}

    std::optional<SemanticAvailableAttr>
    operator()(const DeclAttribute *attr) const;
  };

  using Range =
      OptionalTransformRange<iterator_range<DeclAttributes::const_iterator>,
                             Filter>;

private:
  Range attrRange;

public:
  SemanticAvailableAttributes(const DeclAttributes &attrs, const Decl *decl,
                              bool includeInactive = false)
      : attrRange(make_range(attrs.begin(), attrs.end()),
                  Filter(decl, includeInactive)) {}

  Range::iterator begin() const { return attrRange.begin(); }
  Range::iterator end() const { return attrRange.end(); }
  bool empty() const { return attrRange.empty(); }
};

class alignas(1 << AttrAlignInBits) TypeAttribute
    : public ASTAllocated<TypeAttribute> {
protected:
  // clang-format off
  union {
    uint64_t OpaqueBits;

    SWIFT_INLINE_BITFIELD_BASE(TypeAttribute, bitmax(NumTypeAttrKindBits,8)+1+1,
      Kind : bitmax(NumTypeAttrKindBits,8),
      Implicit : 1,
      Invalid : 1
    );

    SWIFT_INLINE_BITFIELD(DifferentiableTypeAttr, TypeAttribute, 8,
      Differentiability : 8
    );

    SWIFT_INLINE_BITFIELD_FULL(OpaqueReturnTypeOfTypeAttr, TypeAttribute, 32,
      : NumPadBits,
      Index : 32
    );

    SWIFT_INLINE_BITFIELD_FULL(IsolatedTypeAttr, TypeAttribute, 8,
      Kind : 8
    );
  } Bits;
  // clang-format on

  SourceLoc AttrLoc;

  TypeAttribute(TypeAttrKind kind, SourceLoc attrLoc) : AttrLoc(attrLoc) {
    Bits.TypeAttribute.Kind = unsigned(kind);
    Bits.TypeAttribute.Implicit = false;
    Bits.TypeAttribute.Invalid = false;
  }

public:
  TypeAttrKind getKind() const {
    return TypeAttrKind(Bits.TypeAttribute.Kind);
  }

  /// - Note: Do not call this directly when emitting a diagnostic. Instead,
  /// define the diagnostic to accept a `const TypeAttribute *` and use the
  /// appropriate format specifier.
  const char *getAttrName() const {
    return getAttrName(getKind());
  }

  bool isInvalid() const {
    return Bits.TypeAttribute.Invalid;
  }

  bool isImplicit() const {
    return Bits.TypeAttribute.Implicit;
  }

  void setInvalid() {
    Bits.TypeAttribute.Invalid = true;
  }

  void setImplicit() {
    Bits.TypeAttribute.Implicit = true;
  }

  /// Return the location of the attribute identifier / keyword.
  SourceLoc getAttrLoc() const {
    return AttrLoc;
  }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  /// Given a name like "autoclosure", return the type attribute ID that
  /// corresponds to it.
  static std::optional<TypeAttrKind> getAttrKindFromString(StringRef Str);

  /// Returns true if type attributes of the given kind only appear in SIL.
  static bool isSilOnly(TypeAttrKind TK);

  /// Return the name (like "autoclosure") for an attribute ID.
  static const char *getAttrName(TypeAttrKind kind);

  /// Returns whether the given attribute is considered "user inaccessible",
  /// which affects e.g whether it shows up in code completion.
  static bool isUserInaccessible(TypeAttrKind DK);

  static TypeAttribute *createSimple(const ASTContext &context,
                                     TypeAttrKind kind,
                                     SourceLoc atLoc,
                                     SourceLoc attrLoc);

  SWIFT_DEBUG_DUMPER(dump());
  void print(ASTPrinter &Printer, const PrintOptions &Options) const;
};

class AtTypeAttrBase : public TypeAttribute {
  SourceLoc AtLoc;
public:
  AtTypeAttrBase(TypeAttrKind kind, SourceLoc atLoc, SourceLoc attrLoc)
    : TypeAttribute(kind, attrLoc), AtLoc(atLoc) {}

  SourceLoc getAtLoc() const { return AtLoc; }

  SourceLoc getStartLocImpl() const {
    return AtLoc.isValid() ? AtLoc : getAttrLoc();
  }
  SourceLoc getEndLocImpl() const { return getAttrLoc(); }
};

class AtTypeAttrWithArgsBase : public AtTypeAttrBase {
  SourceRange Parens;
public:
  AtTypeAttrWithArgsBase(TypeAttrKind kind, SourceLoc atLoc, SourceLoc idLoc,
                         SourceRange parens)
    : AtTypeAttrBase(kind, atLoc, idLoc), Parens(parens) {}

  SourceRange getParensRange() const { return Parens; }

  SourceLoc getEndLocImpl() const { return Parens.End; }
};

template <TypeAttrKind Kind, class Base = AtTypeAttrBase>
class SimpleTypeAttr : public Base {
public:
  template <class... Args>
  SimpleTypeAttr(Args ...args) : Base(Kind, args...) {}

  static constexpr TypeAttrKind StaticKind = Kind;

  static bool classof(const TypeAttribute *attr) {
    return attr->getKind() == Kind;
  }
};

template <TypeAttrKind Kind>
using SimpleTypeAttrWithArgs = SimpleTypeAttr<Kind, AtTypeAttrWithArgsBase>;

#define SIMPLE_TYPE_ATTR(SPELLING, CLASS)                                      \
  using CLASS##TypeAttr = SimpleTypeAttr<TypeAttrKind::CLASS>;
#include "swift/AST/TypeAttr.def"

class ConventionTypeAttr
    : public SimpleTypeAttrWithArgs<TypeAttrKind::Convention> {
  Located<StringRef> Name;
  DeclNameRef WitnessMethodProtocol;
  Located<StringRef> ClangType;

public:
  ConventionTypeAttr(SourceLoc atLoc, SourceLoc kwLoc,
                     SourceRange parens,
                     Located<StringRef> name,
                     DeclNameRef witnessMethodProtocol,
                     Located<StringRef> clangType)
    : SimpleTypeAttr(atLoc, kwLoc, parens),
      Name(name),
      WitnessMethodProtocol(witnessMethodProtocol),
      ClangType(clangType) {}

  StringRef getConventionName() const { return Name.Item; }
  SourceLoc getConventionLoc() const { return Name.Loc; }
  std::optional<StringRef> getClangType() const {
    if (!ClangType.Item.empty()) return ClangType.Item;
    return {};
  }
  SourceLoc getClangTypeLoc() const { return ClangType.Loc; }
  DeclNameRef getWitnessMethodProtocol() const {
    return WitnessMethodProtocol;
  }

  void printImpl(ASTPrinter &printer, const PrintOptions &options) const;
};

class DifferentiableTypeAttr
    : public SimpleTypeAttrWithArgs<TypeAttrKind::Differentiable> {
  SourceLoc DifferentiabilityLoc;
public:
  DifferentiableTypeAttr(SourceLoc atLoc, SourceLoc kwLoc,
                         SourceRange parensRange,
                         Located<DifferentiabilityKind> differentiability)
    : SimpleTypeAttr(atLoc, kwLoc, parensRange),
      DifferentiabilityLoc(differentiability.Loc) {
    Bits.DifferentiableTypeAttr.Differentiability =
      unsigned(differentiability.Item);
  }

  SourceLoc getEndLocImpl() const {
    // "Override" this method to handle that @differentiable doesn't
    // always take an argument.
    if (getParensRange().isValid()) return getParensRange().End;
    return getAttrLoc();
  }

  DifferentiabilityKind getDifferentiability() const {
    return DifferentiabilityKind(Bits.DifferentiableTypeAttr.Differentiability);
  }
  SourceLoc getDifferentiabilityLoc() const {
    return DifferentiabilityLoc;
  }

  void printImpl(ASTPrinter &printer, const PrintOptions &options) const;
};

class OpaqueReturnTypeOfTypeAttr
    : public SimpleTypeAttrWithArgs<TypeAttrKind::OpaqueReturnTypeOf> {
  Located<StringRef> MangledName;
  SourceLoc IndexLoc;
public:
  OpaqueReturnTypeOfTypeAttr(SourceLoc atLoc, SourceLoc kwLoc, SourceRange parens,
                             Located<StringRef> mangledName,
                             Located<unsigned> index)
    : SimpleTypeAttr(atLoc, kwLoc, parens), MangledName(mangledName) {
    Bits.OpaqueReturnTypeOfTypeAttr.Index = index.Item;
  }

  StringRef getMangledName() const { return MangledName.Item; }
  SourceLoc getMangledNameLoc() const { return MangledName.Loc; }
  unsigned getIndex() const { return Bits.OpaqueReturnTypeOfTypeAttr.Index; }
  SourceLoc getIndexLoc() const { return IndexLoc; }

  void printImpl(ASTPrinter &printer, const PrintOptions &options) const;
};

class OpenedTypeAttr : public SimpleTypeAttrWithArgs<TypeAttrKind::Opened> {
  Located<UUID> ID;
  TypeRepr *ConstraintType;
public:
  OpenedTypeAttr(SourceLoc atLoc, SourceLoc kwLoc, SourceRange parensRange,
                 Located<UUID> id, TypeRepr *constraintType)
    : SimpleTypeAttr(atLoc, kwLoc, parensRange),
      ID(id), ConstraintType(constraintType) {}

  UUID getUUID() const {
    return ID.Item;
  }
  SourceLoc getUUIDLoc() const {
    return ID.Loc;
  }
  TypeRepr *getConstraintType() const {
    return ConstraintType;
  }

  void printImpl(ASTPrinter &printer, const PrintOptions &options) const;
};

class PackElementTypeAttr
    : public SimpleTypeAttrWithArgs<TypeAttrKind::PackElement> {
  Located<UUID> ID;
public:
  PackElementTypeAttr(SourceLoc atLoc, SourceLoc kwLoc, SourceRange parensRange,
                      Located<UUID> id)
    : SimpleTypeAttr(atLoc, kwLoc, parensRange), ID(id) {}

  UUID getUUID() const {
    return ID.Item;
  }
  SourceLoc getUUIDLoc() const {
    return ID.Loc;
  }

  void printImpl(ASTPrinter &printer, const PrintOptions &options) const;
};

/// The @isolated function type attribute, not to be confused with the
/// `isolated` declaration modifier (IsolatedAttr) or the `isolated`
/// parameter specifier (IsolatedTypeRepr).
class IsolatedTypeAttr : public SimpleTypeAttrWithArgs<TypeAttrKind::Isolated> {
public:
  enum class IsolationKind : uint8_t {
    Dynamic
  };

private:
  SourceLoc KindLoc;

public:
  IsolatedTypeAttr(SourceLoc atLoc, SourceLoc kwLoc, SourceRange parensRange,
                   Located<IsolationKind> kind)
    : SimpleTypeAttr(atLoc, kwLoc, parensRange), KindLoc(kind.Loc) {
    Bits.IsolatedTypeAttr.Kind = uint8_t(kind.Item);
  }

  IsolationKind getIsolationKind() const {
    return IsolationKind(Bits.IsolatedTypeAttr.Kind);
  }
  SourceLoc getIsolationKindLoc() const {
    return KindLoc;
  }
  const char *getIsolationKindName() const {
    return getIsolationKindName(getIsolationKind());
  }
  static const char *getIsolationKindName(IsolationKind kind);

  void printImpl(ASTPrinter &printer, const PrintOptions &options) const;
};

using TypeOrCustomAttr =
  llvm::PointerUnion<CustomAttr*, TypeAttribute*>;

void simple_display(llvm::raw_ostream &out, const DeclAttribute *attr);

inline SourceLoc extractNearestSourceLoc(const DeclAttribute *attr) {
  return attr->getLocation();
}

/// Determine whether the given attribute is available, looking up the
/// attribute by name.
bool hasAttribute(const LangOptions &langOpts, llvm::StringRef attributeName);

template <>
struct EnumTraits<TypeAttrKind> {
  static constexpr size_t NumValues = NumTypeAttrKinds;
};

} // end namespace swift

namespace llvm {
using swift::AvailableAttr;
using swift::SemanticAvailableAttr;

// A SemanticAvailableAttr just wraps an `AvailableAttr *` and is therefore
// "pointer like".
template <typename T>
struct PointerLikeTypeTraits;
template <>
struct PointerLikeTypeTraits<SemanticAvailableAttr> {
public:
  static inline void *getAsVoidPointer(SemanticAvailableAttr attr) {
    return reinterpret_cast<void *>(
        const_cast<AvailableAttr *>(attr.getParsedAttr()));
  }
  static inline SemanticAvailableAttr getFromVoidPointer(void *P) {
    return SemanticAvailableAttr(static_cast<AvailableAttr *>(P));
  }
  enum {
    NumLowBitsAvailable =
        PointerLikeTypeTraits<AvailableAttr *>::NumLowBitsAvailable
  };
};

} // end namespace llvm

#undef UNIMPLEMENTED_CLONE

#endif
