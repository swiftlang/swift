//===--- DiagnosticFormatting.h - Diagnostic Message Formatting -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Formatting of diagnostic messages and arguments.
//
//===----------------------------------------------------------------------===//

#ifndef DiagnosticFormatting_h
#define DiagnosticFormatting_h

#include "swift/AST/Attr.h"
#include "swift/AST/DeclNameLoc.h"
#include "swift/AST/TypeLoc.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Allocator.h"

namespace swift {

/// Describes the kind of diagnostic argument we're storing.
///
enum class DiagnosticArgumentKind {
  String,
  Integer,
  Unsigned,
  Identifier,
  ObjCSelector,
  ValueDecl,
  Type,
  TypeRepr,
  PatternKind,
  SelfAccessKind,
  ReferenceOwnership,
  StaticSpellingKind,
  DescriptiveDeclKind,
  DeclAttribute,
  VersionTuple,
  LayoutConstraint,
};

enum class PatternKind : uint8_t;
enum class SelfAccessKind : uint8_t;
enum class ReferenceOwnership : uint8_t;
enum class StaticSpellingKind : uint8_t;
enum class DescriptiveDeclKind : uint8_t;

/// Variant type that holds a single diagnostic argument of a known
/// type.
///
/// All diagnostic arguments are converted to an instance of this class.
class DiagnosticArgument {
  DiagnosticArgumentKind Kind;
  union {
    int IntegerVal;
    unsigned UnsignedVal;
    StringRef StringVal;
    DeclName IdentifierVal;
    ObjCSelector ObjCSelectorVal;
    ValueDecl *TheValueDecl;
    Type TypeVal;
    TypeRepr *TyR;
    PatternKind PatternKindVal;
    SelfAccessKind SelfAccessKindVal;
    ReferenceOwnership ReferenceOwnershipVal;
    StaticSpellingKind StaticSpellingKindVal;
    DescriptiveDeclKind DescriptiveDeclKindVal;
    const DeclAttribute *DeclAttributeVal;
    llvm::VersionTuple VersionVal;
    LayoutConstraint LayoutConstraintVal;
  };

public:
  DiagnosticArgument(StringRef S)
      : Kind(DiagnosticArgumentKind::String), StringVal(S) {}

  DiagnosticArgument(int I)
      : Kind(DiagnosticArgumentKind::Integer), IntegerVal(I) {}

  DiagnosticArgument(unsigned I)
      : Kind(DiagnosticArgumentKind::Unsigned), UnsignedVal(I) {}

  DiagnosticArgument(DeclName D)
      : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(D) {}

  DiagnosticArgument(DeclBaseName D)
      : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(D) {}

  DiagnosticArgument(Identifier I)
      : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(I) {}

  DiagnosticArgument(ObjCSelector S)
      : Kind(DiagnosticArgumentKind::ObjCSelector), ObjCSelectorVal(S) {}

  DiagnosticArgument(ValueDecl *VD)
      : Kind(DiagnosticArgumentKind::ValueDecl), TheValueDecl(VD) {}

  DiagnosticArgument(Type T) : Kind(DiagnosticArgumentKind::Type), TypeVal(T) {}

  DiagnosticArgument(TypeRepr *T)
      : Kind(DiagnosticArgumentKind::TypeRepr), TyR(T) {}

  DiagnosticArgument(const TypeLoc &TL) {
    if (TypeRepr *tyR = TL.getTypeRepr()) {
      Kind = DiagnosticArgumentKind::TypeRepr;
      TyR = tyR;
    } else {
      Kind = DiagnosticArgumentKind::Type;
      TypeVal = TL.getType();
    }
  }

  DiagnosticArgument(PatternKind K)
      : Kind(DiagnosticArgumentKind::PatternKind), PatternKindVal(K) {}

  DiagnosticArgument(ReferenceOwnership RO)
      : Kind(DiagnosticArgumentKind::ReferenceOwnership),
        ReferenceOwnershipVal(RO) {}

  DiagnosticArgument(SelfAccessKind SAK)
      : Kind(DiagnosticArgumentKind::SelfAccessKind), SelfAccessKindVal(SAK) {}

  DiagnosticArgument(StaticSpellingKind SSK)
      : Kind(DiagnosticArgumentKind::StaticSpellingKind),
        StaticSpellingKindVal(SSK) {}

  DiagnosticArgument(DescriptiveDeclKind DDK)
      : Kind(DiagnosticArgumentKind::DescriptiveDeclKind),
        DescriptiveDeclKindVal(DDK) {}

  DiagnosticArgument(const DeclAttribute *attr)
      : Kind(DiagnosticArgumentKind::DeclAttribute), DeclAttributeVal(attr) {}

  DiagnosticArgument(llvm::VersionTuple version)
      : Kind(DiagnosticArgumentKind::VersionTuple), VersionVal(version) {}

  DiagnosticArgument(LayoutConstraint L)
      : Kind(DiagnosticArgumentKind::LayoutConstraint), LayoutConstraintVal(L) {
  }
  /// Initializes a diagnostic argument using the underlying type of the
  /// given enum.
  template <
      typename EnumType,
      typename std::enable_if<std::is_enum<EnumType>::value>::type * = nullptr>
  DiagnosticArgument(EnumType value)
      : DiagnosticArgument(
            static_cast<typename std::underlying_type<EnumType>::type>(value)) {
  }

  DiagnosticArgumentKind getKind() const { return Kind; }

  StringRef getAsString() const {
    assert(Kind == DiagnosticArgumentKind::String);
    return StringVal;
  }

  int getAsInteger() const {
    assert(Kind == DiagnosticArgumentKind::Integer);
    return IntegerVal;
  }

  unsigned getAsUnsigned() const {
    assert(Kind == DiagnosticArgumentKind::Unsigned);
    return UnsignedVal;
  }

  DeclName getAsIdentifier() const {
    assert(Kind == DiagnosticArgumentKind::Identifier);
    return IdentifierVal;
  }

  ObjCSelector getAsObjCSelector() const {
    assert(Kind == DiagnosticArgumentKind::ObjCSelector);
    return ObjCSelectorVal;
  }

  ValueDecl *getAsValueDecl() const {
    assert(Kind == DiagnosticArgumentKind::ValueDecl);
    return TheValueDecl;
  }

  Type getAsType() const {
    assert(Kind == DiagnosticArgumentKind::Type);
    return TypeVal;
  }

  TypeRepr *getAsTypeRepr() const {
    assert(Kind == DiagnosticArgumentKind::TypeRepr);
    return TyR;
  }

  PatternKind getAsPatternKind() const {
    assert(Kind == DiagnosticArgumentKind::PatternKind);
    return PatternKindVal;
  }

  ReferenceOwnership getAsReferenceOwnership() const {
    assert(Kind == DiagnosticArgumentKind::ReferenceOwnership);
    return ReferenceOwnershipVal;
  }

  SelfAccessKind getAsSelfAccessKind() const {
    assert(Kind == DiagnosticArgumentKind::SelfAccessKind);
    return SelfAccessKindVal;
  }

  StaticSpellingKind getAsStaticSpellingKind() const {
    assert(Kind == DiagnosticArgumentKind::StaticSpellingKind);
    return StaticSpellingKindVal;
  }

  DescriptiveDeclKind getAsDescriptiveDeclKind() const {
    assert(Kind == DiagnosticArgumentKind::DescriptiveDeclKind);
    return DescriptiveDeclKindVal;
  }

  const DeclAttribute *getAsDeclAttribute() const {
    assert(Kind == DiagnosticArgumentKind::DeclAttribute);
    return DeclAttributeVal;
  }

  llvm::VersionTuple getAsVersionTuple() const {
    assert(Kind == DiagnosticArgumentKind::VersionTuple);
    return VersionVal;
  }

  LayoutConstraint getAsLayoutConstraint() const {
    assert(Kind == DiagnosticArgumentKind::LayoutConstraint);
    return LayoutConstraintVal;
  }
};

struct DiagnosticFormatOptions {
  const std::string OpeningQuotationMark;
  const std::string ClosingQuotationMark;
  const std::string AKAFormatString;
  const std::string OpaqueResultFormatString;

  DiagnosticFormatOptions(std::string OpeningQuotationMark,
                          std::string ClosingQuotationMark,
                          std::string AKAFormatString,
                          std::string OpaqueResultFormatString)
      : OpeningQuotationMark(OpeningQuotationMark),
        ClosingQuotationMark(ClosingQuotationMark),
        AKAFormatString(AKAFormatString),
        OpaqueResultFormatString(OpaqueResultFormatString) {}

  DiagnosticFormatOptions()
      : OpeningQuotationMark("'"), ClosingQuotationMark("'"),
        AKAFormatString("'%s' (aka '%s')"),
        OpaqueResultFormatString("'%s' (%s of '%s')") {}

  /// When formatting fix-it arguments, don't include quotes or other
  /// additions which would result in invalid code.
  static DiagnosticFormatOptions formatForFixits() {
    return DiagnosticFormatOptions("", "", "%s", "%s");
  }
};

namespace DiagnosticFormatting {

/// Format the given diagnostic text and place the result in the given
/// buffer.
void formatDiagnosticText(
    llvm::raw_ostream &Out, StringRef InText,
    ArrayRef<DiagnosticArgument> FormatArgs,
    DiagnosticFormatOptions FormatOpts = DiagnosticFormatOptions());

} // end namespace DiagnosticFormatting
} // end namespace swift

#endif /* DiagnosticFormatting_h */
