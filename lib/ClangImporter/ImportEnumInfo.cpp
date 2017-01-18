//===--- ImportEnumInfo.cpp - Information about importable Clang enums ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides EnumInfo, which describes a Clang enum ready to be
// imported
//
//===----------------------------------------------------------------------===//

#include "ClangAdapter.h"
#include "ImportEnumInfo.h"
#include "ImporterImpl.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/Lex/MacroInfo.h"
#include "clang/Lex/Preprocessor.h"

#include "llvm/ADT/Statistic.h"
#define DEBUG_TYPE "Enum Info"
STATISTIC(EnumInfoNumCacheHits, "# of times the enum info cache was hit");
STATISTIC(EnumInfoNumCacheMisses, "# of times the enum info cache was missed");

using namespace swift;
using namespace importer;

/// Classify the given Clang enumeration to describe how to import it.
void EnumInfo::classifyEnum(ASTContext &ctx, const clang::EnumDecl *decl,
                            clang::Preprocessor &pp) {
  // Anonymous enumerations simply get mapped to constants of the
  // underlying type of the enum, because there is no way to conjure up a
  // name for the Swift type.
  if (!decl->hasNameForLinkage()) {
    kind = EnumKind::Constants;
    return;
  }

  // First, check for attributes that denote the classification
  if (auto domainAttr = decl->getAttr<clang::NSErrorDomainAttr>()) {
    kind = EnumKind::Enum;
    nsErrorDomain = ctx.AllocateCopy(domainAttr->getErrorDomain()->getName());
    return;
  }

  // Was the enum declared using *_ENUM or *_OPTIONS?
  // FIXME: Use Clang attributes instead of groveling the macro expansion loc.
  auto loc = decl->getLocStart();
  if (loc.isMacroID()) {
    StringRef MacroName = pp.getImmediateMacroName(loc);
    if (MacroName == "CF_ENUM" || MacroName == "__CF_NAMED_ENUM" ||
        MacroName == "OBJC_ENUM" || MacroName == "SWIFT_ENUM" ||
        MacroName == "SWIFT_ENUM_NAMED") {
      kind = EnumKind::Enum;
      return;
    }
    if (MacroName == "CF_OPTIONS" || MacroName == "OBJC_OPTIONS" ||
        MacroName == "SWIFT_OPTIONS") {
      kind = EnumKind::Options;
      return;
    }
  }

  // Hardcode a particular annoying case in the OS X headers.
  if (decl->getName() == "DYLD_BOOL") {
    kind = EnumKind::Enum;
    return;
  }

  // Fall back to the 'Unknown' path.
  kind = EnumKind::Unknown;
}

/// Returns the common prefix of two strings at camel-case word granularity.
///
/// For example, given "NSFooBar" and "NSFooBas", returns "NSFoo"
/// (not "NSFooBa"). The returned StringRef is a slice of the "a" argument.
///
/// If either string has a non-identifier character immediately after the
/// prefix, \p followedByNonIdentifier will be set to \c true. If both strings
/// have identifier characters after the prefix, \p followedByNonIdentifier will
/// be set to \c false. Otherwise, \p followedByNonIdentifier will not be
/// changed from its initial value.
///
/// This is used to derive the common prefix of enum constants so we can elide
/// it from the Swift interface.
StringRef importer::getCommonWordPrefix(StringRef a, StringRef b,
                                        bool &followedByNonIdentifier) {
  auto aWords = camel_case::getWords(a), bWords = camel_case::getWords(b);
  auto aI = aWords.begin(), aE = aWords.end(), bI = bWords.begin(),
       bE = bWords.end();

  unsigned prevLength = 0;
  unsigned prefixLength = 0;
  for (; aI != aE && bI != bE; ++aI, ++bI) {
    if (*aI != *bI) {
      followedByNonIdentifier = false;
      break;
    }

    prevLength = prefixLength;
    prefixLength = aI.getPosition() + aI->size();
  }

  // Avoid creating a prefix where the rest of the string starts with a number.
  if ((aI != aE && !Lexer::isIdentifier(*aI)) ||
      (bI != bE && !Lexer::isIdentifier(*bI))) {
    followedByNonIdentifier = true;
    prefixLength = prevLength;
  }

  return a.slice(0, prefixLength);
}

/// Returns the common word-prefix of two strings, allowing the second string
/// to be a common English plural form of the first.
///
/// For example, given "NSProperty" and "NSProperties", the full "NSProperty"
/// is returned. Given "NSMagicArmor" and "NSMagicArmory", only
/// "NSMagic" is returned.
///
/// The "-s", "-es", and "-ies" patterns cover every plural NS_OPTIONS name
/// in Cocoa and Cocoa Touch.
///
/// \see getCommonWordPrefix
StringRef importer::getCommonPluralPrefix(StringRef singular,
                                          StringRef plural) {
  assert(!plural.empty());

  if (singular.empty())
    return singular;

  bool ignored;
  StringRef commonPrefix = getCommonWordPrefix(singular, plural, ignored);
  if (commonPrefix.size() == singular.size() || plural.back() != 's')
    return commonPrefix;

  StringRef leftover = singular.substr(commonPrefix.size());
  StringRef firstLeftoverWord = camel_case::getFirstWord(leftover);
  StringRef commonPrefixPlusWord =
      singular.substr(0, commonPrefix.size() + firstLeftoverWord.size());

  // Is the plural string just "[singular]s"?
  plural = plural.drop_back();
  if (plural.endswith(firstLeftoverWord))
    return commonPrefixPlusWord;

  if (plural.empty() || plural.back() != 'e')
    return commonPrefix;

  // Is the plural string "[singular]es"?
  plural = plural.drop_back();
  if (plural.endswith(firstLeftoverWord))
    return commonPrefixPlusWord;

  if (plural.empty() || !(plural.back() == 'i' && singular.back() == 'y'))
    return commonPrefix;

  // Is the plural string "[prefix]ies" and the singular "[prefix]y"?
  plural = plural.drop_back();
  firstLeftoverWord = firstLeftoverWord.drop_back();
  if (plural.endswith(firstLeftoverWord))
    return commonPrefixPlusWord;

  return commonPrefix;
}

/// Determine the prefix to be stripped from the names of the enum constants
/// within the given enum.
void EnumInfo::determineConstantNamePrefix(ASTContext &ctx,
                                           const clang::EnumDecl *decl) {
  switch (getKind()) {
  case EnumKind::Enum:
  case EnumKind::Options:
    // Enums are mapped to Swift enums, Options to Swift option sets, both
    // of which attempt prefix-stripping.
    break;

  case EnumKind::Constants:
  case EnumKind::Unknown:
    // Nothing to do.
    return;
  }

  // If there are no enumers, there is no prefix to compute.
  auto ec = decl->enumerator_begin(), ecEnd = decl->enumerator_end();
  if (ec == ecEnd)
    return;

  // Determine whether the given enumerator is non-deprecated and has no
  // specifically-provided name.
  auto isNonDeprecatedWithoutCustomName = [](
      const clang::EnumConstantDecl *elem) -> bool {
    if (elem->hasAttr<clang::SwiftNameAttr>())
      return false;

    clang::VersionTuple maxVersion{~0U, ~0U, ~0U};
    switch (elem->getAvailability(nullptr, maxVersion)) {
    case clang::AR_Available:
    case clang::AR_NotYetIntroduced:
      for (auto attr : elem->attrs()) {
        if (auto annotate = dyn_cast<clang::AnnotateAttr>(attr)) {
          if (annotate->getAnnotation() == "swift1_unavailable")
            return false;
        }
        if (auto avail = dyn_cast<clang::AvailabilityAttr>(attr)) {
          if (avail->getPlatform()->getName() == "swift")
            return false;
        }
      }
      return true;

    case clang::AR_Deprecated:
    case clang::AR_Unavailable:
      return false;
    }

    llvm_unreachable("Invalid AvailabilityAttr.");
  };

  // Move to the first non-deprecated enumerator, or non-swift_name'd
  // enumerator, if present.
  auto firstNonDeprecated =
      std::find_if(ec, ecEnd, isNonDeprecatedWithoutCustomName);
  bool hasNonDeprecated = (firstNonDeprecated != ecEnd);
  if (hasNonDeprecated) {
    ec = firstNonDeprecated;
  } else {
    // Advance to the first case without a custom name, deprecated or not.
    while (ec != ecEnd && (*ec)->hasAttr<clang::SwiftNameAttr>())
      ++ec;
    if (ec == ecEnd) {
      return;
    }
  }

  // Compute the common prefix.
  StringRef commonPrefix = (*ec)->getName();
  bool followedByNonIdentifier = false;
  for (++ec; ec != ecEnd; ++ec) {
    // Skip deprecated or swift_name'd enumerators.
    const clang::EnumConstantDecl *elem = *ec;
    if (hasNonDeprecated) {
      if (!isNonDeprecatedWithoutCustomName(elem))
        continue;
    } else {
      if (elem->hasAttr<clang::SwiftNameAttr>())
        continue;
    }

    commonPrefix = getCommonWordPrefix(commonPrefix, elem->getName(),
                                       followedByNonIdentifier);
    if (commonPrefix.empty())
      break;
  }

  if (!commonPrefix.empty()) {
    StringRef checkPrefix = commonPrefix;

    // Account for the 'kConstant' naming convention on enumerators.
    if (checkPrefix[0] == 'k') {
      bool canDropK;
      if (checkPrefix.size() >= 2)
        canDropK = clang::isUppercase(checkPrefix[1]);
      else
        canDropK = !followedByNonIdentifier;

      if (canDropK)
        checkPrefix = checkPrefix.drop_front();
    }

    // Don't use importFullName() here, we want to ignore the swift_name
    // and swift_private attributes.
    StringRef enumNameStr = decl->getName();
    StringRef commonWithEnum = getCommonPluralPrefix(checkPrefix, enumNameStr);
    size_t delta = commonPrefix.size() - checkPrefix.size();

    // Account for the 'EnumName_Constant' convention on enumerators.
    if (commonWithEnum.size() < checkPrefix.size() &&
        checkPrefix[commonWithEnum.size()] == '_' && !followedByNonIdentifier) {
      delta += 1;
    }

    commonPrefix = commonPrefix.slice(0, commonWithEnum.size() + delta);
  }

  constantNamePrefix = ctx.AllocateCopy(commonPrefix);
}

EnumInfo EnumInfoCache::getEnumInfo(const clang::EnumDecl *decl) {
  if (enumInfos.count(decl)) {
    ++EnumInfoNumCacheHits;
    return enumInfos[decl];
  }
  ++EnumInfoNumCacheMisses;
  EnumInfo enumInfo(swiftCtx, decl, clangPP);
  enumInfos[decl] = enumInfo;
  return enumInfo;
}
