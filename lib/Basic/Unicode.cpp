//===--- Unicode.cpp - Unicode utilities ----------------------------------===//
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

#include "swift/Basic/Unicode.h"
#include "swift/Basic/Compiler.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ConvertUTF.h"
#include <iterator>

using namespace swift;
using swift::unicode::GraphemeClusterBreakProperty;

// HACK: Allow support for many newer emoji by overriding behavior of ZWJ and
// emoji modifiers. This does not make the breaks correct for any version of
// Unicode, but shifts the ways in which it is incorrect to be less harmful.
//
// TODO: Remove this hack and reevaluate whether we should have any static
// notion of what a grapheme is.
//
// Returns true if lhs and rhs shouldn't be considered as having a grapheme
// break between them. That is, whether we're overriding the behavior of the
// hard coded Unicode 8 rules surrounding ZWJ and emoji modifiers.
static inline bool graphemeBreakOverride(llvm::UTF32 lhs, llvm::UTF32 rhs) {
  // Assume ZWJ sequences produce new emoji
  if (lhs == 0x200D) {
    return true;
  }

  // Permit continuing regional indicators
  if (rhs >= 0x1F3FB && rhs <= 0x1F3FF) {
    return true;
  }

  // Permit emoji tag sequences
  if (rhs >= 0xE0020 && rhs <= 0xE007F) {
    return true;
  }

  return false;
}

// Rule GB9c of UAX #29 (Unicode 15.1): do not break within a conjunct, i.e.
// between Indic_Conjunct_Break=Consonant [Extend Linker]* Linker [Extend Linker]*
// and a following Indic_Conjunct_Break=Consonant. The pairwise boundary matrix
// cannot express this rule, so it is tracked with a small state machine.
//
// This mirrors the standard library's implementation (see
// StringGraphemeBreaking.swift) so that a string literal is classified as a
// single extended grapheme cluster exactly when `String` and `Character` agree
// at run time.

// Indic_Conjunct_Break=Consonant, from DerivedCoreProperties.txt (Unicode 17).
static const struct { llvm::UTF32 First, Last; } InCBConsonantRanges[] = {
    {0x0915, 0x0939},
    {0x0958, 0x095F},
    {0x0978, 0x097F},
    {0x0995, 0x09A8},
    {0x09AA, 0x09B0},
    {0x09B2, 0x09B2},
    {0x09B6, 0x09B9},
    {0x09DC, 0x09DD},
    {0x09DF, 0x09DF},
    {0x09F0, 0x09F1},
    {0x0A95, 0x0AA8},
    {0x0AAA, 0x0AB0},
    {0x0AB2, 0x0AB3},
    {0x0AB5, 0x0AB9},
    {0x0AF9, 0x0AF9},
    {0x0B15, 0x0B28},
    {0x0B2A, 0x0B30},
    {0x0B32, 0x0B33},
    {0x0B35, 0x0B39},
    {0x0B5C, 0x0B5D},
    {0x0B5F, 0x0B5F},
    {0x0B71, 0x0B71},
    {0x0C15, 0x0C28},
    {0x0C2A, 0x0C39},
    {0x0C58, 0x0C5A},
    {0x0D15, 0x0D3A},
    {0x1000, 0x102A},
    {0x103F, 0x103F},
    {0x1050, 0x1055},
    {0x105A, 0x105D},
    {0x1061, 0x1061},
    {0x1065, 0x1066},
    {0x106E, 0x1070},
    {0x1075, 0x1081},
    {0x108E, 0x108E},
    {0x1780, 0x17B3},
    {0x1A20, 0x1A54},
    {0x1B0B, 0x1B0C},
    {0x1B13, 0x1B33},
    {0x1B45, 0x1B4C},
    {0x1B83, 0x1BA0},
    {0x1BAE, 0x1BAF},
    {0x1BBB, 0x1BBD},
    {0xA989, 0xA98B},
    {0xA98F, 0xA9B2},
    {0xA9E0, 0xA9E4},
    {0xA9E7, 0xA9EF},
    {0xA9FA, 0xA9FE},
    {0xAA60, 0xAA6F},
    {0xAA71, 0xAA73},
    {0xAA7A, 0xAA7A},
    {0xAA7E, 0xAA7F},
    {0xAAE0, 0xAAEA},
    {0xABC0, 0xABDA},
    {0x10A00, 0x10A00},
    {0x10A10, 0x10A13},
    {0x10A15, 0x10A17},
    {0x10A19, 0x10A35},
    {0x11103, 0x11126},
    {0x11144, 0x11144},
    {0x11147, 0x11147},
    {0x11380, 0x11389},
    {0x1138B, 0x1138B},
    {0x1138E, 0x1138E},
    {0x11390, 0x113B5},
    {0x11900, 0x11906},
    {0x11909, 0x11909},
    {0x1190C, 0x11913},
    {0x11915, 0x11916},
    {0x11918, 0x1192F},
    {0x11A00, 0x11A00},
    {0x11A0B, 0x11A32},
    {0x11A50, 0x11A50},
    {0x11A5C, 0x11A83},
    {0x11F04, 0x11F10},
    {0x11F12, 0x11F33},
};

static bool isInCBConsonant(llvm::UTF32 C) {
  size_t Lo = 0, Hi = std::size(InCBConsonantRanges);
  while (Lo < Hi) {
    size_t Mid = Lo + (Hi - Lo) / 2;
    if (C < InCBConsonantRanges[Mid].First)
      Hi = Mid;
    else if (C > InCBConsonantRanges[Mid].Last)
      Lo = Mid + 1;
    else
      return true;
  }
  return false;
}

static bool isInCBLinker(llvm::UTF32 C) {
  switch (C) {
  case 0x094D: // Devanagari
  case 0x09CD: // Bengali
  case 0x0ACD: // Gujarati
  case 0x0B4D: // Oriya
  case 0x0C4D: // Telugu
  case 0x0D4D: // Malayalam
    return true;
  default:
    return false;
  }
}

namespace {
/// Progress through `Consonant [Extend Linker]* Linker [Extend Linker]*`.
enum class IndicConjunctState : uint8_t {
  /// Not inside a possible conjunct.
  None,
  /// Saw a consonant, possibly followed by extenders, but no linker yet.
  Consonant,
  /// Saw a consonant and at least one linker; a consonant here continues the
  /// cluster.
  ConsonantLinker,
};
} // end anonymous namespace

/// Returns the state after consuming \p C, given the state before it and
/// \p GCB, the Grapheme_Cluster_Break property of \p C.
static IndicConjunctState
advanceIndicConjunctState(IndicConjunctState State, llvm::UTF32 C,
                          GraphemeClusterBreakProperty GCB) {
  if (isInCBConsonant(C))
    return IndicConjunctState::Consonant;
  if (State == IndicConjunctState::None)
    return IndicConjunctState::None;
  if (isInCBLinker(C))
    return IndicConjunctState::ConsonantLinker;
  // InCB=Extend: other Extend scalars and ZWJ, but not ZWNJ.
  if ((GCB == GraphemeClusterBreakProperty::Extend || C == 0x200D) &&
      C != 0x200C)
    return State;
  return IndicConjunctState::None;
}

StringRef swift::unicode::extractFirstExtendedGraphemeCluster(StringRef S) {
  // Extended grapheme cluster segmentation algorithm as described in Unicode
  // Standard Annex #29.
  if (S.empty())
    return StringRef();

  const llvm::UTF8 *SourceStart =
    reinterpret_cast<const llvm::UTF8 *>(S.data());

  const llvm::UTF8 *SourceNext = SourceStart;
  llvm::UTF32 C[2];
  llvm::UTF32 *TargetStart = C;

  ConvertUTF8toUTF32(&SourceNext, SourceStart + S.size(), &TargetStart, C + 1,
                     llvm::lenientConversion);
  if (TargetStart == C) {
    // The source string contains an ill-formed subsequence at the end.
    return S;
  }

  GraphemeClusterBreakProperty GCBForC0 = getGraphemeClusterBreakProperty(C[0]);
  IndicConjunctState IndicState = advanceIndicConjunctState(
      IndicConjunctState::None, C[0], GCBForC0);
  while (true) {
    size_t C1Offset = SourceNext - SourceStart;
    ConvertUTF8toUTF32(&SourceNext, SourceStart + S.size(), &TargetStart, C + 2,
                       llvm::lenientConversion);

    if (TargetStart == C + 1) {
      // End of source string or the source string contains an ill-formed
      // subsequence at the end.
      return S.slice(0, C1Offset);
    }

    GraphemeClusterBreakProperty GCBForC1 =
        getGraphemeClusterBreakProperty(C[1]);
    bool IsConjunctContinuation =
        IndicState == IndicConjunctState::ConsonantLinker &&
        isInCBConsonant(C[1]);
    if (isExtendedGraphemeClusterBoundary(GCBForC0, GCBForC1) &&
        !graphemeBreakOverride(C[0], C[1]) && !IsConjunctContinuation)
      return S.slice(0, C1Offset);

    IndicState = advanceIndicConjunctState(IndicState, C[1], GCBForC1);
    C[0] = C[1];
    TargetStart = C + 1;
    GCBForC0 = GCBForC1;
  }
}

static bool extractFirstUnicodeScalarImpl(StringRef S, unsigned &Scalar) {
  if (S.empty())
    return false;

  const llvm::UTF8 *SourceStart =
    reinterpret_cast<const llvm::UTF8 *>(S.data());

  const llvm::UTF8 *SourceNext = SourceStart;
  llvm::UTF32 C;
  llvm::UTF32 *TargetStart = &C;

  ConvertUTF8toUTF32(&SourceNext, SourceStart + S.size(), &TargetStart,
                     TargetStart + 1, llvm::lenientConversion);
  if (TargetStart == &C) {
    // The source string contains an ill-formed subsequence at the end.
    return false;
  }

  Scalar = C;
  return size_t(SourceNext - SourceStart) == S.size();
}

bool swift::unicode::isSingleUnicodeScalar(StringRef S) {
  unsigned Scalar;
  return extractFirstUnicodeScalarImpl(S, Scalar);
}

unsigned swift::unicode::extractFirstUnicodeScalar(StringRef S) {
  unsigned Scalar;
  bool Result = extractFirstUnicodeScalarImpl(S, Scalar);
  assert(Result && "string does not consist of one Unicode scalar");
  (void)Result;
  return Scalar;
}

bool swift::unicode::isWellFormedUTF8(StringRef S) {
  const llvm::UTF8 *begin = S.bytes_begin();
  return llvm::isLegalUTF8String(&begin, S.bytes_end());
}

std::string swift::unicode::sanitizeUTF8(StringRef Text) {
  llvm::SmallString<256> Builder;
  Builder.reserve(Text.size());
  const llvm::UTF8* Data = reinterpret_cast<const llvm::UTF8*>(Text.begin());
  const llvm::UTF8* End = reinterpret_cast<const llvm::UTF8*>(Text.end());
  StringRef Replacement = SWIFT_UTF8("\ufffd");
  while (Data < End) {
    auto Step = llvm::getNumBytesForUTF8(*Data);
    if (Data + Step > End) {
      Builder.append(Replacement);
      break;
    }

    if (llvm::isLegalUTF8Sequence(Data, Data + Step)) {
      Builder.append(Data, Data + Step);
    } else {

      // If malformed, add replacement characters.
      Builder.append(Replacement);
    }
    Data += Step;
  }
  return std::string(Builder.str());
}
