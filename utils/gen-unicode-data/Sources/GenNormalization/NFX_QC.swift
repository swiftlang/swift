//===----------------------------------------------------------------------===//
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

import GenUtils

enum EquivalenceType {
  case canonical
  case compatibility
}

private enum NormalizationForm {
  case decomposed
  case precomposed
}

/// Parses normalization Quick Check flags for a given equivalence type
/// from a DerivedNormalizationProps.txt file, and updates the given database.
///
/// If a scalar is already present in the database,
/// the precomposed and decomposed quick-check flags will be updated.
///
/// If a scalar is not already present in the database,
/// a default-initialized `NormData` will be inserted with the appropriate
/// quick-check flags.
///
/// The format of DerivedNormalizationProps.txt is described by [UAX#44](UAX44).
///
/// [UAX44]: https://www.unicode.org/reports/tr44/#DerivedNormalizationProps.txt
///
func parseQuickCheckFlags(
  kind: EquivalenceType,
  from data: String,
  mergingInto database: inout [UInt32: NormData]
) {

  for line in data.split(separator: "\n") {

    // Each line in the data file contains a series of components
    // separated by a ';'. For example:
    //
    //     0343..0344    ; NFC_QC; N # Mn   [2] COMBINING GREEK KORONIS..COMBINING GREEK DIALYTIKA TONOS
    //     0374          ; NFC_QC; N # Lm       GREEK NUMERAL SIGN
    //
    // Index 0 is a scalar value or range of scalars.
    // Index 1 is the property being specified.
    // Index 2 is the property value. It is either "N" (No) or "M" (Maybe).
    // Unlisted scalars have a default value of Yes.
    //
    // Everything after the "#" is a comment.

    // Skip comments.

    guard !line.hasPrefix("#") else {
      continue
    }
    
    let components = line.prefix { $0 != "#" }.split(separator: ";")
    
    // Parse the normalization form.

    let normalizationForm: NormalizationForm
    switch (kind, components[1].filter({ !$0.isWhitespace })) {
    case (.canonical, "NFD_QC"):      normalizationForm = .decomposed
    case (.canonical, "NFC_QC"):      normalizationForm = .precomposed
    case (.compatibility, "NFKD_QC"): normalizationForm = .decomposed
    case (.compatibility, "NFKC_QC"): normalizationForm = .precomposed
    default: continue
    }

    // Parse the scalar/range of scalars.

    let scalars: ClosedRange<UInt32>

    let filteredScalars = components[0].filter { !$0.isWhitespace }
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")
      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!
      scalars = scalar ... scalar
    }
    
    // Special case: Do not store hangul decompositions.

    if scalars == 0xAC00...0xD7A3, normalizationForm == .decomposed {
      continue
    }
    
    let filteredNFCQC = components[2].filter { !$0.isWhitespace }

    // Insert the data in to the result dictionary.
    // Any scalars not listed are implicitly *_QC=Yes.

    for scalar in scalars {

      var normData = database[scalar, default: NormData()]
      switch normalizationForm {
      case .decomposed:
        normData.decomposedFormQuickCheck = false
      case .precomposed:
        normData.precomposedFormQuickCheck = filteredNFCQC == "N" ? .no : .maybe
      }
      database[scalar] = normData
    }
  }
}
