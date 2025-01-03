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

// Main entry point into the normalization generator.
func generateNormalization(for platform: String, version: String) {

  var result = readFile("Input/NormalizationData.h")
  
  let derivedNormalizationProps = readFile("Data/\(version)/DerivedNormalizationProps.txt")

  let unicodeData: String
  
  switch platform {
  case "Apple":
    unicodeData = readFile("Data/\(version)/Apple/UnicodeData.txt")
  default:
    unicodeData = readFile("Data/\(version)/UnicodeData.txt")
  }
  
  let combiningClasses = parseCCCData(from: unicodeData)
  let canonicalDecompositions: ParsedDecompositions

  // Canonical Normalization Forms.
  do {

    // Construct and emit NormData for canonical normalization forms.
    // This includes the combining class and QuickCheck flags.
    var canonicalNormData = combiningClasses
    parseQuickCheckFlags(
      kind: .canonical,
      from: derivedNormalizationProps,
      mergingInto: &canonicalNormData
    )
    emitNormData(canonicalNormData, kind: .canonical, into: &result)

    // Parse, expand, and emit the canonical decomposition mapping table.
    let rawDecompositions = parseDecompositionMappings(
      kind: .canonical,
      from: unicodeData
    )
    canonicalDecompositions = recursivelyExpand(rawDecompositions)
    emitDecompositionMappingTable(
      canonicalDecompositions,
      kind: .canonical,
      into: &result
    )

    // Parse and emit composition data. (Remove composition exclusions)
    // Composition data is computed from raw decompositions,
    // not the expanded data.
    let compExclusions = getCompExclusions(from: derivedNormalizationProps)
    let filteredDecomp = rawDecompositions.filter { (scalar, _) in
      !compExclusions.contains {
        $0.contains(scalar)
      }
    }
    let compMph = mph(for: Array(Set(filteredDecomp.map { UInt64($0.decomposition[1]) })))
    emitComp(compMph, filteredDecomp, into: &result)
  }

  // Compatibility Normalization Forms.
  do {

    // Construct and emit NormData for compatibility normalization forms.
    // This includes the same combining class data from before.
    var compatibilityNormData = combiningClasses
    parseQuickCheckFlags(
      kind: .compatibility,
      from: derivedNormalizationProps,
      mergingInto: &compatibilityNormData
    )
    emitNormData(compatibilityNormData, kind: .compatibility, into: &result)

    // Parse, expand, and emit the compatibility decomposition mapping table.
    let rawCompatibilityDecompositions = parseDecompositionMappings(
      kind: .compatibility,
      from: unicodeData
    )
    // Expansion merges the two decomposition tables,
    // but the result is just about too large to fit in our MPH table.
    // To reduce its size, remove purely canonical decompositions.
    //
    // This means we will need to perform 2 table lookups at runtime,
    // which is fine - compatibility normalization is not performance critical.
    var compatibilityDecompositions = recursivelyExpand(
      rawCompatibilityDecompositions, including: canonicalDecompositions
    )
    compatibilityDecompositions.removeAll { compatibilityMapping in
      guard let canonicalMapping = canonicalDecompositions.first(where: { $0.scalar >= compatibilityMapping.scalar }),
            canonicalMapping.scalar == compatibilityMapping.scalar else {
        return false
      }
      return compatibilityMapping.decomposition == canonicalMapping.decomposition
    }
    emitDecompositionMappingTable(
      compatibilityDecompositions,
      kind: .compatibility,
      into: &result
    )
  }
  
  result += """
  #endif // #ifndef NORMALIZATION_DATA_H
  
  """
  
  // Finally, write it out.
  write(result, to: "Output/\(platform)/\(version)/NormalizationData.h")
}

for platform in ["Common", "Apple"] {
  generateNormalization(for: platform, version: "15")
}
