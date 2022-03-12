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

func generateScalarProps(for platform: String) {
  var result = readFile("Input/ScalarPropData.h")
  
  generateBinaryProps(for: platform, into: &result)
  generateNumericProps(into: &result)
  generateNameAliasProp(into: &result)
  generateMappingProps(for: platform, into: &result)
  generateNameProp(into: &result)
  generateAgeProp(into: &result)
  generateGeneralCategory(into: &result)
  
  result += """
  #endif // #ifndef SCALAR_PROP_DATA_H
  
  """
  
  write(result, to: "Output/\(platform)/ScalarPropData.h")
}

for platform in ["Common", "Apple"] {
  generateScalarProps(for: platform)
}
