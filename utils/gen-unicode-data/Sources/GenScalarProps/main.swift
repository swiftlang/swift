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

func generateScalarProps() {
  var result = readFile("Input/UnicodeScalarProps.cpp")
  
  generateBinaryProps(into: &result)
  genericNumericProps(into: &result)
  generateNameAliasProp(into: &result)
  generateMappingProps(into: &result)
  generateNameProp(into: &result)
  generateAgeProp(into: &result)
  generateGeneralCategory(into: &result)
  
  write(result, to: "../../stdlib/public/stubs/UnicodeScalarProps.cpp")
}

generateScalarProps()
