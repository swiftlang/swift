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

enum NumericType: String {
  case numeric = "Numeric"
  case digit = "Digit"
  case decimal = "Decimal"
  
  var binaryRepresentation: UInt32 {
    switch self {
    case .numeric:
      return 0
    case .digit:
      return 1
    case .decimal:
      return 2
    }
  }
}

func getNumericTypes(
  from data: String,
  into result: inout [(ClosedRange<UInt32>, NumericType)]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    let filteredProperty = components[1].filter { !$0.isWhitespace }
    
    guard let numericType = NumericType(rawValue: filteredProperty) else {
      continue
    }
    
    let scalars: ClosedRange<UInt32>
    
    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    // If we have . appear, it means we have a legitimate range. Otherwise,
    // it's a singular scalar.
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")
      
      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!
      
      scalars = scalar ... scalar
    }
    
    result.append((scalars, numericType))
  }
}

func getNumericValues(
  from data: String,
  into result: inout [(ClosedRange<UInt32>, String)]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    let filteredValue = components[3].filter { !$0.isWhitespace }
    
    let scalars: ClosedRange<UInt32>
    
    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    // If we have . appear, it means we have a legitimate range. Otherwise,
    // it's a singular scalar.
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")
      
      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!
      
      scalars = scalar ... scalar
    }
    
    result.append((scalars, filteredValue))
  }
}

func emitNumericData(
  types: [(ClosedRange<UInt32>, NumericType)],
  values: [(ClosedRange<UInt32>, String)],
  into result: inout String
) {
  emitCollection(
    types,
    name: "_swift_stdlib_numeric_type",
    type: "__swift_uint32_t",
    into: &result
  ) { range, type in
    var value = range.lowerBound
    assert(range.count - 1 <= UInt8.max)
    value |= UInt32(range.count - 1) << 21
    
    value |= type.binaryRepresentation << 29
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  let uniqueValues = Array(Set(values.map { $0.1 }))
  
  emitCollection(
    uniqueValues,
    name: "_swift_stdlib_numeric_values",
    type: "double",
    into: &result
  ) {
    "(double) \($0)"
  }
  
  var allScalars: [UInt64] = []
  
  for (range, _) in values {
    for scalar in range {
      allScalars.append(UInt64(scalar))
    }
  }
  
  let valueMph = mph(for: allScalars)
  
  emitMph(valueMph, name: "_swift_stdlib_numeric_values", into: &result)
  
  var valueIndices: [UInt8] = .init(repeating: 0, count: allScalars.count)
  
  for scalar in allScalars {
    let idx = valueMph.index(for: scalar)
    
    let value = values.first { $0.0.contains(UInt32(scalar)) }!
    let valueIdx = uniqueValues.firstIndex(of: value.1)!
    
    valueIndices[idx] = UInt8(valueIdx)
  }
  
  emitCollection(
    valueIndices,
    name: "_swift_stdlib_numeric_values_indices",
    into: &result
  )
}

func generateNumericProps(into result: inout String) {
  let derivedNumericType = readFile("Data/DerivedNumericType.txt")
  let derivedNumericValues = readFile("Data/DerivedNumericValues.txt")
  
  var numericTypes: [(ClosedRange<UInt32>, NumericType)] = []
  var numericValues: [(ClosedRange<UInt32>, String)] = []
  
  getNumericTypes(from: derivedNumericType, into: &numericTypes)
  getNumericValues(from: derivedNumericValues, into: &numericValues)
  
  emitNumericData(
    types: flatten(numericTypes),
    values: flatten(numericValues),
    into: &result
  )
}
