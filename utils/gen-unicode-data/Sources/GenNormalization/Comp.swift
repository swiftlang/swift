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

func getCompExclusions(from data: String) -> [ClosedRange<UInt32>] {
  var result: [ClosedRange<UInt32>] = []
  
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    // Get the property first because we only care about Full_Composition_Exclusion
    let filteredProperty = components[1].filter { !$0.isWhitespace }
    
    guard filteredProperty == "Full_Composition_Exclusion" else {
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
    
    result.append(scalars)
  }
  
  return result
}

func emitComp(_ mph: Mph, _ data: [(UInt32, [UInt32])], into result: inout String) {
  emitMph(mph, name: "_swift_stdlib_nfc_comp", into: &result)
  emitCompComps(mph, data, into: &result)
}

func emitCompComps(
  _ mph: Mph,
  _ data: [(UInt32, [UInt32])],
  into result: inout String
) {
  let uniqueKeys = Set(data.map { $0.1[1] })
  var sortedData: [[(UInt32, UInt32)]] = .init(
    repeating: [],
    count: uniqueKeys.count
  )
  
  for (scalar, comp) in data {
    let idx = mph.index(for: UInt64(comp[1]))
    
    if sortedData[idx].isEmpty {
      sortedData[idx].append((comp[1], .max))
    }
    
    sortedData[idx].append((comp[0], scalar))
  }
  
  // Go back and sort each array as well as putting the size information of each
  // in the first element (who contains the original y scalar that was hashed).
  for i in sortedData.indices {
    sortedData[i][1...].sort { $0.0 < $1.0 }
    
    sortedData[i][0].0 = sortedData[i][0].0 | UInt32(sortedData[i].count << 21)
  }
  
  for i in sortedData.indices {
    result += """
    static const __swift_uint32_t _swift_stdlib_nfc_comp\(i)[\(sortedData[i].count)] = {
    
    """
    
    formatCollection(sortedData[i], into: &result) { (comp, scalar) in
      // This only occurs for the first element who stores the original y scalar
      // in the composition and the size of the array.
      if scalar == .max {
        return "0x\(String(comp, radix: 16, uppercase: true))"
      }
      
      // Make sure that these scalars don't exceed past 17 bits. We need the other
      // 15 bits to store the range to the final composition. Although Unicode
      // scalars can go up to 21 bits, none of the compositions with this scalar
      // go that high.
      assert(comp <= 0x1FFFF)
      var value = comp
      
      // Calculate the distance from our current composition scalar to our final
      // composed scalar.
      let distance = Int(scalar) - Int(comp)
      // Make sure that our distance doesn't exceed 14 bits. Although the above
      // assertion gives us 15 bits, we use the last bit to indicate negative
      // or not.
      assert(distance <= 0x3FFF)
      
      value |= UInt32(distance.magnitude) << 17
                                    
      if distance < 0 {
        value |= 1 << 31
      }
      
      return "0x\(String(value, radix: 16, uppercase: true))"
    }
    
    result += "\n};\n\n"
  }
  
  result += """
  static const __swift_uint32_t * const _swift_stdlib_nfc_comp_indices[\(sortedData.count)] = {
  
  """
  
  formatCollection(sortedData.indices, into: &result) { i in
    return "_swift_stdlib_nfc_comp\(i)"
  }
  
  result += "\n};\n\n"
}

func emitCompAccessor(_ mph: Mph, into result: inout String) {
  result += """
  SWIFT_RUNTIME_STDLIB_INTERNAL
  __swift_uint32_t _swift_stdlib_getComposition(__swift_uint32_t x,
                                              __swift_uint32_t y) {
    __swift_intptr_t compIdx = _swift_stdlib_getMphIdx(y, \(mph.bitArrays.count),
                                                    _swift_stdlib_nfc_comp_keys,
                                                    _swift_stdlib_nfc_comp_ranks,
                                                    _swift_stdlib_nfc_comp_sizes);
    auto array = _swift_stdlib_nfc_comp_indices[compIdx];
  
    // Ensure that the first element in this array is equal to our y scalar.
    auto realY = (array[0] << 11) >> 11;
  
    if (y != realY) {
      return std::numeric_limits<__swift_uint32_t>::max();
    }
  
    auto count = array[0] >> 21;
  
    __swift_uint32_t low = 1;
    __swift_uint32_t high = count - 1;
  
    while (high >= low) {
      auto idx = low + (high - low) / 2;
    
      auto entry = array[idx];
    
      // Shift the range count out of the scalar.
      auto lower = (entry << 15) >> 15;
    
      bool isNegative = entry >> 31;
      auto rangeCount = (entry << 1) >> 18;
    
      if (isNegative) {
        rangeCount = -rangeCount;
      }
    
      auto composed = lower + rangeCount;
    
      if (x == lower) {
        return composed;
      }
    
      if (x > lower) {
        low = idx + 1;
        continue;
      }
    
      if (x < lower) {
        high = idx - 1;
        continue;
      }
    }
  
    // If we made it out here, then our scalar was not found in the composition
    // array.
    // Return the max here to indicate that we couldn't find one.
    return std::numeric_limits<__swift_uint32_t>::max();
  }
  
  """
}
