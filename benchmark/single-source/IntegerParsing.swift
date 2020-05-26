//===--- IntegerParsing.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils


// - Definitions
public let IntegerParsing = [
  // IntSmall
  BenchmarkInfo(name: "ParseInt.IntSmall.Decimal",
    runFunction: run_ParseIntFromIntSmallDecimal,
    tags: [.validation, .api],
    setUpFunction: {
      blackHole(intSmallValuesSum)
      blackHole(intSmallDecimalStrings)
  }),
  BenchmarkInfo(name: "ParseInt.IntSmall.Binary",
    runFunction: run_ParseIntFromIntSmallBinary,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(intSmallValuesSum)
      blackHole(intSmallBinaryStrings)
  }),
  BenchmarkInfo(name: "ParseInt.IntSmall.Hex",
    runFunction: run_ParseIntFromIntSmallHex,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(intSmallValuesSum)
      blackHole(intSmallHexStrings)
  }),
  BenchmarkInfo(name: "ParseInt.IntSmall.UncommonRadix",
    runFunction: run_ParseIntFromIntSmallUncommonRadix,
    tags: [.validation, .api],
    setUpFunction: {
      blackHole(intSmallValuesSum)
      blackHole(intSmallUncommonRadixStrings)
  }),
  // UIntSmall
  BenchmarkInfo(name: "ParseInt.UIntSmall.Decimal",
    runFunction: run_ParseIntFromUIntSmallDecimal,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uintSmallValuesSum)
      blackHole(uintSmallDecimalStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UIntSmall.Binary",
    runFunction: run_ParseIntFromUIntSmallBinary,
    tags: [.validation, .api],
    setUpFunction: {
      blackHole(uintSmallValuesSum)
      blackHole(uintSmallBinaryStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UIntSmall.Hex",
    runFunction: run_ParseIntFromUIntSmallHex,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uintSmallValuesSum)
      blackHole(uintSmallHexStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UIntSmall.UncommonRadix",
    runFunction: run_ParseIntFromUIntSmallUncommonRadix,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uintSmallValuesSum)
      blackHole(uintSmallUncommonRadixStrings)
  }),
  // Int32
  BenchmarkInfo(name: "ParseInt.Int32.Decimal",
    runFunction: run_ParseIntFromInt32Decimal,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int32ValuesSum)
      blackHole(int32DecimalStrings)
  }),
  BenchmarkInfo(name: "ParseInt.Int32.Binary",
    runFunction: run_ParseIntFromInt32Binary,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int32ValuesSum)
      blackHole(int32BinaryStrings)
  }),
  BenchmarkInfo(name: "ParseInt.Int32.Hex",
    runFunction: run_ParseIntFromInt32Hex,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int32ValuesSum)
      blackHole(int32HexStrings)
  }),
  BenchmarkInfo(name: "ParseInt.Int32.UncommonRadix",
    runFunction: run_ParseIntFromInt32UncommonRadix,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int32ValuesSum)
      blackHole(int32UncommonRadixStrings)
  }),
  // Int64
  BenchmarkInfo(name: "ParseInt.Int64.Decimal",
    runFunction: run_ParseIntFromInt64Decimal,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int64ValuesSum)
      blackHole(int64DecimalStrings)
  }),
  BenchmarkInfo(name: "ParseInt.Int64.Binary",
    runFunction: run_ParseIntFromInt64Binary,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int64ValuesSum)
      blackHole(int64BinaryStrings)
  }),
  BenchmarkInfo(name: "ParseInt.Int64.Hex",
    runFunction: run_ParseIntFromInt64Hex,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int64ValuesSum)
      blackHole(int64HexStrings)
  }),
  BenchmarkInfo(name: "ParseInt.Int64.UncommonRadix",
    runFunction: run_ParseIntFromInt64UncommonRadix,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(int64ValuesSum)
      blackHole(int64UncommonRadixStrings)
  }),
  // UInt32
  BenchmarkInfo(name: "ParseInt.UInt32.Decimal",
    runFunction: run_ParseIntFromUInt32Decimal,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uint32ValuesSum)
      blackHole(uint32DecimalStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UInt32.Binary",
    runFunction: run_ParseIntFromUInt32Binary,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uint32ValuesSum)
      blackHole(uint32BinaryStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UInt32.Hex",
    runFunction: run_ParseIntFromUInt32Hex,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uint32ValuesSum)
      blackHole(uint32HexStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UInt32.UncommonRadix",
    runFunction: run_ParseIntFromUInt32UncommonRadix,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uint32ValuesSum)
      blackHole(uint32UncommonRadixStrings)
  }),
  // UInt64
  BenchmarkInfo(name: "ParseInt.UInt64.Decimal",
    runFunction: run_ParseIntFromUInt64Decimal,
    tags: [.validation, .api],
    setUpFunction: {
      blackHole(uint64ValuesSum)
      blackHole(uint64DecimalStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UInt64.Binary",
    runFunction: run_ParseIntFromUInt64Binary,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uint64ValuesSum)
      blackHole(uint64BinaryStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UInt64.Hex",
    runFunction: run_ParseIntFromUInt64Hex,
    tags: [.validation, .api],
    setUpFunction: {
      blackHole(uint64ValuesSum)
      blackHole(uint64HexStrings)
  }),
  BenchmarkInfo(name: "ParseInt.UInt64.UncommonRadix",
    runFunction: run_ParseIntFromUInt64UncommonRadix,
    tags: [.validation, .api, .skip],
    setUpFunction: {
      blackHole(uint64ValuesSum)
      blackHole(uint64UncommonRadixStrings)
  }),
]

// - Verification Sums
private let intSmallValuesSum: Int
  = intSmallValues.reduce(0, &+)
private let uintSmallValuesSum: UInt
  = uintSmallValues.reduce(0, &+)
private let int32ValuesSum: Int32
  = int32Values.reduce(0, &+)
private let int64ValuesSum: Int64
  = int64Values.reduce(0, &+)
private let uint32ValuesSum: UInt32
  = uint32Values.reduce(0, &+)
private let uint64ValuesSum: UInt64
  = uint64Values.reduce(0, &+)

// - Values
func generateValues<Integer : FixedWidthInteger>(
  in range: ClosedRange<Integer>, count: Int
) -> [Integer] {
    var rng = SplitMix64(seed: 42)
    return (0..<count).map { _ in
      Integer.random(in: range, using: &rng)
    }
}
private let intSmallValues: [Int]
  = generateValues(in: -9999 ... 9999, count: 1000)
private let uintSmallValues: [UInt]
  = generateValues(in: 0 ... 9999, count: 1000)
private let int32Values: [Int32]
  = generateValues(in: .min ... .max, count: 1000)
private let int64Values: [Int64]
  = generateValues(in: .min ... .max, count: 1000)
private let uint32Values: [UInt32]
  = generateValues(in: .min ... .max, count: 1000)
private let uint64Values: [UInt64]
  = generateValues(in: .min ... .max, count: 1000)

// - Strings
// IntSmall
private let intSmallDecimalStrings: [String]
  = intSmallValues.map { String($0, radix: 10) }
private let intSmallBinaryStrings: [String]
  = intSmallValues.map { String($0, radix: 2) }
private let intSmallHexStrings: [String]
  = intSmallValues.map { String($0, radix: 16) }
private let intSmallUncommonRadixStrings: [String]
  = intSmallValues.map { String($0, radix: 7) }
// UIntSmall
private let uintSmallDecimalStrings: [String]
  = uintSmallValues.map { String($0, radix: 10) }
private let uintSmallBinaryStrings: [String]
  = uintSmallValues.map { String($0, radix: 2) }
private let uintSmallHexStrings: [String]
  = uintSmallValues.map { String($0, radix: 16) }
private let uintSmallUncommonRadixStrings: [String]
  = uintSmallValues.map { String($0, radix: 7) }
// Int32
private let int32DecimalStrings: [String]
  = int32Values.map { String($0, radix: 10) }
private let int32BinaryStrings: [String]
  = int32Values.map { String($0, radix: 2) }
private let int32HexStrings: [String]
  = int32Values.map { String($0, radix: 16) }
private let int32UncommonRadixStrings: [String]
  = int32Values.map { String($0, radix: 7) }
// Int64
private let int64DecimalStrings: [String]
  = int64Values.map { String($0, radix: 10) }
private let int64BinaryStrings: [String]
  = int64Values.map { String($0, radix: 2) }
private let int64HexStrings: [String]
  = int64Values.map { String($0, radix: 16) }
private let int64UncommonRadixStrings: [String]
  = int64Values.map { String($0, radix: 7) }
// UInt32
private let uint32DecimalStrings: [String]
  = uint32Values.map { String($0, radix: 10) }
private let uint32BinaryStrings: [String]
  = uint32Values.map { String($0, radix: 2) }
private let uint32HexStrings: [String]
  = uint32Values.map { String($0, radix: 16) }
private let uint32UncommonRadixStrings: [String]
  = uint32Values.map { String($0, radix: 7) }
// UInt64
private let uint64DecimalStrings: [String]
  = uint64Values.map { String($0, radix: 10) }
private let uint64BinaryStrings: [String]
  = uint64Values.map { String($0, radix: 2) }
private let uint64HexStrings: [String]
  = uint64Values.map { String($0, radix: 16) }
private let uint64UncommonRadixStrings: [String]
  = uint64Values.map { String($0, radix: 7) }

// - Implementations

// IntSmall
@inline(never)
public func run_ParseIntFromIntSmallDecimal(N: Int) {
  var result: Int = 0
  let count = N * 20
  for _ in 0..<count {
    for string in intSmallDecimalStrings {
      result &+= Int(string, radix: 10)!
    }
  }
  CheckResults(result == intSmallValuesSum &* Int(count))
}

@inline(never)
public func run_ParseIntFromIntSmallBinary(N: Int) {
  var result: Int = 0
  let count = N * 20
  for _ in 0..<count {
    for string in intSmallBinaryStrings {
      result &+= Int(string, radix: 2)!
    }
  }
  CheckResults(result == intSmallValuesSum &* Int(count))
}

@inline(never)
public func run_ParseIntFromIntSmallHex(N: Int) {
  var result: Int = 0
  let count = N * 20
  for _ in 0..<count {
    for string in intSmallHexStrings {
      result &+= Int(string, radix: 16)!
    }
  }
  CheckResults(result == intSmallValuesSum &* Int(count))
}

@inline(never)
public func run_ParseIntFromIntSmallUncommonRadix(N: Int) {
  var result: Int = 0
  let count = N * 20
  for _ in 0..<count {
    for string in intSmallUncommonRadixStrings {
      result &+= Int(string, radix: 7)!
    }
  }
  CheckResults(result == intSmallValuesSum &* Int(count))
}

// UIntSmall
@inline(never)
public func run_ParseIntFromUIntSmallDecimal(N: Int) {
  var result: UInt = 0
  let count = N * 20
  for _ in 0..<count {
    for string in uintSmallDecimalStrings {
      result &+= UInt(string, radix: 10)!
    }
  }
  CheckResults(result == uintSmallValuesSum &* UInt(count))
}

@inline(never)
public func run_ParseIntFromUIntSmallBinary(N: Int) {
  var result: UInt = 0
  let count = N * 20
  for _ in 0..<count {
    for string in uintSmallBinaryStrings {
      result &+= UInt(string, radix: 2)!
    }
  }
  CheckResults(result == uintSmallValuesSum &* UInt(count))
}

@inline(never)
public func run_ParseIntFromUIntSmallHex(N: Int) {
  var result: UInt = 0
  let count = N * 20
  for _ in 0..<count {
    for string in uintSmallHexStrings {
      result &+= UInt(string, radix: 16)!
    }
  }
  CheckResults(result == uintSmallValuesSum &* UInt(count))
}

@inline(never)
public func run_ParseIntFromUIntSmallUncommonRadix(N: Int) {
  var result: UInt = 0
  let count = N * 20
  for _ in 0..<count {
    for string in uintSmallUncommonRadixStrings {
      result &+= UInt(string, radix: 7)!
    }
  }
  CheckResults(result == uintSmallValuesSum &* UInt(count))
}

// Int32
@inline(never)
public func run_ParseIntFromInt32Decimal(N: Int) {
  var result: Int32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in int32DecimalStrings {
      result &+= Int32(string, radix: 10)!
    }
  }
  CheckResults(result == int32ValuesSum &* Int32(count))
}

@inline(never)
public func run_ParseIntFromInt32Binary(N: Int) {
  var result: Int32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in int32BinaryStrings {
      result &+= Int32(string, radix: 2)!
    }
  }
  CheckResults(result == int32ValuesSum &* Int32(count))
}

@inline(never)
public func run_ParseIntFromInt32Hex(N: Int) {
  var result: Int32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in int32HexStrings {
      result &+= Int32(string, radix: 16)!
    }
  }
  CheckResults(result == int32ValuesSum &* Int32(count))
}

@inline(never)
public func run_ParseIntFromInt32UncommonRadix(N: Int) {
  var result: Int32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in int32UncommonRadixStrings {
      result &+= Int32(string, radix: 7)!
    }
  }
  CheckResults(result == int32ValuesSum &* Int32(count))
}

// Int64
@inline(never)
public func run_ParseIntFromInt64Decimal(N: Int) {
  var result: Int64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in int64DecimalStrings {
      result &+= Int64(string, radix: 10)!
    }
  }
  CheckResults(result == int64ValuesSum &* Int64(count))
}

@inline(never)
public func run_ParseIntFromInt64Binary(N: Int) {
  var result: Int64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in int64BinaryStrings {
      result &+= Int64(string, radix: 2)!
    }
  }
  CheckResults(result == int64ValuesSum &* Int64(count))
}

@inline(never)
public func run_ParseIntFromInt64Hex(N: Int) {
  var result: Int64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in int64HexStrings {
      result &+= Int64(string, radix: 16)!
    }
  }
  CheckResults(result == int64ValuesSum &* Int64(count))
}

@inline(never)
public func run_ParseIntFromInt64UncommonRadix(N: Int) {
  var result: Int64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in int64UncommonRadixStrings {
      result &+= Int64(string, radix: 7)!
    }
  }
  CheckResults(result == int64ValuesSum &* Int64(count))
}

// UInt32
@inline(never)
public func run_ParseIntFromUInt32Decimal(N: Int) {
  var result: UInt32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in uint32DecimalStrings {
      result &+= UInt32(string, radix: 10)!
    }
  }
  CheckResults(result == uint32ValuesSum &* UInt32(count))
}

@inline(never)
public func run_ParseIntFromUInt32Binary(N: Int) {
  var result: UInt32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in uint32BinaryStrings {
      result &+= UInt32(string, radix: 2)!
    }
  }
  CheckResults(result == uint32ValuesSum &* UInt32(count))
}

@inline(never)
public func run_ParseIntFromUInt32Hex(N: Int) {
  var result: UInt32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in uint32HexStrings {
      result &+= UInt32(string, radix: 16)!
    }
  }
  CheckResults(result == uint32ValuesSum &* UInt32(count))
}

@inline(never)
public func run_ParseIntFromUInt32UncommonRadix(N: Int) {
  var result: UInt32 = 0
  let count = N * 8
  for _ in 0..<count {
    for string in uint32UncommonRadixStrings {
      result &+= UInt32(string, radix: 7)!
    }
  }
  CheckResults(result == uint32ValuesSum &* UInt32(count))
}

// UInt64
@inline(never)
public func run_ParseIntFromUInt64Decimal(N: Int) {
  var result: UInt64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in uint64DecimalStrings {
      result &+= UInt64(string, radix: 10)!
    }
  }
  CheckResults(result == uint64ValuesSum &* UInt64(count))
}

@inline(never)
public func run_ParseIntFromUInt64Binary(N: Int) {
  var result: UInt64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in uint64BinaryStrings {
      result &+= UInt64(string, radix: 2)!
    }
  }
  CheckResults(result == uint64ValuesSum &* UInt64(count))
}

@inline(never)
public func run_ParseIntFromUInt64Hex(N: Int) {
  var result: UInt64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in uint64HexStrings {
      result &+= UInt64(string, radix: 16)!
    }
  }
  CheckResults(result == uint64ValuesSum &* UInt64(count))
}

@inline(never)
public func run_ParseIntFromUInt64UncommonRadix(N: Int) {
  var result: UInt64 = 0
  let count = N * 4
  for _ in 0..<count {
    for string in uint64UncommonRadixStrings {
      result &+= UInt64(string, radix: 7)!
    }
  }
  CheckResults(result == uint64ValuesSum &* UInt64(count))
}



