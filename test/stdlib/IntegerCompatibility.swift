// RUN: %target-build-swift %s -swift-version 4 -typecheck


func byteswap_n(_ a: UInt64) -> UInt64 {
  return ((a & 0x00000000000000FF) &<< 56) |
         ((a & 0x000000000000FF00) &<< 40) |
         ((a & 0x0000000000FF0000) &<< 24) |
         ((a & 0x00000000FF000000) &<<  8) |
         ((a & 0x000000FF00000000) &>>  8) |
         ((a & 0x0000FF0000000000) &>> 24) |
         ((a & 0x00FF000000000000) &>> 40) |
         ((a & 0xFF00000000000000) &>> 56)
}


// expression should not be too complex
func radar31845712(_ i: Int, _ buffer: [UInt8]) {
  _ =  UInt64(buffer[i])
    | (UInt64(buffer[i + 1]) &<< 8)
    | (UInt64(buffer[i + 2]) &<< 16)
    | (UInt64(buffer[i + 3]) &<< 24)
    | (UInt64(buffer[i + 4]) &<< 32)
    | (UInt64(buffer[i + 5]) &<< 40)
    | (UInt64(buffer[i + 6]) &<< 48)
    | (UInt64(buffer[i + 7]) &<< 56)
}

// expression should not be too complex
func radar32149641() {
  func from(bigEndian input: UInt32) -> UInt32 {
    var val: UInt32 = input
    return withUnsafePointer(to: &val) { (ptr: UnsafePointer<UInt32>) -> UInt32 in
      return ptr.withMemoryRebound(to: UInt8.self, capacity: 4) { data in
        return (UInt32(data[3]) &<< 0) |
               (UInt32(data[2]) &<< 8) |
               (UInt32(data[1]) &<< 16) |
               (UInt32(data[0]) &<< 24)
      }
    }
  }
}

func homogeneousLookingShiftAndAMask(_ i64: Int64) {
  _ = (i64 >> 8) & 0xFF
}

func negativeShift(_ u8: UInt8) {
  _ = (u8 << -1)
}

func sr5176(description: String = "unambiguous Int32.init(bitPattern:)") {
  _ = Int32(bitPattern: 0) // should compile without ambiguity
}

func sr6634(x: UnsafeBufferPointer<UInt8>) -> Int {
  return x.lazy.filter { $0 > 127 || $0 == 0 }.count // should be unambiguous
}

// abs of an integer literal
func returnIntAbs() -> Int {
  let x = abs(-8)
  return x
}
