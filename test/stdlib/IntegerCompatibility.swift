// RUN: %target-build-swift %s -swift-version 3 -typecheck
// RUN: %target-build-swift %s -swift-version 4 -typecheck


func byteswap_n(_ a: UInt64) -> UInt64 {
#if swift(>=4)
  return ((a & 0x00000000000000FF) &<< 56) |
         ((a & 0x000000000000FF00) &<< 40) |
         ((a & 0x0000000000FF0000) &<< 24) |
         ((a & 0x00000000FF000000) &<<  8) |
         ((a & 0x000000FF00000000) &>>  8) |
         ((a & 0x0000FF0000000000) &>> 24) |
         ((a & 0x00FF000000000000) &>> 40) |
         ((a & 0xFF00000000000000) &>> 56)
#else
  return ((a & 0x00000000000000FF) << 56) |
         ((a & 0x000000000000FF00) << 40) |
         ((a & 0x0000000000FF0000) << 24) |
         ((a & 0x00000000FF000000) <<  8) |
         ((a & 0x000000FF00000000) >>  8) |
         ((a & 0x0000FF0000000000) >> 24) |
         ((a & 0x00FF000000000000) >> 40) |
         ((a & 0xFF00000000000000) >> 56)
#endif
}


// expression should not be too complex
func radar31845712(_ i: Int, _ buffer: [UInt8]) {
#if swift(>=4)
  _ =  UInt64(buffer[i])
    | (UInt64(buffer[i + 1]) &<< 8)
    | (UInt64(buffer[i + 2]) &<< 16)
    | (UInt64(buffer[i + 3]) &<< 24)
    | (UInt64(buffer[i + 4]) &<< 32)
    | (UInt64(buffer[i + 5]) &<< 40)
    | (UInt64(buffer[i + 6]) &<< 48)
    | (UInt64(buffer[i + 7]) &<< 56)
#else
  _ =  UInt64(buffer[i])
    | (UInt64(buffer[i + 1]) << 8)
    | (UInt64(buffer[i + 2]) << 16)
    | (UInt64(buffer[i + 3]) << 24)
    | (UInt64(buffer[i + 4]) << 32)
    | (UInt64(buffer[i + 5]) << 40)
    | (UInt64(buffer[i + 6]) << 48)
    | (UInt64(buffer[i + 7]) << 56)
#endif
}

// expression should not be too complex
func radar32149641() {
  func from(bigEndian input: UInt32) -> UInt32 {
    var val: UInt32 = input
    return withUnsafePointer(to: &val) { (ptr: UnsafePointer<UInt32>) -> UInt32 in
      return ptr.withMemoryRebound(to: UInt8.self, capacity: 4) { data in
#if swift(>=4)
        return (UInt32(data[3]) &<< 0) |
               (UInt32(data[2]) &<< 8) |
               (UInt32(data[1]) &<< 16) |
               (UInt32(data[0]) &<< 24)
#else
        return (UInt32(data[3]) << 0) |
               (UInt32(data[2]) << 8) |
               (UInt32(data[1]) << 16) |
               (UInt32(data[0]) << 24)
#endif
      }
    }
  }
}

func homogeneousLookingShiftAndAMask(_ i64: Int64) {
  _ = (i64 >> 8) & 0xFF
}

#if swift(>=4)
func negativeShift(_ u8: UInt8) {
  _ = (u8 << -1)
}
#endif
