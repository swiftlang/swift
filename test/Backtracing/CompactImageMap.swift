// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Onone -o %t/ImageMap
// RUN: %target-codesign %t/ImageMap
// RUN: %target-run %t/ImageMap | tee %t/ImageMap.out
// RUN: cat %t/ImageMap.out | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// REQUIRES: executable_test
// REQUIRES: backtracing
// REQUIRES: OS=macosx || OS=linux-gnu

import Runtime
@_spi(Internal) import Runtime

@main
struct ImageMapTest {
  static func main() {
    let map = ImageMap.capture()
    let encoder = CompactImageMapFormat.Encoder(map)
    let encoded = Array(encoder)

    print(map)

    print("Encoded \(map.count) images in \(encoded.count) bytes")

    for (ndx, byte) in encoded.enumerated() {
      let separator: String
      if ((ndx + 1) & 0xf) == 0 {
        separator = "\n"
      } else {
        separator = " "
      }

      var hex = String(byte, radix: 16)
      if hex.count < 2 {
        hex = "0" + hex
      }
      print(hex, terminator: separator)
    }
    print("")

    guard let decodedMap = ImageMap(compactImageMapData: encoded) else {
      print("Unable to decode")
      return
    }

    print("Decoded \(decodedMap.count) images")

    print(decodedMap)

    if map.description != decodedMap.description {
      print("Maps do not match")
    } else {
      print("Maps match")
    }

    // CHECK: Encoded [[COUNT:[0-9]+]] images in [[BYTES:[0-9]+]] bytes
    // CHECK-NOT: Unable to decode
    // CHECK: Decoded [[COUNT]] images
    // CHECK-NOT: Maps do not match
    // CHECK: Maps match
  }
}
