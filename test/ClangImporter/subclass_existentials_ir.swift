// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o - -primary-file %s -swift-version 4

// REQUIRES: objc_interop

import Foundation

class SwiftLaundryService : NSLaundry {
  var g: (Garment & Coat)? = nil

  func wash(_ g: Garment & Coat) {
    self.g = g
  }

  func bleach(_ g: Garment & Coat & Cotton) {}

  func dry() -> Garment & Coat {
    return g!
  }
}

func doTheLaundry(_ service: NSLaundry, clothing: Garment & Coat) {
  service.wash(clothing)
  _ = service.dry()
}
