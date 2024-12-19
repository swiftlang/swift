// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

// rdar://139812024 - error: cannot convert return expression of type 'NSObject' to return type 'NSImage'

import Foundation

@objc
final class Image: NSObject {
}

@available(*, unavailable)
extension Image: Sendable {} // expected-note {{explicitly marked unavailable here}}

class Lock<State> {
  func withLock<R: Sendable>(_: @Sendable (inout State) -> R) -> R {
    fatalError()
  }
}

extension Lock where State == Void {
  func withLock<R: Sendable>(_: @Sendable () -> R) -> R {
    fatalError()
  }
}

class Test {
  var images: [Int: Image] = [:]

  func fetchImage(lock: Lock<Void>, id: Int) -> Image? {
    if let existingImage = lock.withLock({ return images[id] }) { // expected-warning {{unavailable}}
      return existingImage
    }
    return nil
  }
}
