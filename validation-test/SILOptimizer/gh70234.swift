// RUN: %target-swift-frontend -emit-sil -verify %s

// REQUIRES: objc_interop

import Foundation

func mutateText(_ rawText: NSString?) -> String {
  guard let text = consume rawText else {
    return "text unavailable"
  }
  return String(text)
}
