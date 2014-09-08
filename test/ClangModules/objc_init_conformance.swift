// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache %s

// Note: this is in a separate file because -verify doesn't complain
// about diagnostics from other files.
import Foundation
import AppKit

protocol URLInitializable {
  init?(URL: String!)
}

extension NSDocument : URLInitializable { }
