// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s

// XFAIL: linux

class Unique {} 

#if canImport(Swift)
#else
  class Unique {} // This should not happen
#endif

#if canImport(Swiftz)
  class Unique {} // This should not happen
#else
#endif

#if canImport(UIKit)
  import UIKit
  class MyView : UIView {}
#elseif canImport(AppKit)
  import AppKit
  class MyView : NSView {}
#else
  class Unique {} // This should not happen
#endif

#if canImport(Foundation) || canImport(Foundation)
  import Foundation
#else
  class Unique {} // This should not happen
#endif

#if canImport(Foundation) && canImport(Foundation)
  import Foundation
#else
  class Unique {} // This should not happen
#endif

#if !canImport(Swiftz)
#if canImport(UIKit)
  import UIKit
#else
  class Unique {} // This should not happen
#endif
#else
  class Unique {} // This should not happen
#endif
