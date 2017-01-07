// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop
// REQUIRES: can_import

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

func keepOn(keepingOn : () -> ()) {
#if canImport(Foundation)
  keepingOn()
#else
  class Unique {} // This should not happen 
#endif
}

keepOn {
#if !canImport(Swift) || canImport(Foundation)
  print("")
#elseif canImport(Swiftz)
  class Unique {} // This should not happen
#else
  class Unique {} // This should not happen
#endif

  let object : NSObject
#if canImport(Foundation)
  object = NSObject()
#else
  object = "This should not happen"
#endif
  print(object)
}

