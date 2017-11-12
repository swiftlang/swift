import Foundation
import UIKit

public func publicFoo(x: AnyObject.Type) -> String {
  return NSStringFromClass(x)
}

public func publicBar() {
  UIApplicationMain(0, nil, nil, nil)
}


