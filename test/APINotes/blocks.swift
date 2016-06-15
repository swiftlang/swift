// RUN: %target-build-swift -Xfrontend %clang-importer-sdk %s -emit-ir
// REQUIRES: executable_test

// REQUIRES: objc_interop

import UIKit

class MyView: UIView {
  func foo() {
    UIView.animate(withDuration: 1, delay: 1, options: UIViewAnimationOptions.layoutSubviews,
      animations: { print("animating") },
      completion: { (finished: Bool) -> Void in print("here we are") });
    UIView.animate(withDuration: 1, delay: 1, options: UIViewAnimationOptions.layoutSubviews,
      animations: { print("animating") },
      completion: nil);
  }
}
