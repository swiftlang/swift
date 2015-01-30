// RUN: %target-build-swift -Xfrontend %clang-importer-sdk %s -emit-ir

// REQUIRES: objc_interop

import UIKit

class MyView: UIView {
  func foo() {
    UIView.animateWithDuration( 1, delay: 1, options: UIViewAnimationOptions.LayoutSubviews,
      animations:{println("animating")},
      completion: {(finished:Bool)->Void in println("here we are")});
    UIView.animateWithDuration( 1, delay: 1, options: UIViewAnimationOptions.LayoutSubviews,
      animations:{println("animating")},
      completion: nil);
  }
}
