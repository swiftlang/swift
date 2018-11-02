// REQUIRES: OS=ios
// REQUIRES: objc_interop

// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_uikit %s | %FileCheck %s

import UIKit

class ViewController: UIViewController {
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_uikit.ViewController.viewDidLoad() -> ()
// CHECK-NEXT: [[@LINE+1]]:31 -> [[@LINE+3]]:4 : 0
  override func viewDidLoad() {
    super.viewDidLoad()
  }
}
