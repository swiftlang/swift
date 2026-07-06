// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

let _: (CFURL) -> CGDataProvider? = CGDataProvider.init

// CHECK-LABEL: sil private [ossa] @$s15cf_curried_initSo17CGDataProviderRefaSgSo8CFURLRefacfu_ : $@convention(thin) (@guaranteed CFURL) -> @owned Optional<CGDataProvider>
// CHECK-LABEL: sil {{.*}}[asmname "CGDataProviderCreateWithURL"] [clang CGDataProvider.init] @$sSo17CGDataProviderRefa3urlABSgSo8CFURLRefa_tcfCTo : $@convention(c) (CFURL) -> @owned Optional<CGDataProvider>
