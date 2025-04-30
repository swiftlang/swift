// XFAIL: *
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Method -source-filename=x
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/method.swift -dump-macro-expansions -typecheck -verify

// REQUIRES: objc_interop

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h

//--- method.swift
import CoreImage

func test(_ image: CGImage) -> Int {
  return image.width
}
