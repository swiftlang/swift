// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ShadowsStringProcessing.swiftmodule -module-name ShadowsStringProcessing %S/Inputs/ShadowsStringProcessing.swift -target %target-swift-5.7-abi-triple
// RUN: %target-typecheck-verify-swift -I %t -target %target-swift-5.7-abi-triple

import ShadowsStringProcessing

func f(_ t : Regex<Substring>) -> Bool {
  return t.someProperty == "123"
}

func g(_: _StringProcessing.Regex<Substring>) {}
