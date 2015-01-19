// This file is a part of the multi-file test driven by 'another_delegate.swift'.

// NB: No "-verify"--this file should parse successfully on its own.
// RUN: %target-swift-frontend %clang-importer-sdk -parse -parse-as-library %s

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate: NSObject, NSApplicationDelegate {
}

func hi() {}
