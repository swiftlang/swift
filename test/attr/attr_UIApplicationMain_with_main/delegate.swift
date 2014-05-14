// This file is a part of the multi-file test driven by 'main.swift'.

// NB: No "-verify"--this file should parse successfully on its own.
// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -module-cache-path %t/clang-module-cache %s

import UIKit

@UIApplicationMain // expected-error{{'UIApplicationMain' attribute cannot be used in a module that has a 'main.swift' file}}
class MyDelegate: NSObject, UIApplicationDelegate {
}

func hi() {}
