// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/clang_conformances.sil -module-name clang_conformances -import-objc-header %S/Inputs/clang_conformances_helper.h
// RUN: %target-swift-frontend -emit-sil -o %t -I %t -primary-file %s -module-name main -O

// REQUIRES: objc_interop

// Don't crash when inlining a function that uses a conformance for a Clang
// decl that we haven't used in this file.

import clang_conformances

public func main() {
  inlineMe()
}
