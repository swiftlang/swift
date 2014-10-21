// RUN: rm -rf %t && mkdir %t
// RUN: %swift -sdk %sdk -emit-module -o %t %S/Inputs/clang_conformances.sil -module-name clang_conformances
// RUN: %swift -sdk %sdk -emit-sil -o %t -I %t -primary-file %s -module-name main -O

// Don't crash when inlining a function that uses a conformance for a Clang
// decl that we haven't used in this file.

import clang_conformances

public func main() {
  inlineMe()
}
