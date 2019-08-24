// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/generic_shared_function_helper.sil -module-name SILSource
// RUN: %target-swift-frontend -emit-module -o %t -I %t -primary-file %s -module-name main

// Just don't crash when using -primary-file and re-serializing a
// generic shared_external SIL function.

import SILSource

public func main() {
  public_func() as Int?
}
