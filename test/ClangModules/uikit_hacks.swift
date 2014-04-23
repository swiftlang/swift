// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -parse-as-library -verify -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 %s

import Foundation

// Check that we drop the variadic parameter from certain UIKit initializers.
func makeAnActionSheet() -> SomeActionSheet {
  return SomeActionSheet(withTitle: "Error",
                         delegate: nil,
                         cancelButtonTitle: "Cancel",
                         destructiveButtonTitle: "OK")
}

func makeAnAlertView() -> SomeAlertView {
  return SomeAlertView(withTitle: "Error",
                       message: "The operation completed successfully.",
                       delegate: nil,
                       cancelButtonTitle: "Abort")
}
