// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -verify -module-cache-path %t/clang-module-cache %s

import UIKit

// Check that we drop the variadic parameter from certain UIKit initializers.
func makeAnActionSheet() -> UIActionSheet {
  return UIActionSheet(title: "Error",
                       delegate: nil,
                       cancelButtonTitle: "Cancel",
                       destructiveButtonTitle: "OK")
}

func makeAnAlertView() -> UIAlertView {
  return UIAlertView(title: "Error",
                     message: "The operation completed successfully.",
                     delegate: nil,
                     cancelButtonTitle: "Abort")
}
