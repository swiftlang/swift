// RUN: %swift %clang-importer-sdk -parse -verify %s

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
