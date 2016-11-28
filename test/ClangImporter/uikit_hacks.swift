// RUN: %target-swift-frontend -typecheck %s

// REQUIRES: OS=ios
// FIXME: this test could use the %clang-importer-sdk, but swift tries to
// deserialize the real UIKit module and match it up with the mock SDK, and
// crashes.  Another reason to stop using source imports.

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
