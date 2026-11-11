// rdar://141113848
// RUN: %target-typecheck-verify-swift

class TextField {
  func resignFirstResponder() {}
}

class ViewController {
  weak var redTextField: TextField!
  weak var greenTextField: TextField!
  weak var blueTextField: TextField!

  func doneButtonTapped() {
    for textField in (redTextField, greenTextField, blueTextField) { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
      textField.resignFirstResponder()
    }
  }
}
