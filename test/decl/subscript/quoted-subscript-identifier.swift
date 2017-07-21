// RUN: %target-swift-frontend -typecheck -verify %s
// SR-5513

class OTTextView {
  func `subscript`(_ sender: Any?) {}
}

class MyTextView: OTTextView {
  override func `subscript`(_ sender: Any?) { super.`subscript`(sender) }
}
