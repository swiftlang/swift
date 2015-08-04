// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: OS=macosx

import AppKit

class MyDocument : NSDocument {
  override func readFromURL(URL: NSURL, ofType type: String) throws {
    try super.readFromURL(URL, ofType: type)
  }

  override func writeToURL(URL: NSURL, ofType type: String) throws {
    try super.writeToURL(URL, ofType: type)
  }
}

func test(URL: NSURL, controller: NSDocumentController) {
  try! NSDocument(contentsOfURL: URL, ofType: "") // expected-warning{{unused}}
  try! MyDocument(contentsOfURL: URL, ofType: "")

  try! controller.makeDocumentWithContentsOfURL(URL, ofType: "")
}

extension NSBox {
  func foo() {
    print("abc") // expected-warning {{use of 'print' treated as a reference to instance method in class 'NSView'}}
    // expected-note@-1 {{use 'self.' to silence this warning}} {{5-5=self.}}
    // expected-note@-2 {{use 'Swift.' to reference the global function}} {{5-5=Swift.}}
  }
}

class MyView : NSView {
  func foo() {
    print("abc") // expected-warning {{use of 'print' treated as a reference to instance method in class 'NSView'}}
    // expected-note@-1 {{use 'self.' to silence this warning}} {{5-5=self.}}
    // expected-note@-2 {{use 'Swift.' to reference the global function}} {{5-5=Swift.}}
  }
}

