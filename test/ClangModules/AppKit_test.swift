// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: OS=macosx

import AppKit

class MyDocument : NSDocument {
  override func readFrom(URL: NSURL, ofType type: String) throws {
    try super.readFrom(URL, ofType: type)
  }

  override func writeTo(URL: NSURL, ofType type: String) throws {
    try super.writeTo(URL, ofType: type)
  }
}

func test(URL: NSURL, controller: NSDocumentController) {
  try! NSDocument(contentsOf: URL, ofType: "") // expected-warning{{unused}}
  try! MyDocument(contentsOf: URL, ofType: "")

  try! controller.makeDocumentWithContentsOf(URL, ofType: "")
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

