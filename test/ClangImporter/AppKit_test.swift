// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: OS=macosx

import AppKit

class MyDocument : NSDocument {
  override func read(from url: URL, ofType type: String) throws {
    try super.read(from: url, ofType: type)
  }

  override func write(to url: URL, ofType type: String) throws {
    try super.write(to: url, ofType: type)
  }
}

func test(_ url: URL, controller: NSDocumentController) {
  try! NSDocument(contentsOf: url, ofType: "") // expected-warning{{result of 'NSDocument' initializer is unused}}
  try! MyDocument(contentsOf: url, ofType: "") // expected-warning{{result of 'NSDocument' initializer is unused}}

  try! controller.makeDocument(withContentsOf: url, ofType: "")
}

extension NSBox {
  func foo() {
    print("abc" as NSString) // expected-warning {{use of 'print' treated as a reference to instance method in class 'NSView'}}
    // expected-note@-1 {{use 'self.' to silence this warning}} {{5-5=self.}}
    // expected-note@-2 {{use 'Swift.' to reference the global function}} {{5-5=Swift.}}
  }
}

class MyView : NSView {
  func foo() {
    print("abc" as NSString) // expected-warning {{use of 'print' treated as a reference to instance method in class 'NSView'}}
    // expected-note@-1 {{use 'self.' to silence this warning}} {{5-5=self.}}
    // expected-note@-2 {{use 'Swift.' to reference the global function}} {{5-5=Swift.}}
  }
}

