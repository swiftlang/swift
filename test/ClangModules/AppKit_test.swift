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
