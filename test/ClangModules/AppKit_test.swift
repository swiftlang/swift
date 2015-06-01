// RUN: %target-parse-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import AppKit

class MyDocument : NSDocument {
  override func readFromURL(URL: NSURL, ofType type: String) throws {
    try super.readFromURL(URL, ofType: type)
  }

  override func writeToURL(URL: NSURL, ofType type: String) throws {
    try super.writeToURL(URL, ofType: type)
  }
}

func test(URL: NSURL) {
  try! NSDocument(contentsOfURL: URL, ofType: "")
  try! MyDocument(contentsOfURL: URL, ofType: "")
}
