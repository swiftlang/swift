// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

import enums_using_attributes

func testEvent(event: Event) {
  if event == .`init` { print("Initialize") }
  if event == .reset { print("Reset") }
}
