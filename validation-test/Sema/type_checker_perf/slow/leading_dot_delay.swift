// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100

// https://forums.swift.org/t/typechecker-performance-is-way-worse-in-6-3/84779

class NSColor: Equatable {
  init(red: Double, green: Double, blue: Double, alpha: Double) {}

  static func ==(_: NSColor, _: NSColor) -> Bool { return false }
}

var backgroundColor: NSColor
var optBackgroundColor: NSColor?

func XCTAssertEqual<T: Equatable>(_: T, _: T) {}

func test() {
  XCTAssertEqual(backgroundColor, .init(red: 244.0 / 255.0, green: 218.0 / 255.0, blue: 129.0 / 255.0, alpha: 1.0))
  XCTAssertEqual(backgroundColor, .init(red: 244 / 255.0, green: 218 / 255.0, blue: 129 / 255.0, alpha: 1.0))  // expected-error {{reasonable time}}
  XCTAssertEqual(backgroundColor, NSColor(red: 244.0 / 255.0, green: 218.0 / 255.0, blue: 129.0 / 255.0, alpha: 1.0))
  XCTAssertEqual(backgroundColor, NSColor(red: 244 / 255.0, green: 218 / 255.0, blue: 129 / 255.0, alpha: 1.0))
  XCTAssertEqual(backgroundColor, NSColor.init(red: 244.0 / 255.0, green: 218.0 / 255.0, blue: 129.0 / 255.0, alpha: 1.0))
  XCTAssertEqual(backgroundColor, NSColor.init(red: 244 / 255.0, green: 218 / 255.0, blue: 129 / 255.0, alpha: 1.0))

  XCTAssertEqual(optBackgroundColor, .init(red: 244.0 / 255.0, green: 218.0 / 255.0, blue: 129.0 / 255.0, alpha: 1.0))
  XCTAssertEqual(optBackgroundColor, .init(red: 244 / 255.0, green: 218 / 255.0, blue: 129 / 255.0, alpha: 1.0))  // expected-error {{reasonable time}}
  XCTAssertEqual(optBackgroundColor, NSColor(red: 244.0 / 255.0, green: 218.0 / 255.0, blue: 129.0 / 255.0, alpha: 1.0))
  XCTAssertEqual(optBackgroundColor, NSColor(red: 244 / 255.0, green: 218 / 255.0, blue: 129 / 255.0, alpha: 1.0))
  XCTAssertEqual(optBackgroundColor, NSColor.init(red: 244.0 / 255.0, green: 218.0 / 255.0, blue: 129.0 / 255.0, alpha: 1.0))
  XCTAssertEqual(optBackgroundColor, NSColor.init(red: 244 / 255.0, green: 218 / 255.0, blue: 129 / 255.0, alpha: 1.0))
}
