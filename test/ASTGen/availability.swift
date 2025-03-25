// RUN: %empty-directory(%t)

// RUN: COMPILER_ARGS=( \
// RUN:   -define-availability '_iOS53Aligned:macOS 50.0, iOS 53.0' \
// RUN:   -define-availability '_iOS54Aligned:macOS 51.0, iOS 54.0' \
// RUN:   -define-availability '_iOS54:iOS 54.0' \
// RUN:   -define-availability '_macOS51_0:macOS 51.0' \
// RUN:   -define-availability '_myProject 1.0:macOS 51.0' \
// RUN:   -define-availability '_myProject 2.5:macOS 52.5' \
// RUN: )

// RUN: %target-swift-frontend-dump-parse "${COMPILER_ARGS[@]}" \
// RUN:   -enable-experimental-feature ParserASTGen \
// RUN:   | %sanitize-address > %t/astgen.ast

// RUN: %target-swift-frontend-dump-parse "${COMPILER_ARGS[@]}" \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift "${COMPILER_ARGS[@]}" \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: shell
// REQUIRES: swift_feature_ParserASTGen

@available(swift 4)
func testSwift4OrLater() {}

@available(macOS 12, iOS 13.1, *)
func testShorthandMulti() {}

@available(macOS, unavailable)
func testUnavailableMacOS() {}

@available(macOS, deprecated: 12.0.5, message: "whatever")
func testDeprecaed12MacOS() {}

@available(_iOS53Aligned, *)
func testMacroNameOnly() {}

@available(_myProject 2.5, *)
func testMacroWithVersion() {}

@_specialize(exported: true, availability: _iOS54Aligned, *; where T == Int)
func testSpecialize<T>(arg: T) -> T {}

@backDeployed(before: _iOS53Aligned)
public func testBackDeployed() {}

@available(macOS 10, iOS 12, *)
@_originallyDefinedIn(module: "OriginalModule", macOS 12.0, iOS 23.2)
public func testOriginallyDefinedIn() {}


func testPoundIf() {
  if #available(_myProject 2.5, *) {
    // pass
  } else if #unavailable(macOS 80) {
    // pass
  } else if #_hasSymbol(Int.self) { // expected-warning {{struct 'Int' is not a weakly linked declaration}}
    // pass
  }
}

public class ClassWithMembers {
  @_spi_available(macOS 10.15, *)
  public func spiFunc() {}
}
