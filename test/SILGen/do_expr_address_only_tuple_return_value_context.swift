// RUN: %target-swift-emit-silgen -verify -enable-experimental-feature DoExpressions %s

// REQUIRES: swift_feature_DoExpressions

struct BigNontrivialThing {
  var x: Any
  var y: Any
}

func throwBNT() throws -> BigNontrivialThing? { fatalError() }
func nothrowBNT() -> BigNontrivialThing? { fatalError() }
func throwStr() throws -> String { fatalError() }
func nothrowStr() -> String { fatalError() }

func foo() throws -> (BigNontrivialThing?, String) {
  do {
    (nothrowBNT(), try throwStr())
  } catch _ where .random() {
    (try throwBNT(), nothrowStr())
  } catch _ where .random() {
    (try throwBNT(), try throwStr())
  } catch {
    (nothrowBNT(), nothrowStr())
  }
}
