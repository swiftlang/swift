// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}swift-name-different-type.h

import SwiftNameDifferentType

func testB(_ b: B) {
  b.other()
}
