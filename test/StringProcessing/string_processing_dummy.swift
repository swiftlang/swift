// RUN: %target-swift-frontend -enable-experimental-string-processing -parse-as-library -emit-sil -verify %s

// REQUIRES: string_processing

import _MatchingEngine
import _StringProcessing

func foo() {
  _ = DummyMatchingEngine()
  _ = DummyRegex<Substring>()
}
