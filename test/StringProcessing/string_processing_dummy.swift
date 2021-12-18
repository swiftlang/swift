// RUN: %target-swift-frontend -enable-experimental-string-processing -parse-as-library -emit-sil -verify %s

// REQUIRES: libswift,string_processing

import _MatchingEngine
import _StringProcessing

func foo() {
  _ = DummyMatchingEngine()
  let _: Regex<(Substring, DynamicCaptures)> = ''
}
