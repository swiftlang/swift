
var foo: Double { 1 / 2 }
var bar: Regex<Substring> { /x/ }
var baz: Regex<Substring> { / x/ }
var qux: Regex<Substring> { / x}/ }

// FIXME: Shouldn't need parse-as-library (https://github.com/swiftlang/swift/issues/84785)
// Check that we are not crashing
// RUN: %sourcekitd-test \
// RUN: -req=complete -pos=2:18 %s -- -enable-bare-slash-regex -parse-as-library %s == \
// RUN: -req=complete -pos=3:28 %s -- -enable-bare-slash-regex -parse-as-library %s == \
// RUN: -req=complete -pos=4:28 %s -- -enable-bare-slash-regex -parse-as-library %s == \
// RUN: -req=complete -pos=5:28 %s -- -enable-bare-slash-regex -parse-as-library %s

// REQUIRES: swift_swift_parser
