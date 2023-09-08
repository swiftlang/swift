
var foo: Double { 1 / 2 }
var bar: Regex<Substring> { /x/ }
var baz: Regex<Substring> { / x/ }
var qux: Regex<Substring> { / x}/ }

// Check that we are not crashing
// RUN: %sourcekitd-test \
// RUN: -req=complete -pos=2:18 %s -- -enable-bare-slash-regex %s == \
// RUN: -req=complete -pos=3:28 %s -- -enable-bare-slash-regex %s == \
// RUN: -req=complete -pos=4:28 %s -- -enable-bare-slash-regex %s == \
// RUN: -req=complete -pos=5:28 %s -- -enable-bare-slash-regex %s

// REQUIRES: swift_swift_parser
