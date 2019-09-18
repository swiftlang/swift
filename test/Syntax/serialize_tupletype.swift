// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t.result
// RUN: diff %t.result %s.result

typealias x = (b: Int, _: String)
