// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t.result
// RUN: diff %t.result %s.result

typealias x = (_ b: Int, _: String)
