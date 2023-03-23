// RUN: %sourcekitd-test -req=cursor -req-opts=retrieve_symbol_graph=1 -pos=8:13 %s -- %s

struct AnyError: Swift.Error {
  let error: Swift.Error
}

func test(lhs: AnyError) {
  lhs.error._code
}
