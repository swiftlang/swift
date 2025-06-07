func save(_ record: Int, _ x: (String) -> Void) {}

func test() {
  let record = 2
// RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):19 %s -- %s
  save(record) { (record) in
  }
}
