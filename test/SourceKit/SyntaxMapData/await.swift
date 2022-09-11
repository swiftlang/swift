// RUN: %sourcekitd-test -req=syntax-map %s > %t.response
// RUN: %diff -u %s.response %t.response

func foo() async {}
func test() async {
  await foo()
}
