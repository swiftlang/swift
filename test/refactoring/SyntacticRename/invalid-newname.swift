// RUN: not %refactor -syntactic-rename -source-filename %s -pos="ignore" -old-name "a" -new-name " "
// RUN: not %refactor -syntactic-rename -source-filename %s -pos="ignore" -is-function-like -old-name "foo(a:)" -new-name "foo( :)"
// RUN: not %refactor -syntactic-rename -source-filename %s -pos="ignore" -is-function-like -old-name "foo" -new-name "foo( )"

func /*ignore*/foo() {}