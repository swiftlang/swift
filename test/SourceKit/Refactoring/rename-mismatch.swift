struct Sss {
}

_ = Mismatch()
//  Mismatch()
_ = Sss()

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=syntactic-rename -rename-spec %S/syntactic-rename/rename-mismatch.in.json %s >> %t.result/rename-mismatch.expected
// RUN: diff -u %S/syntactic-rename/rename-mismatch.expected %t.result/rename-mismatch.expected

// RUN: %empty-directory(%t.ranges)
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %S/syntactic-rename/rename-mismatch.in.json %s >> %t.ranges/rename-mismatch.expected
// RUN: diff -u %S/find-rename-ranges/rename-mismatch.expected %t.ranges/rename-mismatch.expected
