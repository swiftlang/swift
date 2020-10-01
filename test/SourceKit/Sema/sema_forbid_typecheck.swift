// RUN: %sourcekitd-test -req=sema  %S/../Inputs/forbid_typecheck_primary.swift -- \
// RUN:     -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NOTYPECHECK -module-name forbid \
// RUN:     %S/../Inputs/forbid_typecheck_2.swift %S/../Inputs/forbid_typecheck_primary.swift > %t.response
// RUN: %diff -u %s.response %t.response
