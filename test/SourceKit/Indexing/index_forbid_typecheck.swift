// RUN: %sourcekitd-test -req=index %S/../Inputs/forbid_typecheck_primary.swift -- -Xfrontend -debug-forbid-typecheck-prefix -Xfrontend NOTYPECHECK %S/../Inputs/forbid_typecheck_2.swift %S/../Inputs/forbid_typecheck_primary.swift -module-name forbid_typecheck | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response
