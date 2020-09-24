// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mcp)
// RUN: cp -r %S/../../CrossImport/Inputs/lib-templates/* %t/
// RUN: %{python} %S/../../CrossImport/Inputs/rewrite-module-triples.py %t %module-target-triple


// RUN: %sourcekitd-test -req=doc-info -module SwiftFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.SwiftFramework.response %t.response

// RUN: %sourcekitd-test -req=doc-info -module ClangFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.ClangFramework.response %t.response

// RUN: %sourcekitd-test -req=doc-info -module OverlaidClangFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.OverlaidClangFramework.response %t.response