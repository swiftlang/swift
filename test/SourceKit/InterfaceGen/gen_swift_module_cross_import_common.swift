
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mcp)
// RUN: cp -r %S/../../CrossImport/Inputs/lib-templates/* %t/
// RUN: %{python} %S/../../CrossImport/Inputs/rewrite-module-triples.py %t %module-target-triple

// 1) Check the interface shows the decls from each of SwiftFramework's cross-import overlays.
//
// RUN: %sourcekitd-test -req=interface-gen -module SwiftFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.SwiftFramework.response %t.response

// Make sure cursor info within the generated interface of SwiftFramework on one of the
// decls originally from a cross-import decls shows 'SwiftFramework' as the parent module.
//
// RUN: %sourcekitd-test -req=interface-gen-open -module SwiftFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp == -req=cursor -print-raw-response -pos=7:13 -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -Xfrontend -enable-cross-import-overlays > %t.response
// RUN: %FileCheck --input-file %t.response --check-prefix=CHECKSWIFT %s
//
// CHECKSWIFT: key.name: "fromSwiftFrameworkCrossImport()"
// CHECKSWIFT: key.modulename: "SwiftFramework"


// 2) Check the interface shows the decls from each of ClangFramework's cross-import overlays.
//
// RUN: %sourcekitd-test -req=interface-gen -module ClangFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.ClangFramework.response %t.response

// Make sure cursor info within the generated interface of ClangFramework on one of the
// decls originally from a cross-import decls shows 'ClangFramework' as the parent module.
//
// RUN: %sourcekitd-test -req=interface-gen-open -module ClangFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp == -req=cursor -print-raw-response -pos=7:13 -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -Xfrontend -enable-cross-import-overlays > %t.response
// RUN: %FileCheck --input-file %t.response --check-prefix=CHECKCLANG %s
//
// CHECKCLANG: key.name: "fromClangFrameworkCrossImport()"
// CHECKCLANG: key.modulename: "ClangFramework"


// 2) Check the interface shows the decls from each of OverlaidClangFramework's cross-import overlays.
//
// RUN: %sourcekitd-test -req=interface-gen -module OverlaidClangFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp > %t.response
// RUN: %diff -u %s.OverlaidClangFramework.response %t.response

// Make sure cursor info within the generated interface of OverlaidClangFramework on one of the
// decls originally from a cross-import decls shows 'OverlaidClangFramework' as the parent module.
//
// RUN: %sourcekitd-test -req=interface-gen-open -module OverlaidClangFramework -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -module-cache-path %t/mcp == -req=cursor -print-raw-response -pos=9:13 -- -target %target-triple -I %t/include -I %t/lib/swift -F %t/Frameworks -Xfrontend -enable-cross-import-overlays > %t.response
// RUN: %FileCheck --input-file %t.response --check-prefix=CHECKOVERLAIDCLANG %s
//
// CHECKOVERLAIDCLANG: key.name: "fromOverlaidClangFrameworkCrossImport()"
// CHECKOVERLAIDCLANG: key.modulename: "OverlaidClangFramework"





