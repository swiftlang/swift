// RUN: %empty-directory(%t)
// RUN: cp -r %S/../../CrossImport/Inputs/lib-templates/* %t/
// RUN: %{python} %S/../../CrossImport/Inputs/rewrite-module-triples.py %t %module-target-triple

import BystandingLibrary


import SwiftFramework

fromSwiftFramework()
fromSwiftFrameworkCrossImport()


import ClangFramework

fromClangFramework()
fromClangFrameworkCrossImport()


import OverlaidClangFramework

fromOverlaidClangFramework()
fromOverlaidClangFrameworkOverlay()
fromOverlaidClangFrameworkCrossImport()


// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=10:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks %s  | %FileCheck -check-prefixes=CHECKSWIFT,CHECKNOBYSTANDERS %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=11:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks %s  | %FileCheck -check-prefixes=CHECKSWIFT,CHECKBYSTANDERS %s

// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=16:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefixes=CHECKCLANG,CHECKNOBYSTANDERS %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=17:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefixes=CHECKCLANG,CHECKBYSTANDERS %s

// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=22:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefixes=CHECKOVERLAID,CHECKNOBYSTANDERS %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=23:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefixes=CHECKOVERLAID,CHECKNOBYSTANDERS %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=24:1 -req-opts=retrieve_symbol_graph=1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefixes=CHECKOVERLAID,CHECKBYSTANDERS %s

// CHECKSWIFT: key.modulename: "SwiftFramework"
// CHECKSWIFT:      key.symbol_graph: "{
// CHECKSWIFT-SAME:   \"module\":{
// CHECKSWIFT-SAME:      \"name\":\"SwiftFramework\"

// CHECKCLANG: key.modulename: "ClangFramework"
// CHECKCLANG:      key.symbol_graph: "{
// CHECKCLANG-SAME:   \"module\":{
// CHECKCLANG-SAME:      \"name\":\"ClangFramework\"

// CHECKOVERLAID: key.modulename: "OverlaidClangFramework"
// CHECKOVERLAID:      key.symbol_graph: "{
// CHECKOVERLAID-SAME:   \"module\":{
// CHECKOVERLAID-SAME:      \"name\":\"OverlaidClangFramework\"

// CHECKBYSTANDERS:  \"bystanders\":[\"BystandingLibrary\"]
// CHECKNOBYSTANDERS-NOT: \"bystanders\":
