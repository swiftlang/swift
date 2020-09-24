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


// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=10:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks %s  | %FileCheck -check-prefix=CHECKSWIFT %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=11:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks %s  | %FileCheck -check-prefix=CHECKSWIFT %s

// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=16:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefix=CHECKCLANG %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=17:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefix=CHECKCLANG %s

// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=22:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefix=CHECKOVERLAID %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=23:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefix=CHECKOVERLAID %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=24:1 %s -- -target %target-triple -Xfrontend -enable-cross-import-overlays -I %t/include -I %t/lib/swift -F %t/Frameworks  %s | %FileCheck -check-prefix=CHECKOVERLAID %s

// CHECKSWIFT: key.modulename: "SwiftFramework"
// CHECKCLANG: key.modulename: "ClangFramework"
// CHECKOVERLAID: key.modulename: "OverlaidClangFramework"
