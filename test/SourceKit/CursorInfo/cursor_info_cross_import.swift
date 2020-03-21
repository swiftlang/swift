import A
import B
import C

func foo(x: From_ABAdditionsType) {
    from_ABAdditions()
    from__ABAdditionsCAdditions()
    fromA()
    fromB()
    fromC()
}

// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=5:13 %s -- -Xfrontend -enable-cross-import-overlays -I %S/../Inputs/CrossImport %s  | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=6:5 %s -- -Xfrontend -enable-cross-import-overlays -I %S/../Inputs/CrossImport  %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=7:5 %s -- -Xfrontend -enable-cross-import-overlays -I %S/../Inputs/CrossImport  %s | %FileCheck -check-prefix=CHECK3 %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=8:5 %s -- -Xfrontend -enable-cross-import-overlays -I %S/../Inputs/CrossImport  %s | %FileCheck -check-prefix=CHECK4 %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=9:5 %s -- -Xfrontend -enable-cross-import-overlays -I %S/../Inputs/CrossImport  %s | %FileCheck -check-prefix=CHECK5 %s
// RUN: %sourcekitd-test -req=cursor -print-raw-response -pos=10:5 %s -- -Xfrontend -enable-cross-import-overlays -I %S/../Inputs/CrossImport  %s | %FileCheck -check-prefix=CHECK6 %s

// CHECK1: key.modulename: "A"
// CHECK2: key.modulename: "A"
// CHECK3: key.modulename: "A"
// CHECK4: key.modulename: "A"
// CHECK5: key.modulename: "B"
// CHECK6: key.modulename: "C"
