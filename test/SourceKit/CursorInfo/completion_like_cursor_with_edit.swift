// RUN: %sourcekitd-test -req=open %s -- %s == \
// RUN: -req=edit -pos=7:16 -length=0 -replace=yyy %s == \
// RUN: -req=cursor -pos=7:16 %s -- %s | %FileCheck %s

class SceneDelegate {
    func scene(_ scene: String?) {
        if let xxx = scene {
        }
    }
}

// CHECK: source.lang.swift.decl.var.local (7:16-7:22)
// CHECK-NEXT: yyyxxx
