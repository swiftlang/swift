// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx13 -swift-version 5 -solver-scope-threshold=2000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

protocol P {
    var s: S { get }
}

struct S {
    static var standard: S {
        fatalError()
    }
}

struct P1: P {
    var s: S = .standard
}
struct P2: P {
    var s: S = .standard
}
struct P3: P {
    var s: S = .standard
}
struct P4: P {
    var s: S = .standard
}
struct P5: P {
    var s: S = .standard
}

enum E {
    case p1(P1)
    case p2(P2)
    case p3(P3)
    case p4(P4)
    case p5(P5)
}

struct B {
    let e: E
    
    @ToolbarContentBuilder
    var content: some ToolbarContent {
        switch e {
        case .p1(_):
            ToolbarItem { Color.red }
        case .p2(_):
            ToolbarItem { Color.red }
        case .p3(_):
            ToolbarItem { Color.red }
        case .p4(_):
            ToolbarItem { Color.red }
        case .p5(_):
            ToolbarItem { Color.red }
        }
    }
}

