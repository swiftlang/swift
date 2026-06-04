// RUN: %target-swift-frontend %s -O -c -o /dev/null

// rdar://23664856
// CommonSubexpressionElimination's processLazyPropertyGetters inlines the
// lazy getter into the caller; if the getter contains an `unreachable`,
// SILBuilder.createUnreachable sets `needCompleteLifetimes` on the caller
// in OSSA. CSE must call `completeLifetimes` before exiting to clear the
// flag.

@inline(never) func opaqueDouble() -> Double { return 1024 }

@inline(never) func opaqueBool() -> Bool { return true }

struct SizeClass {
    let isWideHorizontal: Bool
    init(width: Double) {
        self.isWideHorizontal = width > 600
    }
}

class Info {
    let editorialBg: Bool
    let arts: [Int]
    init() {
        self.editorialBg = opaqueBool()
        self.arts = [opaqueBool() ? 1 : 0]
    }
}

enum Grid {
    @inline(never)
    static func shouldAdd(size: SizeClass, arts: [Int]) -> Bool {
        return size.isWideHorizontal && arts.isEmpty
    }
}

class Base {
    var width: Double { opaqueDouble() }
}

class C: Base {
    let info: Info
    init(info: Info) { self.info = info }

    private lazy var sizeClass: SizeClass = {
        guard width > 0 else {
            fatalError("bad width")
        }
        return SizeClass(width: width)
    }()

    @inline(__always)
    var idiomIsMac: Bool { false }

    @inline(never)
    var shouldScale: Bool {
        return !idiomIsMac &&
            !sizeClass.isWideHorizontal &&
            info.editorialBg &&
            !Grid.shouldAdd(size: sizeClass, arts: info.arts)
    }
}

@inline(never)
public func driver() -> Bool {
    let c = C(info: Info())
    return c.shouldScale
}
