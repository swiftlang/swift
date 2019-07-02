public static func buildBlock(
    _ shapes: PathShape...
) -> PathShape {
    return nil
}

class C {
    init(a: Int,
        b: Int
) {}
    subscript(index1: Int,
              index2: Int
) -> Int { get {} }
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=13 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: ") -> PathShape {"
// CHECK: key.sourcetext: "    ) {}"
// CHECK: key.sourcetext: "    ) -> Int { get {} }"
