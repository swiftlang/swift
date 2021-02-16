/// Something about variable global
let global = 1

/// Something about type Parent
struct Parent {
    /// Something about type Inner
    struct Inner {
        /// Something about variable innerMember
        let innerMember = 1
    }

    /// Something about variable member
    let member = 1
    /// Something aboout method
    func method() {}

    func localContext() {
        /// Something about type Local
        struct Local {
            // Something about variable localMember
            let localMember = 1
        }
    }
}

/// Something about extension of Parent
extension Parent {
    /// Something about method extensionMethod
    func extensionMethod()

    /// Something about type ExtInner
    struct ExtInner {
        /// Something about variable extInnerMember
        let extInnerMember = 10
    }	
}

/// Something about extension of Inner
extension Parent.Inner {
    /// Something about method extensionOfInnerMethod
    func extensionOfInnerMethod() {}

    class DeepNested<T> {
        /// Something about constructor deepNestedInit
    	init(deepNestedInit: T) {}
    }
}

/// Something about extension of DeepInner
extension Parent.Inner.DeepNested<T> where T: Equatable {
    ///	Something about method extensionOfDeepNestedMethod
    func extensionOfDeepNestedMethod() {
        /// Something about local deepNestedLocal
        var deepNestedLocal = {
            /// Something about InClosure
            struct InClosure {}
        }
        var deepNestedComputed: Int {
        	/// Something about InAccessor
        	struct InAccessor {}
        }
    }
}

// 1) Global symbols have no parents, but do have a symbol graph.
//
// RUN:  %sourcekitd-test -req=cursor -pos=2:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=GLOBAL %s
// RUN:  %sourcekitd-test -req=cursor -pos=5:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=GLOBAL %s
// RUN:  %sourcekitd-test -req=cursor -pos=27:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=GLOBAL %s
// RUN:  %sourcekitd-test -req=cursor -pos=39:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=GLOBAL %s
// RUN:  %sourcekitd-test -req=cursor -pos=50:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=GLOBAL %s
//
//
// GLOBAL-NOT: PARENT CONTEXTS BEGIN
// GLOBAL: SYMBOL GRAPH BEGIN
// GLOBAL-NOT: PARENT CONTEXTS BEGIN


// 2) Members within Parent and its extensions should list Parent as a parent
//    context, and have a symbol graph.
//
// RUN:  %sourcekitd-test -req=cursor -pos=7:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=13:9 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=15:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=29:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=32:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=39:18 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=50:18 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=PARENTMEMBER %s
//
//
// PARENTMEMBER: SYMBOL GRAPH BEGIN
// PARENTMEMBER: SYMBOL GRAPH END
// PARENTMEMBER: PARENT CONTEXTS BEGIN
// PARENTMEMBER-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// PARENTMEMBER-NEXT: PARENT CONTEXTS END


// 3) Members within Parent.Inner and its extensions should list both Inner and
//    Parent as parent contexts, and have a symbol graph.
//
// RUN:  %sourcekitd-test -req=cursor -pos=9:13 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=INNERMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=41:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=INNERMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=43:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=INNERMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=50:24 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=INNERMEMBER %s
//
// INNERMEMBER: SYMBOL GRAPH BEGIN
// INNERMEMBER: SYMBOL GRAPH END
// INNERMEMBER: PARENT CONTEXTS BEGIN
// INNERMEMBER-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// INNERMEMBER-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// INNERMEMBER-NEXT: PARENT CONTEXTS END


// 4) Members within Parent.Inner.DeepNested and its extensions should list
//    DeepNested, Inner, and Parent as parent contexts and have a symbol graph.
//
// RUN:  %sourcekitd-test -req=cursor -pos=45:9 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=DEEPNESTEDMEMBER %s
// RUN:  %sourcekitd-test -req=cursor -pos=52:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=DEEPNESTEDMEMBER %s
//
// DEEPNESTEDMEMBER: SYMBOL GRAPH BEGIN
// DEEPNESTEDMEMBER: SYMBOL GRAPH END
// DEEPNESTEDMEMBER: PARENT CONTEXTS BEGIN
// DEEPNESTEDMEMBER-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// DEEPNESTEDMEMBER-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// DEEPNESTEDMEMBER-NEXT: DeepNested swift.class s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedC
// DEEPNESTEDMEMBER-NEXT: PARENT


// 5) Members directly or indirectly within local contexts should show the
//    local contexts as parents if they're representable in the symbol graph.
//
// RUN:  %sourcekitd-test -req=cursor -pos=19:16 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=LOCAL %s
//
// LOCAL: PARENT CONTEXTS BEGIN
// LOCAL-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// LOCAL-NEXT: localContext() swift.method s:27cursor_symbol_graph_parents6ParentV12localContextyyF
// LOCAL-NEXT: PARENT CONTEXTS END
//
// RUN:  %sourcekitd-test -req=cursor -pos=21:17 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=TRANSITIVELOCAL %s
//
// TRANSITIVELOCAL: PARENT CONTEXTS BEGIN
// TRANSITIVELOCAL-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// TRANSITIVELOCAL-NEXT: localContext() swift.method s:27cursor_symbol_graph_parents6ParentV12localContextyyF
// TRANSITIVELOCAL-NEXT: Local swift.struct s:27cursor_symbol_graph_parents6ParentV12localContextyyF5LocalL_V
// TRANSITIVELOCAL-NEXT: PARENT CONTEXTS END
//
// RUN:  %sourcekitd-test -req=cursor -pos=56:20 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=CLOSUREINLOCAL %s
//
// CLOSUREINLOCAL: SYMBOL GRAPH BEGIN
// CLOSUREINLOCAL: "pathComponents": [
// CLOSUREINLOCAL-NEXT:    "Parent"
// CLOSUREINLOCAL-NEXT:    "Inner"
// CLOSUREINLOCAL-NEXT:    "DeepNested"
// CLOSUREINLOCAL-NEXT:    "extensionOfDeepNestedMethod()"
// CLOSUREINLOCAL-NEXT:    "InClosure"
// CLOSUREINLOCAL-NEXT: ],
// CLOSUREINLOCAL: SYMBOL GRAPH END
// CLOSUREINLOCAL: PARENT CONTEXTS BEGIN
// CLOSUREINLOCAL-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// CLOSUREINLOCAL-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// CLOSUREINLOCAL-NEXT: DeepNested swift.class s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedC
// CLOSUREINLOCAL-NEXT: extensionOfDeepNestedMethod() swift.method s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedCAASQRzlE011extensionOfgH6MethodyyF
// CLOSUREINLOCAL-NEXT: PARENT CONTEXTS END
//
// RUN:  %sourcekitd-test -req=cursor -pos=60:20 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=ACCESSORINLOCAL %s

// ACCESSORINLOCAL: SYMBOL GRAPH BEGIN
// ACCESSORINLOCAL: "pathComponents": [
// ACCESSORINLOCAL-NEXT:    "Parent",
// ACCESSORINLOCAL-NEXT:    "Inner",
// ACCESSORINLOCAL-NEXT:    "DeepNested",
// ACCESSORINLOCAL-NEXT:    "extensionOfDeepNestedMethod()",
// ACCESSORINLOCAL-NEXT:    "InAccessor"
// ACCESSORINLOCAL-NEXT: ],
// ACCESSORINLOCAL: PARENT CONTEXTS BEGIN
// ACCESSORINLOCAL-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// ACCESSORINLOCAL-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// ACCESSORINLOCAL-NEXT: DeepNested swift.class s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedC
// ACCESSORINLOCAL-NEXT: extensionOfDeepNestedMethod() swift.method s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedCAASQRzlE011extensionOfgH6MethodyyF
// ACCESSORINLOCAL-NEXT: PARENT CONTEXTS END
