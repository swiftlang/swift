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

// 1) Global symbols should include only themselves.
//
// RUN:  %sourcekitd-test -req=cursor -pos=2:5 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=GLOBAL,GLOBAL_VAR %s
// RUN:  %sourcekitd-test -req=cursor -pos=5:8 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=GLOBAL,PARENT_STRUCT %s
// RUN:  %sourcekitd-test -req=cursor -pos=27:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=GLOBAL,PARENT_STRUCT %s
// RUN:  %sourcekitd-test -req=cursor -pos=39:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=GLOBAL,PARENT_STRUCT %s
// RUN:  %sourcekitd-test -req=cursor -pos=50:11 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=GLOBAL,PARENT_STRUCT %s
//
//
// GLOBAL: SYMBOL GRAPH BEGIN
// GLOBAL: PARENT CONTEXTS BEGIN
// GLOBAL_VAR-NEXT: global swift.var s:27cursor_symbol_graph_parents6globalSivp
// PARENT_STRUCT-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// GLOBAL-NEXT: PARENT CONTEXTS END


// 2) Members within Parent and its extensions should list themselves and
//    Parent.
//
// RUN:  %sourcekitd-test -req=cursor -pos=7:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,INNER_STRUCT %s
// RUN:  %sourcekitd-test -req=cursor -pos=13:9 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,MEMBER_VAR %s
// RUN:  %sourcekitd-test -req=cursor -pos=15:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,METHOD_FUNC %s
// RUN:  %sourcekitd-test -req=cursor -pos=29:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,EXTMETHOD_FUNC %s
// RUN:  %sourcekitd-test -req=cursor -pos=32:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,EXTINNER_STRUCT %s
// RUN:  %sourcekitd-test -req=cursor -pos=39:18 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,INNER_STRUCT %s
// RUN:  %sourcekitd-test -req=cursor -pos=50:18 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=PARENTMEMBER,INNER_STRUCT %s
//
//
// PARENTMEMBER: SYMBOL GRAPH BEGIN
// PARENTMEMBER: SYMBOL GRAPH END
// PARENTMEMBER: PARENT CONTEXTS BEGIN
// PARENTMEMBER-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// INNER_STRUCT-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// MEMBER_VAR-NEXT: member swift.property s:27cursor_symbol_graph_parents6ParentV6memberSivp
// METHOD_FUNC-NEXT: method() swift.method s:27cursor_symbol_graph_parents6ParentV6methodyyF
// EXTMETHOD_FUNC-NEXT: extensionMethod() swift.method s:27cursor_symbol_graph_parents6ParentV15extensionMethodyyF
// EXTINNER_STRUCT-NEXT: ExtInner swift.struct s:27cursor_symbol_graph_parents6ParentV8ExtInnerV
// PARENTMEMBER-NEXT: PARENT CONTEXTS END


// 3) Members within Parent.Inner and its extensions should list themselves,
//    Inner and Parent as parent contexts.
//
// RUN:  %sourcekitd-test -req=cursor -pos=9:13 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=INNERMEMBER,INNERMEMBER_VAR %s
// RUN:  %sourcekitd-test -req=cursor -pos=41:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=INNERMEMBER,EXTINNERMETHOD_FUNC %s
// RUN:  %sourcekitd-test -req=cursor -pos=43:12 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=INNERMEMBER,DEEPNESTED_CLASS %s
// RUN:  %sourcekitd-test -req=cursor -pos=50:24 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=INNERMEMBER,DEEPNESTED_CLASS %s
//
// INNERMEMBER: SYMBOL GRAPH BEGIN
// INNERMEMBER: SYMBOL GRAPH END
// INNERMEMBER: PARENT CONTEXTS BEGIN
// INNERMEMBER-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// INNERMEMBER-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// INNERMEMBER_VAR-NEXT: innerMember swift.property s:27cursor_symbol_graph_parents6ParentV5InnerV11innerMemberSivp
// EXTINNERMETHOD_FUNC-NEXT: extensionOfInnerMethod() swift.method s:27cursor_symbol_graph_parents6ParentV5InnerV011extensionOfF6MethodyyF
// DEEPNESTED_CLASS-NEXT: DeepNested swift.class s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedC
// INNERMEMBER-NEXT: PARENT CONTEXTS END


// 4) Members within Parent.Inner.DeepNested and its extensions should list
//    themselves DeepNested, Inner, and Parent.
//
// RUN:  %sourcekitd-test -req=cursor -pos=45:9 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=DEEPNESTEDMEMBER,DEEPNESTED_INIT %s
// RUN:  %sourcekitd-test -req=cursor -pos=52:10 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefixes=DEEPNESTEDMEMBER,EXTDEEPNESTEDMETHOD_FUNC %s
//
// DEEPNESTEDMEMBER: SYMBOL GRAPH BEGIN
// DEEPNESTEDMEMBER: SYMBOL GRAPH END
// DEEPNESTEDMEMBER: PARENT CONTEXTS BEGIN
// DEEPNESTEDMEMBER-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// DEEPNESTEDMEMBER-NEXT: Inner swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV
// DEEPNESTEDMEMBER-NEXT: DeepNested swift.class s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedC
// DEEPNESTED_INIT-NEXT: init(deepNestedInit:) swift.init s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedC04deepH4InitAGy__xGx_tcfc
// EXTDEEPNESTEDMETHOD_FUNC: extensionOfDeepNestedMethod() swift.method s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedCAASQRzlE011extensionOfgH6MethodyyF
// DEEPNESTEDMEMBER-NEXT: PARENT


// 5) Members directly or indirectly within local contexts should show
//    themselves and the local contexts as parents if they're representable in
//    the symbol graph.
//
// RUN:  %sourcekitd-test -req=cursor -pos=19:16 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=LOCAL %s
//
// LOCAL: PARENT CONTEXTS BEGIN
// LOCAL-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// LOCAL-NEXT: localContext() swift.method s:27cursor_symbol_graph_parents6ParentV12localContextyyF
// LOCAL-NEXT: Local swift.struct s:27cursor_symbol_graph_parents6ParentV12localContextyyF5LocalL_V
// LOCAL-NEXT: PARENT CONTEXTS END
//
// RUN:  %sourcekitd-test -req=cursor -pos=21:17 -req-opts=retrieve_symbol_graph=1 %s -- %s -target %target-triple | %FileCheck -check-prefix=TRANSITIVELOCAL %s
//
// TRANSITIVELOCAL: PARENT CONTEXTS BEGIN
// TRANSITIVELOCAL-NEXT: Parent swift.struct s:27cursor_symbol_graph_parents6ParentV
// TRANSITIVELOCAL-NEXT: localContext() swift.method s:27cursor_symbol_graph_parents6ParentV12localContextyyF
// TRANSITIVELOCAL-NEXT: Local swift.struct s:27cursor_symbol_graph_parents6ParentV12localContextyyF5LocalL_V
// TRANSITIVELOCAL-NEXT: localMember swift.property s:27cursor_symbol_graph_parents6ParentV12localContextyyF5LocalL_V0F6MemberSivp
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
// CLOSUREINLOCAL-NEXT: InClosure swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedCAASQRzlE011extensionOfgH6MethodyyFyycfU_9InClosureL_V
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
// ACCESSORINLOCAL-NEXT: InAccessor swift.struct s:27cursor_symbol_graph_parents6ParentV5InnerV10DeepNestedCAASQRzlE011extensionOfgH6MethodyyF04deepH8ComputedL_Sivg10InAccessorL_V
// ACCESSORINLOCAL-NEXT: PARENT CONTEXTS END
