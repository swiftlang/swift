
enum X {
  case first(Int, String)
  case second(Int, String)
  case third(Int, String)
  case fourth(Int, String)
  case fifth(Int, String)
}

let p = X.first(3, "hello")

switch p {
  case .first(let x, let y)
    print("foo \(x) \(y)")
    fallthrough
  case .second(let x, let y), .third(let x, let y):
    print("bar \(x) \(y)")
  default:
    print("other")
}

// RUN: %sourcekitd-test -req=cursor -pos=13:19 %s -- %s | %FileCheck -check-prefixes=CHECKX,CHECK1DECL %s
// RUN: %sourcekitd-test -req=cursor -pos=14:18 %s -- %s | %FileCheck -check-prefixes=CHECKX,CHECK1REF %s

// CHECK1DECL: source.lang.swift.decl.var.local (13:19-13:20)
// CHECK1REF: source.lang.swift.ref.var.local (13:19-13:20)

// RUN: %sourcekitd-test -req=cursor -pos=16:20 %s -- %s | %FileCheck -check-prefixes=CHECKX,CHECK2DECL %s
// RUN: %sourcekitd-test -req=cursor -pos=16:42 %s -- %s | %FileCheck -check-prefixes=CHECKX,CHECK2DECL2 %s
// RUN: %sourcekitd-test -req=cursor -pos=17:18 %s -- %s | %FileCheck -check-prefixes=CHECKX,CHECK2REF %s

// CHECK2DECL: source.lang.swift.decl.var.local (16:20-16:21)
// CHECK2DECL2: source.lang.swift.decl.var.local (16:42-16:43)
// CHECK2REF: source.lang.swift.ref.var.local (16:42-16:43)

// CHECKX: x
// CHECKX: s:33cursor_vardecl_across_fallthrough1xL_Sivp
// CHECKX: Int


// RUN: %sourcekitd-test -req=cursor -pos=13:26 %s -- %s | %FileCheck -check-prefixes=CHECKY,CHECK3DECL %s
// RUN: %sourcekitd-test -req=cursor -pos=14:23 %s -- %s | %FileCheck -check-prefixes=CHECKY,CHECK3REF %s

// CHECK3DECL: source.lang.swift.decl.var.local (13:26-13:27)
// CHECK3REF: source.lang.swift.ref.var.local (13:26-13:27)

// RUN: %sourcekitd-test -req=cursor -pos=16:27 %s -- %s | %FileCheck -check-prefixes=CHECKY,CHECK4DECL %s
// RUN: %sourcekitd-test -req=cursor -pos=16:49 %s -- %s | %FileCheck -check-prefixes=CHECKY,CHECK4DECL2 %s
// RUN: %sourcekitd-test -req=cursor -pos=17:23 %s -- %s | %FileCheck -check-prefixes=CHECKY,CHECK4REF %s

// CHECK4DECL: source.lang.swift.decl.var.local (16:27-16:28)
// CHECK4DECL2: source.lang.swift.decl.var.local (16:49-16:50)
// CHECK4REF: source.lang.swift.ref.var.local (16:49-16:50)

// CHECKY: y
// CHECKY: s:33cursor_vardecl_across_fallthrough1yL_SSvp
// CHECKY: String
