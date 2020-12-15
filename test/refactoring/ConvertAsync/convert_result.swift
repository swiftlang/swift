func simple(_ completion: (Result<String, Error>) -> Void) { }
func noError(_ completion: (Result<String, Never>) -> Void) { }
func test(_ str: String) -> Bool { return false }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SIMPLE %s
simple { res in
  print("result \(res)")
}
// SIMPLE: let res = try await simple()
// SIMPLE-NEXT: print("result \(<#res#>)")

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NOERROR %s
noError { res in
  print("result \(res)")
}
// NOERROR: let res = await noError()
// NOERROR-NEXT: print("result \(<#res#>)")

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DOBLOCK %s
simple { res in
  print("before")
  switch res {
  case .success(let str):
    print("result \(str)")
  case .failure(let err):
    print("error \(err)")
  }
  print("after")
}
// DOBLOCK: do {
// DOBLOCK-NEXT: let str = try await simple()
// DOBLOCK-NEXT: print("before")
// DOBLOCK-NEXT: print("result \(str)")
// DOBLOCK-NEXT: print("after")
// DOBLOCK-NEXT: } catch let err {
// DOBLOCK-NEXT: print("error \(err)")
// DOBLOCK-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DOBLOCK %s
simple { res in
  print("before")
  if case .success(let str) = res {
    print("result \(str)")
  } else if case .failure(let err) = res {
    print("error \(err)")
  }
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DOBLOCK %s
simple { res in
  print("before")
  switch res {
  case .success(let str):
    print("result \(str)")
    break
  case .failure(let err):
    print("error \(err)")
    break
  }
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DOBLOCK %s
simple { res in
  print("before")
  switch res {
  case .success(let str):
    print("result \(str)")
    return
  case .failure(let err):
    print("error \(err)")
    return
  }
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SUCCESS %s
simple { res in
  print("before")
  if case .success(let str) = res {
    print("result \(str)")
  }
  print("after")
}
// SUCCESS: convert_result.swift
// SUCCESS-NEXT: let str = try await simple()
// SUCCESS-NEXT: print("before")
// SUCCESS-NEXT: print("result \(str)")
// SUCCESS-NEXT: print("after")
// SUCCESS-NOT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SUCCESS %s
simple { res in
  print("before")
  guard case .success(let str) = res else {
    return
  }
  print("result \(str)")
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DOBLOCKUNBOUND %s
simple { res in
  print("before")
  guard case .success(let str) = res else {
    print("err")
    return
  }
  print("result \(str)")
  print("after")
}
// DOBLOCKUNBOUND: do {
// DOBLOCKUNBOUND-NEXT: let str = try await simple()
// DOBLOCKUNBOUND-NEXT: print("before")
// DOBLOCKUNBOUND-NEXT: print("result \(str)")
// DOBLOCKUNBOUND-NEXT: print("after")
// DOBLOCKUNBOUND-NEXT: } catch {
// DOBLOCKUNBOUND-NEXT: print("err")
// DOBLOCKUNBOUND-NEXT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SUCCESS %s
simple { res in
  print("before")
  if let str = try? res.get() {
    print("result \(str)")
  }
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=DOBLOCKUNBOUND %s
simple { res in
  print("before")
  guard let str = try? res.get() else {
    print("err")
    return
  }
  print("result \(str)")
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=UNKNOWN %s
simple { res in
  print("before \(res)")
  if case .success(let str) = res {
    print("result \(str) \(try! res.get())")
  }
  print("after")
}
// UNKNOWN: convert_result.swift
// UNKNOWN-NEXT: let str = try await simple()
// UNKNOWN-NEXT: print("before \(<#str#>)")
// UNKNOWN-NEXT: print("result \(str) \(try! <#str#>.get())")
// UNKNOWN-NEXT: print("after")
// UNKNOWN-NOT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=UNKNOWNUNBOUND %s
simple { res in
  print("before \(res)")
  if case .success = res {
    print("result \(res) \(try! res.get())")
  }
  print("after")
}
// UNKNOWNUNBOUND: convert_result.swift
// UNKNOWNUNBOUND-NEXT: let res = try await simple()
// UNKNOWNUNBOUND-NEXT: print("before \(<#res#>)")
// UNKNOWNUNBOUND-NEXT: print("result \(<#res#>) \(try! <#res#>.get())")
// UNKNOWNUNBOUND-NEXT: print("after")
// UNKNOWN-NOT: }

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  if case .success(let str) = res {
    print("result \(str)")
  }
  if case .success(let str2) = res {
    print("result \(str2)")
  }
  print("after")
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  switch res {
  case .success(let str):
    print("result \(str)")
  case .failure(let err):
    print("error \(err)")
  default:
    print("default")
  }
  print("after")
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  switch res {
  case .success(let str):
    print("result \(str)")
  default:
    print("err")
  }
  print("after")
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  switch res {
  case .success, .failure:
    print("either")
  }
  print("after")
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  switch res {
  case .success, .failure:
    print("either")
  }
  print("after")
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  switch res {
  case .success:
    fallthrough
  case .failure:
    print("either")
  }
  print("after")
}

// RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
simple { res in
  print("before")
  switch res {
  case .success(let str) where str.hasPrefix("match"):
    print("pattern matched result \(str)")
  case .success(let str):
    print("result \(str)")
  case .failure(let err):
    print("error \(err)")
  }
  print("after")
}

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NESTEDRET %s
simple { res in
  print("before")
  switch res {
  case .success(let str):
    if test(str) {
      return
    }
    print("result \(str)")
  case .failure:
    break
  }
  print("after")
}
// NESTEDRET: convert_result.swift
// NESTEDRET-NEXT: let str = try await simple()
// NESTEDRET-NEXT: print("before")
// NESTEDRET-NEXT: if test(str) {
// NESTEDRET-NEXT: return
// NESTEDRET-NEXT: }
// NESTEDRET-NEXT: print("result \(str)")
// NESTEDRET-NEXT: print("after")
// NESTEDRET-NOT: }

// RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=NESTEDBREAK %s
simple { res in
  print("before")
  switch res {
  case .success(let str):
    if test(str) {
      break
    }
    print("result \(str)")
  case .failure:
    break
  }
  print("after")
}
// NESTEDBREAK: convert_result.swift
// NESTEDBREAK-NEXT: let str = try await simple()
// NESTEDBREAK-NEXT: print("before")
// NESTEDBREAK-NEXT: if test(str) {
// NESTEDBREAK-NEXT: break
// NESTEDBREAK-NEXT: }
// NESTEDBREAK-NEXT: print("result \(str)")
// NESTEDBREAK-NEXT: print("after")
// NESTEDBREAK-NOT: }

