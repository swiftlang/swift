func withString(body: (String) -> Void) {}

func test(array: [String]) {
  withString { element in
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):9 %s -- %s | %FileCheck %s
    let refToElement = element
    
    _ = invalid
  }
}

// CHECK: <Declaration>let refToElement: <Type usr="s:SS">String</Type></Declaration>
