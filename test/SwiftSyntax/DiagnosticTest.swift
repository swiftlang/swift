// RUN: %target-swift 2>&1 | %FileCheck %s

import StdlibUnittest

func loc(_ file: String = #file, line: Int = #line, 
         column: Int = #line) -> SourceLocation {
  return SourceLocation(file: file, line: line, column: column, offset: 0)
}

try runTool { engine in
  let startLoc = loc()
  let fixLoc = loc()

  // CHECK: error: cannot convert value of type 'Int' to 'Bool'
  // CHECK-NEXT: note: check for explicit equality to '0'
  engine.diagnose(.error("cannot convert value of type 'Int' to 'Bool'",
                         location: startLoc,
                         notes: [
                           Note("check for explicit equality to '0'",
                                location: fixLoc,
                                fixIts: [
                                  .insert(fixLoc, " != 0")
                                ])
                         ]))
  return 0
}
