// RUN: %target-run-simple-swift %s
// REQUIRES: executable_test

//===----------------------------------------------------------------------===//
// Magic Literals
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct Magic: Hashable {
  let fileID: String
  let file: String
  let filePath: String
  let function: String
  let line: Int
  let column: Int
  let dsohandle: UnsafeRawPointer

  init(
    fileID: String = #fileID,
    file: String = #file,
    filePath: String = #filePath,
    function: String = #function,
    line: Int = #line,
    column: Int = #column,
    dsohandle: UnsafeRawPointer = #dsohandle
  ) {
    self.fileID = fileID
    self.file = file
    self.filePath = filePath
    self.function = function
    self.line = line
    self.column = column
    self.dsohandle = dsohandle
  }

  subscript(
    dynamicMember member: String,
    fileID: String = #fileID,
    file: String = #file,
    filePath: String = #filePath,
    function: String = #function,
    line: Int = #line,
    column: Int = #column,
    dsohandle: UnsafeRawPointer = #dsohandle
  ) -> Magic {
    return Magic(
      fileID: fileID,
      file: file,
      filePath: filePath,
      function: function,
      line: line,
      column: column,
      dsohandle: dsohandle
    )
  }

  func offset(lineBy lineOffset: Int, columnBy columnOffset: Int) -> Magic {
    Magic(
      fileID: fileID,
      file: file,
      filePath: filePath,
      function: function,
      line: line + lineOffset,
      column: column + columnOffset,
      dsohandle: dsohandle
    )
  }
}

/* ! ENSURE FORMATTERS ARE DISABLED TO PRESERVE WHITESPACE ! */
let m1 = Magic() // 71:15
let m2 = m1.member // 72:13
assert(m1.offset(lineBy: +1, columnBy: -2) == m2)

let m3 = m1
  .member // 76:4
assert(m1.offset(lineBy: +5, columnBy: -11) == m3)

let m4 = m1
  .
  member // 81:3
assert(m1.offset(lineBy: +10, columnBy: -12) == m4)

//===----------------------------------------------------------------------===//
// Inheritance
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
class Parent {
  subscript(
    dynamicMember member: String,
    kept: String = #fileID,
    overridden: String = #fileID
  ) -> String {
    [member, kept, overridden].joined(separator: ":")
  }
}

@dynamicMemberLookup
class Child: Parent {
  override subscript(
    dynamicMember member: String,
    kept: String = #fileID,
    overridden: String = #filePath
  ) -> String {
    [member, kept, overridden].joined(separator: ":")
  }
}

assert(Parent().member == ["member", #fileID, #fileID].joined(separator: ":"))
assert(Child().member == ["member", #fileID, #filePath].joined(separator: ":"))

//===----------------------------------------------------------------------===//
// Parameter Packs
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct Pack {
  subscript<each T>(dynamicMember member: String, pack: repeat each T) -> (repeat each T) {
    (repeat each pack)
  }

  subscript<each T, each U>(
    dynamicMember member: String, pack1 pack1: repeat each T, pack2 pack2: repeat each U
  ) -> Int {
    var count = 0
    for _ in repeat each pack1 {
      count += 1
    }

    for _ in repeat each pack2 {
      count += 1
    }

    return count
  }
}

let p = Pack()
assert(p.member == ())
assert(p.member == 0)

//===----------------------------------------------------------------------===//
// Isolation
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct Isolated {
  subscript(dynamicMember member: String, isolation: isolated (any Actor)? = #isolation) -> (any Actor)? {
    isolation
  }
}

Task<Void, Never>.startSynchronously { @MainActor in
  let `actor` = await Isolated().actor
  assert(`actor` === MainActor.shared)
}

