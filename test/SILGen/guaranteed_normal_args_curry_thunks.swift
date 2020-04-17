// RUN: %target-swift-emit-silgen -parse-as-library -module-name Swift -parse-stdlib %s

// This test checks specific codegen related to converting curry thunks to
// canonical representations when we are emitting guaranteed normal
// arguments. Eventually it will be merged into the normal SILGen tests.

//////////////////
// Declarations //
//////////////////

precedencegroup AssignmentPrecedence {
  assignment: true
}

enum Optional<T> {
case none
case some(T)
}

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word) {
  // This would usually contain an assert, but we don't need one since we are
  // just emitting SILGen.
}

class Klass {
  init() {}
}

typealias AnyObject = Builtin.AnyObject

protocol ProtocolInitNoArg {
  init()
}

protocol ClassProtocolInitNoArg : class {
  init()
}

protocol ProtocolInitAddressOnly {
  associatedtype SubType : ProtocolInitNoArg

  init(t: SubType)
  init(t2: AddrOnlyStructInitGeneric<Klass>)
  init(t3: AddrOnlyStructInitGeneric<SubType>)
}

protocol ProtocolInitClassProtocol {
  associatedtype SubType : ClassProtocolInitNoArg

  init(t: SubType)
}

protocol ProtocolInitLoadable {
  init(t: Klass)
}

protocol ClassProtocolInitAddressOnly : class {
  associatedtype SubType : ProtocolInitNoArg

  init(t: SubType)
}

protocol ClassProtocolInitClassProtocol : class {
  associatedtype SubType : ClassProtocolInitNoArg

  init(t: SubType)
}

protocol ClassProtocolInitLoadable : class {
  init(t: Klass)
}

class LoadableClassInitLoadable {
  init(_ k: Klass) {}
}

struct LoadableStructInitLoadable {
  var k: Klass

  init(_ newK: Klass) {
    k = newK
  }
}

struct AddrOnlyStructInitGeneric<T> {
  var k: T

  init(_ newK: T) {
    k = newK
  }
}

///////////
// Tests //
///////////

// There used to be FileCheck tests here. Now that curry thunks are built
// in CSApply, there's no point writing them out in great detail again.
// Let's just make sure we don't hit any assertions or SIL verifier
// failures.

func testAddrOnlyStructInitGenericConcrete() {
  let x = AddrOnlyStructInitGeneric<Klass>.init
  let y = x(Klass())
}

func testAddrOnlyStructInitGenericAddrOnly<T : ProtocolInitAddressOnly>(t: T) {
  let x = AddrOnlyStructInitGeneric<T.SubType>.init
  let y = x(T.SubType())
}

func testGenericInitClass<T : ProtocolInitLoadable>(t: T) {
  let x = T.init
  let y = x(Klass())
}
