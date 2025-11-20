// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency
// REQUIRES: asserts


struct CopyableStruct {}
class Ref { var x = 0 } // expected-note 3{{class 'Ref' does not conform to the 'Sendable' protocol}}

struct FileDescriptor: Sendable, ~Copyable {
  var id = 0
}

enum MaybeFile: ~Copyable { // should implicitly conform
  case available(FileDescriptor)
  case closed
}

struct NotSendableMO: ~Copyable {
  var ref: Ref
}

// expect no warnings about sendable conformance when crossing actor boundaries:
func invalidFile() async -> FileDescriptor {
  return FileDescriptor(id: -1)
}

func takeNotSendable(_ nsmo: borrowing NotSendableMO) async {}

actor A {
  init(_ t: __owned FileDescriptor) {}
  init (_ t: __owned MaybeFile) {}
  func takeFileDescriptor(_ fd: __owned FileDescriptor) {}
  func takeMaybeFile(_ mfd: __owned MaybeFile) {}
  func giveFileDescriptor() -> MaybeFile {
    return .closed
  }

  func getRef() -> NotSendableMO { return NotSendableMO(ref: Ref()) }
}

@MainActor
func processFiles(_ a: A, _ anotherFile: borrowing FileDescriptor) async {
  let file = await invalidFile()
  await a.takeFileDescriptor(file)

  await a.takeMaybeFile(.available(anotherFile))
  _ = A(.available(anotherFile))

  let ns = await a.getRef()
  await takeNotSendable(ns)

  switch (await a.giveFileDescriptor()) {
  case let .available(fd):
    await a.takeFileDescriptor(fd)
  default:
    break
  }
}

func caller() async {
  await processFiles(A(invalidFile()), invalidFile())
}

// now make sure you can't form a Sendable existential from a move-only type.

struct RefPair: Sendable, ~Copyable {
  var left: Ref // expected-warning {{stored property 'left' of 'Sendable'-conforming struct 'RefPair' has non-Sendable type 'Ref'}}
  var right: Ref  // expected-warning {{stored property 'right' of 'Sendable'-conforming struct 'RefPair' has non-Sendable type 'Ref'}}
}

enum MaybeRef: Sendable, ~Copyable {
  case ref(Ref) // expected-warning {{associated value 'ref' of 'Sendable'-conforming enum 'MaybeRef' has non-Sendable type 'Ref'}}
  case null
}

enum OK_NoncopyableOption<T: Sendable> : Sendable, ~Copyable {
  case some(T)
  case none
}

enum Wrong_NoncopyableOption<T> : Sendable, ~Copyable { // expected-note {{consider making generic parameter 'T' conform to the 'Sendable' protocol}}
  case some(T) // expected-warning {{associated value 'some' of 'Sendable'-conforming generic enum 'Wrong_NoncopyableOption' has non-Sendable type 'T'}}
  case none
}

func takeAnySendable(_ s: any Sendable) {}
func takeSomeSendable(_ s: some Sendable) {} // expected-note {{'some Sendable & Copyable' is implicit here}}

protocol Munchable: ~Copyable {}
struct Chips: ~Copyable, Sendable, Munchable {}
func takeSomeMunchySendable(_ s: some Sendable & Munchable) {} // expected-note {{'some Sendable & Munchable & Copyable' is implicit here}}

// expected-error@+1 {{return expression of type 'FileDescriptor' does not conform to 'Copyable'}}
func mkSendable() -> Sendable { return FileDescriptor(id: 0) }

func tryToCastIt(_ fd: borrowing FileDescriptor) {
  let _: any Sendable = fd // expected-error {{value of type 'FileDescriptor' does not conform to specified type 'Copyable'}}
  let _: Sendable = fd // expected-error {{value of type 'FileDescriptor' does not conform to specified type 'Copyable'}}

  takeAnySendable(fd) // expected-error {{argument type 'FileDescriptor' does not conform to expected type 'Copyable'}}
  takeSomeSendable(fd) // expected-error {{global function 'takeSomeSendable' requires that 'FileDescriptor' conform to 'Copyable'}}
  takeSomeMunchySendable(Chips()) // expected-error {{global function 'takeSomeMunchySendable' requires that 'Chips' conform to 'Copyable'}}

  let _ = fd as Sendable // expected-error {{cannot convert value of type 'FileDescriptor' to type 'any Sendable' in coercion}}

  let _ = fd as? Sendable // expected-warning {{cast from 'FileDescriptor' to unrelated type 'any Sendable' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}

  let _ = fd as! Sendable // expected-warning {{cast from 'FileDescriptor' to unrelated type 'any Sendable' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}

  let _ = fd is Sendable // expected-warning {{cast from 'FileDescriptor' to unrelated type 'any Sendable' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}

  let sendy = mkSendable()
  let _ = sendy as FileDescriptor // expected-error {{cannot convert value of type 'any Sendable' to type 'FileDescriptor' in coercion}}
  let _ = sendy is FileDescriptor // expected-warning {{cast from 'any Sendable' to unrelated type 'FileDescriptor' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
  let _ = sendy as! FileDescriptor // expected-warning {{cast from 'any Sendable' to unrelated type 'FileDescriptor' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
  let _ = sendy as? FileDescriptor// expected-warning {{cast from 'any Sendable' to unrelated type 'FileDescriptor' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
}

protocol GiveSendable<T> {
  associatedtype T: Sendable // expected-note {{protocol requires nested type 'T'}}
  func give() -> T
}

// make sure witnessing associatedtypes is still prevented, even though we meet the explicit constraint.
class Bad: GiveSendable { 
  // expected-error@-1 {{type 'Bad' does not conform to protocol 'GiveSendable'}} 
  // expected-note@-2 {{add stubs for conformance}}
  typealias T = FileDescriptor // expected-note {{possibly intended match 'Bad.T' (aka 'FileDescriptor') does not conform to 'Copyable'}}
  func give() -> FileDescriptor { return FileDescriptor(id: -1) }
}

class Ok: GiveSendable {
  typealias T = CopyableStruct
  func give() -> CopyableStruct { return CopyableStruct() }
}

class Container<T> where T:Sendable {
  var elm: T
  init(_ t: T) { self.elm = t }
}

func createContainer(_ fd: borrowing FileDescriptor) {
  let _: Container<Sendable> = Container(fd) // expected-error {{argument type 'FileDescriptor' does not conform to expected type 'Copyable'}}
  let _: Container<Sendable> = Container(CopyableStruct())
}

struct PaperAirplaneFile: ~Copyable {
  var fd: FileDescriptor
}

extension PaperAirplaneFile: Sendable {}
