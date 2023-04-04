// RUN: %target-typecheck-verify-swift

enum Destination {
  case inbox
  case messageThread(id: Int)
  case profile(userId: Int, name: String)
}

let value = Destination.inbox
let _ = value is case .inbox // Ok

let value2 = Destination.messageThread(id: 42)
let _ = value2 is case .messageThread // Ok
let _ = value2 is case .messageThread(_) // Ok
let _ = value2 is case .messageThread(id: 42) // Ok
let _ = value2 is case .messageThread(let x) // expected-error {{'let' binding cannot appear in 'is case' expression}}

let value3 = Destination.profile(userId: 42, name: "John Doe")
let _ = value3 is case .profile(userId: 42, _) // Ok
