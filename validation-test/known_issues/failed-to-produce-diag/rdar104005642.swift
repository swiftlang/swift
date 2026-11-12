// rdar://104005642
// RUN: %target-typecheck-verify-swift

struct Text {
  init<S: StringProtocol>(_ content: S) {}
}

struct Restaurant {
  var name: String
  var type: String
}

struct BasicTextImageRow {
  var restaurant: Restaurant

  var body: Text {
    Text(type) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}

let _ = type(of:) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
let _ = withoutActuallyEscaping(_:do:) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
let _ = _openExistential(_:do:) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}

class ACL {}

func secCall<Result>(_ body: (UnsafeMutablePointer<Result?>) -> Int32) -> Result {
  fatalError()
}

func copyOwnerAndACL(
  _ ownerType: UnsafeMutablePointer<UInt32>?,
  _ aclList: UnsafeMutablePointer<AnyObject?>?
) -> Int32 {
  0
}

func test() {
  var ownerType: UInt32 = 0
  let _ = secCall { copyOwnerAndACL(type, $0) } as! [ACL] // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
