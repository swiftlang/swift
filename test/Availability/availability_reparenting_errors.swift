// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

// REQUIRES: OS=macosx

// REQUIRES: swift_feature_Reparenting

public protocol Seq: BorrowingSeq {
  func seq()
}

@available(macOS 99, *)
@reparentable public protocol BorrowingSeq {
  func borrowSeq()
  var borrowCount: Int { get set }
  associatedtype BorrowIter
}

@available(macOS 99, *)
extension Seq: @reparented BorrowingSeq where BorrowIter == String {
  public func borrowSeq() {
    self.seq()
  }

  public var borrowCount: Int { get { } set { } }
}

func tryToUseRequirement(_ s: some Seq) { // expected-note 2{{add '@available'}}
  s.seq()
  s.borrowSeq() // expected-error{{'borrowSeq()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available'}}

  _ = s.borrowCount // expected-error{{'borrowCount' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available'}}
}

func correctedUse(_ s: some Seq) {
  if #available(macOS 99, *) {
    s.borrowSeq()
    _ = s.borrowCount
  } else {
    s.seq()
  }
}

func useAsBorrowingSeq1(_ bs: some BorrowingSeq) {} // expected-error 2{{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note 2{{}}
func useAsBorrowingSeq2<T>(_ t: T) where T: BorrowingSeq {} // expected-error {{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note {{}}

public struct AlwaysAvailableStruct: BorrowingSeq {
  public typealias BorrowIter = Self
  public func borrowSeq() {}
  public var borrowCount: Int { get { 0 } set { } }
}

protocol NotAReparenting: BorrowingSeq {} // expected-error {{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note{{add '@available' attribute to enclosing protocol}}

protocol ForgotAvailabilityOnReparenting: BorrowingSeq {}

extension ForgotAvailabilityOnReparenting: @reparented BorrowingSeq where BorrowIter == Float { // expected-note 2{{add}}
// expected-error@-1 {{'BorrowingSeq' is only available in macOS 99 or newer}}
// expected-error@-2 {{'BorrowIter' is only available in macOS 99 or newer}}

  public func borrowSeq() {} // expected-error {{cannot declare a public instance method in an extension with internal requirements}}

  public var borrowCount: Int { get { } set { } } // expected-error {{cannot declare a public property in an extension with internal requirements}}
}
