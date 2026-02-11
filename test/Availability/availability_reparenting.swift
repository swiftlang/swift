// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

// REQUIRES: OS=macosx

// REQUIRES: swift_feature_Reparenting

public protocol Seq: BorrowingSeq {
  func seq()
}

@available(macOS 99, *)
@reparentable public protocol BorrowingSeq {
  func borrowSeq()
}

@available(macOS 99, *)
extension Seq: @reparented BorrowingSeq {
  public func borrowSeq() {
    self.seq()
  }
}

func tryToUseRequirement(_ s: some Seq) { // expected-note {{add '@available' attribute to enclosing global function}}
  s.seq()
  s.borrowSeq() // expected-error {{'borrowSeq()' is only available in macOS 99 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
}

func correctedUse(_ s: some Seq) {
  if #available(macOS 99, *) {
    s.borrowSeq()
  } else {
    s.seq()
  }
}

func useAsBorrowingSeq1(_ bs: some BorrowingSeq) {} // expected-error 2{{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note 2{{}}
func useAsBorrowingSeq2<T>(_ t: T) where T: BorrowingSeq {} // expected-error {{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note {{}}

public struct AlwaysAvailableStruct: BorrowingSeq {
  public func borrowSeq() {}
}

protocol NotAReparenting: BorrowingSeq {} // expected-error {{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note{{add '@available' attribute to enclosing protocol}}

protocol ForgotAvailabilityOnReparenting: BorrowingSeq {}

extension ForgotAvailabilityOnReparenting: @reparented BorrowingSeq { // expected-error {{'BorrowingSeq' is only available in macOS 99 or newer}} // expected-note {{}}
  public func borrowSeq() {}
}
