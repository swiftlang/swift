// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
import Foundation

/// This is a doc comment on a public function.
public func documentedPublicFunc() {
  print("hello")
}

/// This is a doc comment on a private function.
private func documentedPrivateFunc() {
  print("private")
}

/** Block doc comment on a public struct. */
public struct DocumentedStruct {
  /// Doc comment on a public member.
  public var x: Int

  /// Doc comment on a private member.
  private func privateMethod() {}
}

// Line comment before public func.
public func afterLineComment() {}

// Line comment before private func.
private func afterLineCommentPrivate() {}

/* Block comment on internal func. */
internal func blockCommentedInternal() {}
//--- expected.swift
import Foundation

public func documentedPublicFunc()

private func documentedPrivateFunc()

public struct DocumentedStruct {
    public var x: Int

    private func privateMethod()
}

public func afterLineComment()

private func afterLineCommentPrivate()
