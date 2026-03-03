// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module -enable-library-evolution -emit-module-path=%t/A.swiftmodule -module-name=A %S/Inputs/mangle-opaque-return-types-A.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -emit-ir  %s
import A

public struct C<T, Content: Proto> {
  let data: T
  let content: Content

  init(_ t: T, _ c: Content) {
    data = t
    content = c
  }

  public var dontCrash : some Proto {
    return Group(Choice(content, EmptyProto().add()))
  }
}

