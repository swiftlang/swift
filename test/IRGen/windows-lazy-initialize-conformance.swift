// RUN: %target-swift-frontend -S -emit-ir %s -o - | %FileCheck %s
// REQUIRES: OS=windows-msvc

class C {
  static func encode(_ c: Encodable) throws {}
}

public protocol P: Codable {}

extension String: P {}

public enum E {
  case associated(P)
}

extension E: Encodable {
  public func encode(to encoder: Encoder) throws {
    switch self {
    case .associated(let p):
      try p.encode(to: encoder)
    }
  }
}

try? print(C.encode(E.associated("string")))

// CHECK-NOT: store i8** getelementptr inbounds ([3 x i8*], [3 x i8*]* @"$sSS4main1PAAWP", i32 0, i32 0), i8***
