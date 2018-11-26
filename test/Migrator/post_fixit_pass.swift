// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %s -emit-migrated-file-path %t/post_fixit_pass.swift.result -o /dev/null -F %S/mock-sdk -swift-version 4
// RUN: diff -u %S/post_fixit_pass.swift.expected %t/post_fixit_pass.swift.result

#if swift(>=4.2)
  public struct SomeAttribute: RawRepresentable {
    public init(rawValue: Int) { self.rawValue = rawValue }
    public init(_ rawValue: Int) { self.rawValue = rawValue }
    public var rawValue: Int
    public typealias RawValue = Int
  }
#else
  public typealias SomeAttribute = Int
#endif

func foo(_ d: SomeAttribute) {
  let i: Int = d
}

