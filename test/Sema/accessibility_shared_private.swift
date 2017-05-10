// RUN: rm -rf %t && mkdir -p %t
// RUN: %utils/split_file.py -o %t %s
// RUN: not %target-swift-frontend -swift-version 4 -typecheck %t/declarations.swift %t/other_file_extensions.swift 2>&1 | %FileCheck -check-prefixes=DECLARED,OTHER %s
// RUN: not %target-swift-frontend -swift-version 4 -typecheck -primary-file %t/declarations.swift %t/other_file_extensions.swift 2>&1 | %FileCheck -check-prefixes=DECLARED %s
// RUN: not %target-swift-frontend -swift-version 4 -typecheck %t/declarations.swift -primary-file %t/other_file_extensions.swift 2>&1 | %FileCheck -check-prefixes=OTHER %s



// Padded to start on line 10 to make @LINE-10 work
// BEGIN declarations.swift
struct PrivateMembers  {
  private var privateCounter: Int = 0
  private func privateMethod() {}
  private struct PrivateInner {
    private struct Invisible {}
  }
}

extension PrivateMembers {
  private func usePrivate() {
    print(privateCounter)
    privateMethod()
    _ = PrivateInner()
    _ = PrivateInner.Invisible() // DECLARED: :[[@LINE-10]]:{{[^:]+}}: error: 'Invisible' is inaccessible due to 'private' protection level
  }
}

func using(_ obj: PrivateMembers) {
  print(obj.privateCounter) // DECLARED: :[[@LINE-10]]:{{[^:]+}}: error: 'privateCounter' is inaccessible due to 'private' protection level
  obj.privateMethod() // DECLARED: :[[@LINE-10]]:{{[^:]+}}: error: 'privateMethod' is inaccessible due to 'private' protection level
  obj.usePrivate() // DECLARED: :[[@LINE-10]]:{{[^:]+}}: error: 'usePrivate' is inaccessible due to 'private' protection level
  _ = PrivateMembers.PrivateInner() // DECLARED: :[[@LINE-10]]:{{[^:]+}}: error: 'PrivateInner' is inaccessible due to 'private' protection level
  _ = PrivateMembers.PrivateInner.Invisible() // DECLARED: :[[@LINE-10]]:{{[^:]+}}: error: 'PrivateInner' is inaccessible due to 'private' protection level
}

struct Outer {
  private static func privateDeclaration() {}
  struct Middle {
    private static func privateDeclaration() {}
    struct Inner {
      private static func privateDeclaration() {}
    }
  }
}

extension Outer.Middle.Inner {
  func useParentDeclarationPrivate() {
    Outer.privateDeclaration()
    Outer.Middle.privateDeclaration()
    Outer.Middle.Inner.privateDeclaration()
  }
}

// Padded to start on line 55 to make @LINE-55 work
// BEGIN other_file_extensions.swift
extension PrivateMembers {
  private func useDeclarationPrivate() {
    print(privateCounter) // OTHER: :[[@LINE-55]]:{{[^:]+}}: error: 'privateCounter' is inaccessible due to 'private' protection level
    privateMethod() // OTHER: :[[@LINE-55]]:{{[^:]+}}: error: 'privateMethod' is inaccessible due to 'private' protection level
    usePrivate() // OTHER: :[[@LINE-55]]:{{[^:]+}}: error: 'usePrivate' is inaccessible due to 'private' protection level
    _ = PrivateInner() // OTHER: :[[@LINE-55]]:{{[^:]+}}: error: 'PrivateInner' is inaccessible due to 'private' protection level
  }
}

extension PrivateMembers {
  private func useExtensionPrivate() {
    useDeclarationPrivate()
  }
}

extension Outer {
  private struct MiddleExtension {
    private static func privateDeclaration() {}
  }
  private static func outerExtension() {}
}

extension Outer.Middle.Inner {
  func useParentExtensionPrivate() {
    Outer.outerExtension()
    _ = Outer.MiddleExtension()
    Outer.MiddleExtension.privateDeclaration() // OTHER: :[[@LINE-55]]:{{[^:]+}}: error: 'privateDeclaration' is inaccessible due to 'private' protection level
  }
}
