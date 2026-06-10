// RUN: %target-swift-frontend -typecheck -dump-availability-scopes %s -swift-version 5 > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace %s < %t.dump

// CHECK: {{^}}(root

// CHECK-NEXT: {{^}}  (decl version={{.*}} unavailable=* decl=universallyUnavailable()
@available(*, unavailable)
func universallyUnavailable() { }

// CHECK-NEXT: {{^}}  (decl version={{.*}} deprecated decl=universallyDeprecated()
@available(*, deprecated)
func universallyDeprecated() { }

// CHECK-NEXT: {{^}}  (decl version={{.*}} decl=introducedInSwift5()
@available(swift 5)
func introducedInSwift5() { }

// CHECK-NEXT: {{^}}  (decl version={{.*}} unavailable=swift decl=introducedInSwift6()
@available(swift 6)
func introducedInSwift6() { }

// CHECK-NEXT: {{^}}  (decl version={{.*}} unavailable=swift decl=obsoletedInSwift5()
@available(swift, obsoleted: 5)
func obsoletedInSwift5() { }

// CHECK-NEXT: {{^}}  (decl version={{.*}} decl=obsoletedInSwift6()
@available(swift, obsoleted: 6)
func obsoletedInSwift6() { }

// CHECK-NEXT: {{^}}  (decl version={{.*}} unavailable=* decl=UniversallyUnavailable
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=* decl=universallyUnavailable()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=* deprecated decl=universallyDeprecated()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=* decl=introducedInSwift5()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=*,swift decl=introducedInSwift6()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=*,swift decl=obsoletedInSwift5
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=* decl=obsoletedInSwift6()
@available(*, unavailable)
struct UniversallyUnavailable {
  @available(*, unavailable)
  func universallyUnavailable() { }

  @available(*, deprecated)
  func universallyDeprecated() { }

  @available(swift 5)
  func introducedInSwift5() { }

  @available(swift 6)
  func introducedInSwift6() { }

  @available(swift, obsoleted: 5)
  func obsoletedInSwift5() { }

  @available(swift, obsoleted: 6)
  func obsoletedInSwift6() { }
}

// CHECK-NEXT: {{^}}  (decl version={{.*}} unavailable=swift decl=IntroducedInSwift6
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=*,swift decl=universallyUnavailable()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=swift deprecated decl=universallyDeprecated()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=swift decl=introducedInSwift5()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=swift decl=introducedInSwift6()
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=swift decl=obsoletedInSwift5
// CHECK-NEXT: {{^}}    (decl version={{.*}} unavailable=swift decl=obsoletedInSwift6()
@available(swift 6)
struct IntroducedInSwift6 {
  @available(*, unavailable)
  func universallyUnavailable() { }

  @available(*, deprecated)
  func universallyDeprecated() { }

  @available(swift 5)
  func introducedInSwift5() { }

  @available(swift 6)
  func introducedInSwift6() { }

  @available(swift, obsoleted: 5)
  func obsoletedInSwift5() { }

  @available(swift, obsoleted: 6)
  func obsoletedInSwift6() { }
}
