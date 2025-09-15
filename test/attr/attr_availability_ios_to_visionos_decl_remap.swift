// RUN: %empty-directory(%t/mock-sdk)
// RUN: cp %S/../Inputs/MockPlatformRemapSDKConfig/SDKSettings.json %t/mock-sdk/SDKSettings.json
// RUN: %swift -typecheck -verify -parse-stdlib -target arm64-apple-xros1.0 %s -sdk %t/mock-sdk

@available(iOS 17.4, *)
public func doSomething() { }

@available(iOS 99.0, *)
public func doSomethingFarFuture() { }

@available(iOS, introduced: 16.0, obsoleted: 17.0, message: "you don't want to do that anyway")
public func doSomethingElse() { }
// expected-note@-1 3 {{'doSomethingElse()' was obsoleted in visionOS 1.0}}

@available(iOS, introduced: 16.0, deprecated: 17.0, message: "please don't")
public func doSomethingInadvisable() { }

@available(iOS 17.0, *)
public func doSomethingGood() { }

@available(iOS 13.0, *)
public func doSomethingOld() { }

public func takesSomeProto(_ p: some SomeProto) { }

public protocol SomeProto { }

public struct ConformsToProtoIniOS17_4 { }

@available(iOS 17.4, *)
extension ConformsToProtoIniOS17_4: SomeProto { }

public struct ConformsToProtoIniOS99 { }

@available(iOS 99, *)
extension ConformsToProtoIniOS99: SomeProto { }

public struct ConformsToProtoDeprecatedIniOS17 { }

@available(iOS, introduced: 16.0, deprecated: 17.0, message: "please don't")
extension ConformsToProtoDeprecatedIniOS17: SomeProto { }

public struct ConformsToProtoObsoletedIniOS17 { }

@available(iOS, introduced: 16.0, obsoleted: 17.0, message: "you don't want to do that anyway")
extension ConformsToProtoObsoletedIniOS17: SomeProto { }
// expected-note@-1 {{conformance of 'ConformsToProtoObsoletedIniOS17' to 'SomeProto' was obsoleted in visionOS 1.0}}


func testDeploymentTarget() {
  // expected-note@-1 6 {{add '@available' attribute to enclosing global function}}
  doSomething() // expected-error {{'doSomething()' is only available in visionOS 1.1 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}{{3-16=if #available(visionOS 1.1, *) {\n      doSomething()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  doSomethingFarFuture() // expected-error {{'doSomethingFarFuture()' is only available in iOS 99.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}{{3-25=if #available(iOS 99.0, *) {\n      doSomethingFarFuture()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable in visionOS: you don't want to do that anyway}}
  doSomethingInadvisable() // expected-warning {{'doSomethingInadvisable()' was deprecated in iOS 1.0: please don't}}
  doSomethingGood()
  doSomethingOld()

  takesSomeProto(ConformsToProtoIniOS17_4()) // expected-warning {{conformance of 'ConformsToProtoIniOS17_4' to 'SomeProto' is only available in visionOS 1.1 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}{{3-45=if #available(visionOS 1.1, *) {\n      takesSomeProto(ConformsToProtoIniOS17_4())\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  takesSomeProto(ConformsToProtoIniOS99()) // expected-warning {{conformance of 'ConformsToProtoIniOS99' to 'SomeProto' is only available in iOS 99 or newer; this is an error in the Swift 6 language mode}}
  // expected-note@-1 {{add 'if #available' version check}}{{3-43=if #available(iOS 99, *) {\n      takesSomeProto(ConformsToProtoIniOS99())\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  takesSomeProto(ConformsToProtoDeprecatedIniOS17()) // expected-warning {{conformance of 'ConformsToProtoDeprecatedIniOS17' to 'SomeProto' was deprecated in iOS 1.0: please don't}}
  takesSomeProto(ConformsToProtoObsoletedIniOS17()) // expected-error {{conformance of 'ConformsToProtoObsoletedIniOS17' to 'SomeProto' is unavailable in visionOS: you don't want to do that anyway}}

  if #available(iOS 17.4, *) {
    // #available(iOS ...) is not matched when compiling for visionOS
    doSomething() // expected-error {{'doSomething()' is only available in visionOS 1.1 or newer}}
    // expected-note@-1 {{add 'if #available' version check}}{{5-18=if #available(visionOS 1.1, *) {\n        doSomething()\n    \} else {\n        // Fallback on earlier versions\n    \}}}

    takesSomeProto(ConformsToProtoIniOS17_4()) // expected-warning {{conformance of 'ConformsToProtoIniOS17_4' to 'SomeProto' is only available in visionOS 1.1 or newer; this is an error in the Swift 6 language mode}}
    // expected-note@-1 {{add 'if #available' version check}}{{5-47=if #available(visionOS 1.1, *) {\n        takesSomeProto(ConformsToProtoIniOS17_4())\n    \} else {\n        // Fallback on earlier versions\n    \}}}

    if #available(iOS 17.1, *) { }
  }

  if #available(visionOS 2.0, *) {
    doSomething()
  }
}

@available(iOS 17.4, *)
func testAfterDeployment_iOS() {
  doSomething()
  doSomethingElse() // expected-error {{'doSomethingElse()' is unavailable in visionOS: you don't want to do that anyway}}
  doSomethingFarFuture() // expected-error {{'doSomethingFarFuture()' is only available in iOS 99.0 or newer}}
  // expected-note@-1 {{add 'if #available' version check}}
  doSomethingInadvisable() // expected-warning {{'doSomethingInadvisable()' was deprecated in iOS 1.0: please don't}}
  doSomethingGood()
  doSomethingOld()

  takesSomeProto(ConformsToProtoIniOS17_4())
}

@available(visionOS 2.0, *)
func testAfterDeployment_visionOS() {
  doSomething()
  doSomethingElse() // expected-error{{'doSomethingElse()' is unavailable in visionOS: you don't want to do that anyway}}
  doSomethingInadvisable() // expected-warning {{'doSomethingInadvisable()' was deprecated in iOS 1.0: please don't}}
  doSomethingGood()
  doSomethingOld()

  takesSomeProto(ConformsToProtoIniOS17_4())
}
