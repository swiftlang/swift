// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/availability.swiftmodule -typecheck -emit-objc-header-path %t/availability.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/availability.h
// RUN: %check-in-clang %t/availability.h

// REQUIRES: objc_interop

// CHECK-LABEL: @interface Availability{{$}}
// CHECK-NEXT: - (void)alwaysAvailable;
// CHECK-NEXT: - (void)alwaysUnavailable SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (void)alwaysUnavailableTwo SWIFT_UNAVAILABLE_MSG("stuff happened");
// CHECK-NEXT: - (void)alwaysUnavailableThree SWIFT_UNAVAILABLE_MSG("'alwaysUnavailableThree' has been renamed to 'bar'");
// CHECK-NEXT: - (void)alwaysUnavailableFour SWIFT_UNAVAILABLE_MSG("'alwaysUnavailableFour' has been renamed to 'baz': whatever");
// CHECK-NEXT: - (void)alwaysDeprecated SWIFT_DEPRECATED;
// CHECK-NEXT: - (void)alwaysDeprecatedTwo SWIFT_DEPRECATED_MSG("it's old");
// CHECK-NEXT: - (void)alwaysDeprecatedThree SWIFT_DEPRECATED_MSG("", "qux");
// CHECK-NEXT: - (void)alwaysDeprecatedFour SWIFT_DEPRECATED_MSG("use something else", "quux");
// CHECK-NEXT: - (void)escapeMessage SWIFT_DEPRECATED_MSG("one\ntwo\tthree\x0Dfour\\ \"five\"");
// CHECK-NEXT: - (void)unicodeMessage SWIFT_DEPRECATED_MSG("über");
// CHECK-NEXT: - (void)singlePlatShorthand SWIFT_AVAILABILITY(macos,introduced=10.10);
// CHECK-NEXT: - (void)multiPlatShorthand
// CHECK-DAG: SWIFT_AVAILABILITY(macos,introduced=10.11)
// CHECK-DAG: SWIFT_AVAILABILITY(ios,introduced=9.0)
// CHECK-DAG: SWIFT_AVAILABILITY(tvos,introduced=9.0)
// CHECK-DAG: SWIFT_AVAILABILITY(watchos,introduced=3.0)
// CHECK-NEXT: - (void)singlePlatIntroduced SWIFT_AVAILABILITY(ios,introduced=9.0);
// CHECK-NEXT: - (void)singlePlatDeprecated SWIFT_AVAILABILITY(macos,deprecated=10.10);
// CHECK-NEXT: - (void)singlePlatDeprecatedTwo SWIFT_AVAILABILITY(macos,deprecated=10.10,message="'singlePlatDeprecatedTwo' has been renamed to 'flubber'");
// CHECK-NEXT: - (void)singlePlatDeprecatedThree SWIFT_AVAILABILITY(macos,deprecated=10.10,message="'singlePlatDeprecatedThree' has been renamed to 'fozzybear': we changed our minds");
// CHECK-NEXT: - (void)singlePlatDeprecatedAlways SWIFT_AVAILABILITY(tvos,deprecated=0.0.1);
// CHECK-NEXT: - (void)singlePlatDeprecatedAlwaysTwo SWIFT_AVAILABILITY(macos,introduced=10.7,deprecated=10.7);
// CHECK-NEXT: - (void)singlePlatUnavailable SWIFT_AVAILABILITY(watchos,unavailable);
// CHECK-NEXT: - (void)singlePlatUnavailableTwo SWIFT_AVAILABILITY(watchos,unavailable);
// CHECK-NEXT: - (void)singlePlatObsoleted SWIFT_AVAILABILITY(ios,obsoleted=8.1);
// CHECK-NEXT: - (void)singlePlatCombined SWIFT_AVAILABILITY(macos,introduced=10.7,deprecated=10.9,obsoleted=10.10);
// CHECK-NEXT: - (void)multiPlatCombined
// CHECK-DAG: SWIFT_AVAILABILITY(macos,introduced=10.6,deprecated=10.8,obsoleted=10.9)
// CHECK-DAG: SWIFT_AVAILABILITY(ios,introduced=7.0,deprecated=9.0,obsoleted=10.0)
// CHECK-NEXT: - (void)extensionUnavailable
// CHECK-DAG: SWIFT_AVAILABILITY(macos_app_extension,unavailable)
// CHECK-DAG: SWIFT_AVAILABILITY(ios_app_extension,unavailable)
// CHECK-DAG: SWIFT_AVAILABILITY(tvos_app_extension,unavailable)
// CHECK-DAG: SWIFT_AVAILABILITY(watchos_app_extension,unavailable)
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithX:(NSInteger)_ OBJC_DESIGNATED_INITIALIZER SWIFT_AVAILABILITY(macos,introduced=10.10);
// CHECK-NEXT: @end
// CHECK-LABEL: @interface AvailabilitySub
// CHECK-NEXT: - (nonnull instancetype)init SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initWithX:(NSInteger)_ SWIFT_UNAVAILABLE;
// CHECK-NEXT: @end
@objc class Availability {
    @objc func alwaysAvailable() {}

    @available(*, unavailable)
    @objc func alwaysUnavailable() {}
    @available(*, unavailable, message: "stuff happened")
    @objc func alwaysUnavailableTwo() {}
    @available(*, unavailable, renamed: "bar")
    @objc func alwaysUnavailableThree() {}
    @available(*, unavailable, message: "whatever", renamed: "baz")
    @objc func alwaysUnavailableFour() {}

    @available(*, deprecated)
    @objc func alwaysDeprecated() {}
    @available(*, deprecated, message: "it's old")
    @objc func alwaysDeprecatedTwo() {}
    @available(*, deprecated, renamed: "qux")
    @objc func alwaysDeprecatedThree() {}
    @available(*, deprecated, message: "use something else", renamed: "quux")
    @objc func alwaysDeprecatedFour() {}

    @available(*, deprecated, message: "one\ntwo\tthree\rfour\\ \"five\"")
    @objc func escapeMessage() {}
    @available(*, deprecated, message: "über")
    @objc func unicodeMessage() {}

    @available(macOS 10.10, *)
    @objc func singlePlatShorthand() {}
    @available(macOS 10.11, iOS 9.0, tvOS 9.0, watchOS 3.0, *)
    @objc func multiPlatShorthand() {}

    @available(iOS, introduced: 9.0)
    @objc func singlePlatIntroduced() {}
    @available(macOS, deprecated: 10.10)
    @objc func singlePlatDeprecated() {}
    @available(macOS, deprecated: 10.10, renamed: "flubber")
    @objc func singlePlatDeprecatedTwo() {}
    @available(macOS, deprecated: 10.10, message: "we changed our minds", renamed: "fozzybear")
    @objc func singlePlatDeprecatedThree() {}
    @available(tvOS, deprecated)
    @objc func singlePlatDeprecatedAlways() {}
    @available(macOS, introduced: 10.7, deprecated)
    @objc func singlePlatDeprecatedAlwaysTwo() {}
    @available(watchOS, unavailable)
    @objc func singlePlatUnavailable() {}
    @available(watchOS, introduced: 2.0, unavailable)
    @objc func singlePlatUnavailableTwo() {}
    @available(iOS, obsoleted: 8.1)
    @objc func singlePlatObsoleted() {}
    @available(macOS, introduced: 10.7, deprecated: 10.9, obsoleted: 10.10)
    @objc func singlePlatCombined() {}

    @available(macOS, introduced: 10.6, deprecated: 10.8, obsoleted: 10.9)
    @available(iOS, introduced: 7.0, deprecated: 9.0, obsoleted: 10.0)
    @objc func multiPlatCombined() {}

    @available(macOSApplicationExtension, unavailable)
    @available(iOSApplicationExtension, unavailable)
    @available(tvOSApplicationExtension, unavailable)
    @available(watchOSApplicationExtension, unavailable)
    @objc func extensionUnavailable() {}

    @objc init() {}
    @available(macOS 10.10, *)
    @objc init(x _: Int) {}
}

@objc class AvailabilitySub: Availability {
    private override init() { super.init() }
    @available(macOS 10.10, *)
    private override init(x _: Int) { super.init() }
}
