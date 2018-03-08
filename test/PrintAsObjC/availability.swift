// RUN: %empty-directory(%t)
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
// CHECK-NEXT: - (void)platUnavailableMessage SWIFT_AVAILABILITY(macos,unavailable,message="help I'm trapped in an availability factory");
// CHECK-NEXT: - (void)platUnavailableRename SWIFT_AVAILABILITY(macos,unavailable,message="'platUnavailableRename' has been renamed to 'plea'");
// CHECK-NEXT: - (void)platUnavailableRenameWithMessage SWIFT_AVAILABILITY(macos,unavailable,message="'platUnavailableRenameWithMessage' has been renamed to 'anotherPlea': still trapped");
// CHECK-NEXT: - (void)extensionUnavailable
// CHECK-DAG: SWIFT_AVAILABILITY(macos_app_extension,unavailable)
// CHECK-DAG: SWIFT_AVAILABILITY(ios_app_extension,unavailable)
// CHECK-DAG: SWIFT_AVAILABILITY(tvos_app_extension,unavailable)
// CHECK-DAG: SWIFT_AVAILABILITY(watchos_app_extension,unavailable)
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithX:(NSInteger)_ OBJC_DESIGNATED_INITIALIZER SWIFT_AVAILABILITY(macos,introduced=10.10);
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger simpleProperty;
// CHECK-NEXT: @property (nonatomic) NSInteger alwaysUnavailableProperty SWIFT_UNAVAILABLE_MSG("'alwaysUnavailableProperty' has been renamed to 'baz': whatever");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger alwaysDeprecatedProperty SWIFT_DEPRECATED_MSG("use something else", "quux");
// CHECK-NEXT: @property (nonatomic, readonly, strong) Availability * _Null_unspecified singlePlatCombinedPropertyClass SWIFT_AVAILABILITY(macos,introduced=10.7,deprecated=10.9,obsoleted=10.10);
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger platformUnavailableRenameWithMessageProperty SWIFT_AVAILABILITY(macos,unavailable,message="'platformUnavailableRenameWithMessageProperty' has been renamed to 'anotherPlea': still trapped");
// CHECK-NEXT: @end

// CHECK-LABEL: {{^}}SWIFT_AVAILABILITY(macos,introduced=999){{$}}
// CHECK-NEXT: @interface Availability (SWIFT_EXTENSION(availability))
// CHECK-NEXT: - (void)extensionAvailability:(WholeClassAvailability * _Nonnull)_;
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger propertyDeprecatedInsideExtension SWIFT_AVAILABILITY(macos,deprecated=10.10);
// CHECK-NEXT: @end

// CHECK-LABEL: @interface AvailabilitySub
// CHECK-NEXT: - (nonnull instancetype)init SWIFT_UNAVAILABLE;
// CHECK-NEXT: + (nonnull instancetype)new SWIFT_DEPRECATED_MSG("-init is unavailable");
// CHECK-NEXT: - (nonnull instancetype)initWithX:(NSInteger)_ SWIFT_UNAVAILABLE;
// CHECK-NEXT: @end

// CHECK-LABEL: SWIFT_CLASS("{{.+}}WholeClassAvailability") 
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=999)
// CHECK-NEXT: @interface WholeClassAvailability
// CHECK-NEXT: - (void)wholeClassAvailability:(id <WholeProtoAvailability> _Nonnull)_;
// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: @end

// CHECK-LABEL: SWIFT_PROTOCOL("{{.+}}WholeProtoAvailability{{.*}}") 
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=999)
// CHECK-NEXT: @protocol WholeProtoAvailability
// CHECK-NEXT: - (void)wholeProtoAvailability:(WholeClassAvailability * _Nonnull)_;
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

    @available(macOS, unavailable, message: "help I'm trapped in an availability factory")
    @objc func platUnavailableMessage() {}
    @available(macOS, unavailable, renamed: "plea")
    @objc func platUnavailableRename() {}
    @available(macOS, unavailable, renamed: "anotherPlea", message: "still trapped")
    @objc func platUnavailableRenameWithMessage() {}

    @available(macOSApplicationExtension, unavailable)
    @available(iOSApplicationExtension, unavailable)
    @available(tvOSApplicationExtension, unavailable)
    @available(watchOSApplicationExtension, unavailable)
    @objc func extensionUnavailable() {}

    @objc init() {}
    @available(macOS 10.10, *)
    @objc init(x _: Int) {}

    var simpleProperty: Int {
	get {
		return 100
	    }
    }
    @available(*, unavailable, message: "whatever", renamed: "baz")
    @objc var alwaysUnavailableProperty: Int {
	get {
		return 100
	    }
	set {
	    }
    }
    @available(*, deprecated, message: "use something else", renamed: "quux")
    @objc var alwaysDeprecatedProperty: Int {
	get {
		return -1
	    }
    }
    @available(macOS, introduced: 10.7, deprecated: 10.9, obsoleted: 10.10)
    @objc var singlePlatCombinedPropertyClass: Availability! {
	get {
		return nil
	    }
    }
    @available(macOS, unavailable, renamed: "anotherPlea", message: "still trapped")
    @objc var platformUnavailableRenameWithMessageProperty: Int {
	get {
		return -1
	    }
    }
}

// Deliberately a high number that the default deployment target will not reach.
@available(macOS 999, *)
extension Availability {
  @objc func extensionAvailability(_: WholeClassAvailability) {}


  @available(macOS, deprecated: 10.10)
  var propertyDeprecatedInsideExtension: Int {
	  get {
		  return 0
	  }
  }
}

@objc class AvailabilitySub: Availability {
    private override init() { super.init() }
    @available(macOS 10.10, *)
    private override init(x _: Int) { super.init() }
}


@available(macOS 999, *)
@objc class WholeClassAvailability {
  func wholeClassAvailability(_: WholeProtoAvailability) {}
}

@available(macOS 999, *)
@objc protocol WholeProtoAvailability {
  func wholeProtoAvailability(_: WholeClassAvailability)
}
