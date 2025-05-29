// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/availability.swiftmodule -typecheck -verify -emit-objc-header-path %t/availability.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
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
// CHECK-NEXT: - (void)escapeMessage SWIFT_DEPRECATED_MSG("one\ntwo\tthree\rfour\\ \"five\"{U+0000}six");
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
// CHECK-SAME: ;
// CHECK-NEXT: - (void)overloadMethodWithFirst:(NSInteger)first second:(NSInteger)second;
// CHECK-NEXT: - (void)deprecatedMethodRenamedToOverloadMethodWithFirst:(NSInteger)first second:(NSInteger)second SWIFT_DEPRECATED_MSG("", "overloadMethodWithFirst:second:");
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodRenamedToOverloadMethodWithFirst:(NSInteger)first second:(NSInteger)second SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodRenamedToOverloadMethod' has been renamed to 'overloadMethodWithFirst:second:'");
// CHECK-NEXT: - (void)unavailableMethodRenamedToOverloadMethodWithFirst:(NSInteger)first second:(NSInteger)second SWIFT_UNAVAILABLE_MSG("'unavailableMethodRenamedToOverloadMethod' has been renamed to 'overloadMethodWithFirst:second:'");
// CHECK-NEXT: - (void)unavailableOnMacOSMethodRenamedToOverloadMethodWithFirst:(NSInteger)first second:(NSInteger)second SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodRenamedToOverloadMethod' has been renamed to 'overloadMethodWithFirst:second:'");

// CHECK-NEXT: - (void)firstOverloadingMethodWithDifferenceNameWithFirst:(NSInteger)first second:(NSInteger)second;
// CHECK-NEXT: - (void)secondOverloadingMethodWithDifferenceNameWithFirst:(double)first second:(double)second;
// CHECK-NEXT: - (void)deprecatedMethodRenamedToOverloadMethodWithDifferenceNameWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "firstOverloadingMethodWithDifferenceNameWithFirst:second:");
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodRenamedToOverloadMethodWithDifferenceNameWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodRenamedToOverloadMethodWithDifferenceName' has been renamed to 'firstOverloadingMethodWithDifferenceNameWithFirst:second:'");
// CHECK-NEXT: - (void)unavailableMethodRenamedToOverloadMethodWithDifferenceNameWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodRenamedToOverloadMethodWithDifferenceName' has been renamed to 'firstOverloadingMethodWithDifferenceNameWithFirst:second:'");
// CHECK-NEXT: - (void)unavailableOnMacOSMethodRenamedToOverloadMethodWithDifferenceNameWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodRenamedToOverloadMethodWithDifferenceName' has been renamed to 'firstOverloadingMethodWithDifferenceNameWithFirst:second:'");

// CHECK-NEXT: + (void)deprecatedAvailabilityWithValue:(NSInteger)value;
// CHECK-NEXT: - (void)deprecatedInstanceMethodRenamedToClassMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_DEPRECATED_MSG("This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here", "classMethodWithACustomObjCName(x:)");
// CHECK-NEXT: - (void)deprecatedOnMacOSInstanceMethodRenamedToClassMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSInstanceMethodRenamedToClassMethod' has been renamed to 'classMethodWithACustomObjCName(x:)': This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here");
// CHECK-NEXT: - (void)unavailableInstanceMethodRenamedToClassMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableInstanceMethodRenamedToClassMethod' has been renamed to 'classMethodWithACustomObjCName(x:)': This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here");
// CHECK-NEXT: - (void)unavailableOnMacOSInstanceMethodRenamedToClassMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSInstanceMethodRenamedToClassMethod' has been renamed to 'classMethodWithACustomObjCName(x:)': This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here");
// CHECK-NEXT: + (void)deprecatedClassMethodRenamedToInstanceMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_DEPRECATED_MSG("This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here", "instanceMethodWithACustomObjCName(x:)");
// CHECK-NEXT: + (void)deprecatedOnMacOSClassMethodRenamedToInstanceMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSClassMethodRenamedToInstanceMethod' has been renamed to 'instanceMethodWithACustomObjCName(x:)': This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here");
// CHECK-NEXT: + (void)unavailableClassMethodRenamedToInstanceMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableClassMethodRenamedToInstanceMethod' has been renamed to 'instanceMethodWithACustomObjCName(x:)': This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here");
// CHECK-NEXT: + (void)unavailableOnMacOSClassMethodRenamedToInstanceMethodWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSClassMethodRenamedToInstanceMethod' has been renamed to 'instanceMethodWithACustomObjCName(x:)': This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here");

// CHECK-NEXT: - (void)customObjCNameInstanceMethodWithX:(NSInteger)x;
// CHECK-NEXT: + (void)customObjCNameClassMethodWithX:(NSInteger)x;

// CHECK-NEXT: - (void)deprecatedMethodRenamedToMethodNotAvailableToObjC
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "methodNotAvailableToObjC()");
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodRenamedToMethodNotAvailableToObjC
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodRenamedToMethodNotAvailableToObjC' has been renamed to 'methodNotAvailableToObjC()'");
// CHECK-NEXT: - (void)unavailableMethodRenamedToMethodNotAvailableToObjC
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodRenamedToMethodNotAvailableToObjC' has been renamed to 'methodNotAvailableToObjC()'");
// CHECK-NEXT: - (void)unavailableOnMacOSMethodRenamedToMethodNotAvailableToObjC
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodRenamedToMethodNotAvailableToObjC' has been renamed to 'methodNotAvailableToObjC()'");

// CHECK-NEXT: - (void)deprecatedMethodRenamedToSimpleProperty SWIFT_DEPRECATED_MSG("", "simpleProperty");
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodRenamedToSimpleProperty
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodRenamedToSimpleProperty' has been renamed to 'simpleProperty'");
// CHECK-NEXT: - (void)unavailableMethodRenamedToSimpleProperty
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodRenamedToSimpleProperty' has been renamed to 'simpleProperty'");
// CHECK-NEXT: - (void)unavailableOnMacOSMethodRenamedToSimpleProperty
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodRenamedToSimpleProperty' has been renamed to 'simpleProperty'");

// CHECK-NEXT: - (NSInteger)methodReturningInt SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT: - (NSInteger)methodWithoutCustomObjCNameWithValue:(NSInteger)value SWIFT_WARN_UNUSED_RESULT;

// CHECK-NEXT: - (NSInteger)deprecatedMethodRenamedToMethodWithoutCustomObjCNameWithValue:(NSInteger)value SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "methodWithoutCustomObjCNameWithValue:");
// CHECK-NEXT: - (NSInteger)deprecatedOnMacOSMethodRenamedToMethodWithoutCustomObjCNameWithValue:(NSInteger)value SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodRenamedToMethodWithoutCustomObjCName' has been renamed to 'methodWithoutCustomObjCNameWithValue:'");
// CHECK-NEXT: - (NSInteger)unavailableMethodRenamedToMethodWithoutCustomObjCNameWithValue:(NSInteger)value SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodRenamedToMethodWithoutCustomObjCName' has been renamed to 'methodWithoutCustomObjCNameWithValue:'");
// CHECK-NEXT: - (NSInteger)unavailableOnMacOSMethodRenamedToMethodWithoutCustomObjCNameWithValue:(NSInteger)value SWIFT_WARN_UNUSED_RESULT
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodRenamedToMethodWithoutCustomObjCName' has been renamed to 'methodWithoutCustomObjCNameWithValue:'");

// CHECK-NEXT: + (void)unavailableAvailabilityWithValue:(NSInteger)value;
// CHECK-NEXT: + (void)makeDeprecatedAvailabilityWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_DEPRECATED_MSG("use something else", "deprecatedAvailabilityWithValue:");
// CHECK-NEXT: + (void)makeDeprecatedOnMacOSAvailabilityWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'__makeDeprecatedOnMacOSAvailability' has been renamed to 'deprecatedAvailabilityWithValue:': use something else");
// CHECK-NEXT: + (void)makeUnavailableAvailabilityWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'__makeUnavailableAvailability' has been renamed to 'unavailableAvailabilityWithValue:': use something else");
// CHECK-NEXT: + (void)makeUnavailableOnMacOSAvailabilityWithValue:(NSInteger)value
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'__makeUnavailableOnMacOSAvailability' has been renamed to 'unavailableAvailabilityWithValue:': use something else");

// CHECK-NEXT: - (nonnull instancetype)init OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithX:(NSInteger)x OBJC_DESIGNATED_INITIALIZER SWIFT_AVAILABILITY(macos,introduced=10.10);
// CHECK-NEXT: - (nonnull instancetype)initWithFirst:(NSInteger)first second:(NSInteger)second OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithDeprecatedFirst:(NSInteger)first second:(NSInteger)second OBJC_DESIGNATED_INITIALIZER
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "initWithFirst:second:");
// CHECK-NEXT: - (nonnull instancetype)initWithDeprecatedOnMacOSFirst:(NSInteger)first second:(NSInteger)second OBJC_DESIGNATED_INITIALIZER
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'init' has been renamed to 'initWithFirst:second:'");
// CHECK-NEXT: - (nonnull instancetype)initWithUnavailableFirst:(NSInteger)first second:(NSInteger)second OBJC_DESIGNATED_INITIALIZER
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'init' has been renamed to 'initWithFirst:second:'");
// CHECK-NEXT: - (nonnull instancetype)initWithUnavailableOnMacOSFirst:(NSInteger)first second:(NSInteger)second OBJC_DESIGNATED_INITIALIZER
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'init' has been renamed to 'initWithFirst:second:'");

// CHECK-NEXT: @property (nonatomic, readonly) NSInteger simpleProperty;
// CHECK-NEXT: @property (nonatomic) NSInteger alwaysUnavailableProperty SWIFT_UNAVAILABLE_MSG("'alwaysUnavailableProperty' has been renamed to 'baz': whatever");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger alwaysDeprecatedProperty SWIFT_DEPRECATED_MSG("use something else", "quux");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger replaceForDeprecatedObjCProperty;
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger numberOfReplaceableDeprecatedObjCProperty
// CHECK-SAME: SWIFT_DEPRECATED_MSG("use something else", "replaceForDeprecatedObjCProperty");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger numberOfReplaceableDeprecatedOnMacOSObjCProperty
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'replaceableDeprecatedOnMacOSObjCProperty' has been renamed to 'replaceForDeprecatedObjCProperty': use something else");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger replaceForUnavailableObjCProperty;
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger numberOfReplaceableUnavailableObjCProperty
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'replaceableUnavailableObjCProperty' has been renamed to 'replaceForUnavailableObjCProperty': use something else");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger numberOfReplaceableUnavailableOnMacOSObjCProperty
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'replaceableUnavailableOnMacOSObjCProperty' has been renamed to 'replaceForUnavailableObjCProperty': use something else");

// CHECK-NEXT: @property (nonatomic, readonly, strong) Availability * _Null_unspecified singlePlatCombinedPropertyClass
// CHECK-SAME: SWIFT_AVAILABILITY(macos,introduced=10.7,deprecated=10.9,obsoleted=10.10);
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger platformUnavailableRenameWithMessageProperty
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'platformUnavailableRenameWithMessageProperty' has been renamed to 'anotherPlea': still trapped");

// CHECK-NEXT: @property (nonatomic, readonly) NSInteger deprecatedPropertyRenamedToMethod
// CHECK-SAME: SWIFT_DEPRECATED_MSG("", "simpleMethodReturningInt()");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger deprecatedOnMacOSPropertyRenamedToMethod
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSPropertyRenamedToMethod' has been renamed to 'simpleMethodReturningInt()'");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger unavailablePropertyRenamedToMethod
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailablePropertyRenamedToMethod' has been renamed to 'simpleMethodReturningInt()'");
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger unavailableOnMacOSPropertyRenamedToMethod
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSPropertyRenamedToMethod' has been renamed to 'simpleMethodReturningInt()'");
// CHECK-NEXT: @end

// CHECK-LABEL: {{^}}SWIFT_AVAILABILITY(macos,introduced=999){{$}}
// CHECK-NEXT: @interface Availability (SWIFT_EXTENSION(availability))
// CHECK-NEXT: - (void)extensionAvailability:(WholeClassAvailability * _Nonnull)_;
// CHECK-NEXT: @property (nonatomic, readonly) NSInteger propertyDeprecatedInsideExtension SWIFT_AVAILABILITY(macos,deprecated=10.10);
// CHECK-NEXT: @end

// CHECK-LABEL: @interface AvailabilitySub
// CHECK-NEXT: - (nonnull instancetype)init SWIFT_UNAVAILABLE;
// CHECK-NEXT: + (nonnull instancetype)new SWIFT_DEPRECATED_MSG("-init is unavailable");
// CHECK-NEXT: - (nonnull instancetype)initWithX:(NSInteger)x SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initWithDeprecatedZ:(NSInteger)deprecatedZ OBJC_DESIGNATED_INITIALIZER SWIFT_DEPRECATED_MSG("init(deprecatedZ:) was deprecated. Use the new one instead", "initWithNewZ:")
// CHECK-NEXT: - (nonnull instancetype)initWithNewZ:(NSInteger)z OBJC_DESIGNATED_INITIALIZER;
// CHECK-NEXT: - (nonnull instancetype)initWithFirst:(NSInteger)first second:(NSInteger)second SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initWithDeprecatedFirst:(NSInteger)first second:(NSInteger)second SWIFT_UNAVAILABLE;
// CHECK-NEXT: - (nonnull instancetype)initWithDeprecatedOnMacOSFirst:(NSInteger)first second:(NSInteger)second SWIFT_UNAVAILABLE;
// CHECK: @end

// CHECK-LABEL: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'DeprecatedAvailability' has been renamed to 'SWTReplacementAvailable'")
// CHECK-LABEL: @interface DeprecatedAvailability
// CHECK-NEXT: - (void)deprecatedMethodInDeprecatedClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_DEPRECATED_MSG("use method in another class instead", "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodInDeprecatedClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodInDeprecatedClassWithPrimitiveParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)': use method in another class instead");

// CHECK-NEXT: - (void)deprecatedMethodInDeprecatedClassWithClassObjectParametersWithFirst:(SWTReplacementAvailable * _Nonnull)first second:(SWTReplacementAvailable * _Nonnull)second
// CHECK-SAME: SWIFT_DEPRECATED_MSG("use method in another class instead", "ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)")
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodInDeprecatedClassWithClassObjectParametersWithFirst:(SWTReplacementAvailable * _Nonnull)first second:(SWTReplacementAvailable * _Nonnull)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodInDeprecatedClassWithClassObjectParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)': use method in another class instead");

// CHECK-NEXT: @end

// CHECK-LABEL: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'DeprecatedAvailabilityProtocol' has been renamed to 'SWTReplacementAvailableProtocol'")
// CHECK-LABEL: @protocol DeprecatedAvailabilityProtocol
// CHECK-NEXT: - (void)deprecatedMethodInDeprecatedClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_DEPRECATED_MSG("use method in another class instead", "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
// CHECK-NEXT: - (void)deprecatedOnMacOSMethodInDeprecatedClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,deprecated=0.0.1,message="'deprecatedOnMacOSMethodInDeprecatedClassWithPrimitiveParameters' has been renamed to 'ReplacementAvailableProtocol.methodReplacingInReplacementProtocol(first:second:)': use method in another class instead");
// CHECK-NEXT: @end

// CHECK-LABEL: @interface SWTReplacementAvailable
// CHECK-NEXT: - (void)replacingMethodInReplacementClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second;
// CHECK-NEXT: - (void)replacingMethodInReplacementClassWithClassObjectParametersWithFirst:(SWTReplacementAvailable * _Nonnull)first second:(SWTReplacementAvailable * _Nonnull)second;
// CHECK-NEXT: - (void)deprecatedMethodReplacingInReplacementClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_DEPRECATED_MSG("Deprecated method with the Context name in the renamed attribute - ContextName is self",
// CHECK-SAME: "replacingMethodInReplacementClassWithPrimitiveParametersWithFirst:second:")
// CHECK-NEXT: - (void)deprecatedmethodReplacingInReplacementClassWithClassObjectParametersWithFirst:(SWTReplacementAvailable * _Nonnull)first second:(SWTReplacementAvailable * _Nonnull)second
// CHECK-SAME: SWIFT_DEPRECATED_MSG("Deprecated method with the Context name in the renamed attribute - ContextName is self",
// CHECK-SAME: "replacingMethodInReplacementClassWithClassObjectParametersWithFirst:second:")
// CHECK-NEXT: @end

// CHECK-LABEL: @protocol SWTReplacementAvailableProtocol
// CHECK-NEXT: - (void)replacingMethodInReplacementProtocolWithFirst:(NSInteger)first second:(NSInteger)second;
// CHECK-NEXT: @end


// CHECK-LABEL: SWIFT_AVAILABILITY(macos,unavailable,message="'UnavailableAvailability' has been renamed to 'SWTReplacementAvailable'")
// CHECK-LABEL: @interface UnavailableAvailability
// CHECK-NEXT: - (void)unavailableMethodInUnavailableClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodInUnavailableClassWithPrimitiveParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)': use method in another class instead")
// CHECK-NEXT: - (void)unavailableOnMacOSMethodInUnavailableClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodInUnavailableClassWithPrimitiveParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)': use method in another class instead");

// CHECK-NEXT: - (void)unavailableMethodInUnavailableClassWithClassObjectParametersWithFirst:(SWTReplacementAvailable * _Nonnull)first second:(SWTReplacementAvailable * _Nonnull)second
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodInUnavailableClassWithClassObjectParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)': use method in another class instead")
// CHECK-NEXT: - (void)unavailableOnMacOSMethodInUnavailableClassWithClassObjectParametersWithFirst:(SWTReplacementAvailable * _Nonnull)first second:(SWTReplacementAvailable * _Nonnull)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodInUnavailableClassWithClassObjectParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)': use method in another class instead");
// CHECK-NEXT: @end

// CHECK-LABEL: SWIFT_AVAILABILITY(macos,unavailable,message="'UnavailableAvailabilityProtocol' has been renamed to 'SWTReplacementAvailableProtocol'")
// CHECK-LABEL: @protocol UnavailableAvailabilityProtocol
// CHECK-NEXT: - (void)unavailableMethodInUnavailableClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_UNAVAILABLE_MSG("'unavailableMethodInUnavailableClassWithPrimitiveParameters' has been renamed to 'ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)': use method in another class instead")
// CHECK-NEXT: - (void)unavailableOnMacOSMethodInUnavailableClassWithPrimitiveParametersWithFirst:(NSInteger)first second:(NSInteger)second
// CHECK-SAME: SWIFT_AVAILABILITY(macos,unavailable,message="'unavailableOnMacOSMethodInUnavailableClassWithPrimitiveParameters' has been renamed to 'ReplacementAvailableProtocol.methodReplacingInReplacementProtocol(first:second:)': use method in another class instead");
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

    @available(*, deprecated, message: "one\ntwo\tthree\rfour\\ \"five\"\0six")
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
  
    @objc func overloadMethod(first: Int, second: Int) {}
    func overloadMethod(first: Double, second: Double) {}

    @available(*, deprecated, renamed: "overloadMethod(first:second:)")
    @objc func deprecatedMethodRenamedToOverloadMethod(first: Int, second: Int) {}

    @available(macOS, deprecated, renamed: "overloadMethod(first:second:)")
    @objc func deprecatedOnMacOSMethodRenamedToOverloadMethod(first: Int, second: Int) {}

    @available(*, unavailable, renamed: "overloadMethod(first:second:)")
    @objc func unavailableMethodRenamedToOverloadMethod(first: Int, second: Int) {}

    @available(macOS, unavailable, renamed: "overloadMethod(first:second:)")
    @objc func unavailableOnMacOSMethodRenamedToOverloadMethod(first: Int, second: Int) {}


    @objc(firstOverloadingMethodWithDifferenceNameWithFirst:second:)
    func overloadMethodWithDifferenceObjCName(first: Int, second: Int) {}
    @objc(secondOverloadingMethodWithDifferenceNameWithFirst:second:)
    func overloadMethodWithDifferenceObjCName(first: Double, second: Double) {}

    @available(*, deprecated, renamed: "overloadMethodWithDifferenceObjCName(first:second:)")
    @objc func deprecatedMethodRenamedToOverloadMethodWithDifferenceName(first: Int, second: Int) {}
    @available(macOS, deprecated, renamed: "overloadMethodWithDifferenceObjCName(first:second:)")
    @objc func deprecatedOnMacOSMethodRenamedToOverloadMethodWithDifferenceName(first: Int, second: Int) {}

    @available(*, unavailable, renamed: "overloadMethodWithDifferenceObjCName(first:second:)")
    @objc func unavailableMethodRenamedToOverloadMethodWithDifferenceName(first: Int, second: Int) {}
    @available(macOS, unavailable, renamed: "overloadMethodWithDifferenceObjCName(first:second:)")
    @objc func unavailableOnMacOSMethodRenamedToOverloadMethodWithDifferenceName(first: Int, second: Int) {}

    @objc(deprecatedAvailabilityWithValue:)
    public class func makeDeprecatedAvailability(withValue value: Int) {}

    @available(*, deprecated, message: "This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here", renamed: "classMethodWithACustomObjCName(x:)")
    @objc public func deprecatedInstanceMethodRenamedToClassMethod(value: Int) {}
    @available(macOS, deprecated, message: "This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here", renamed: "classMethodWithACustomObjCName(x:)")
    @objc public func deprecatedOnMacOSInstanceMethodRenamedToClassMethod(value: Int) {}
    
    @available(*, unavailable, message: "This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here", renamed: "classMethodWithACustomObjCName(x:)")
    @objc public func unavailableInstanceMethodRenamedToClassMethod(value: Int) {}
    @available(macOS, unavailable, message: "This method has a renamed attribute point to class method instead of a instance method. It should show the Swift name here", renamed: "classMethodWithACustomObjCName(x:)")
    @objc public func unavailableOnMacOSInstanceMethodRenamedToClassMethod(value: Int) {}
        
    @available(*, deprecated, message: "This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here", renamed: "instanceMethodWithACustomObjCName(x:)")
    @objc public class func deprecatedClassMethodRenamedToInstanceMethod(value: Int) {}
    @available(macOS, deprecated, message: "This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here", renamed: "instanceMethodWithACustomObjCName(x:)")
    @objc public class func deprecatedOnMacOSClassMethodRenamedToInstanceMethod(value: Int) {}

    @available(*, unavailable, message: "This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here", renamed: "instanceMethodWithACustomObjCName(x:)")
    @objc public class func unavailableClassMethodRenamedToInstanceMethod(value: Int) {}
    @available(macOS, unavailable, message: "This method has a renamed attribute point to instance method instead of a class method. It should show the Swift name here", renamed: "instanceMethodWithACustomObjCName(x:)")
    @objc public class func unavailableOnMacOSClassMethodRenamedToInstanceMethod(value: Int) {}
    
    @objc(customObjCNameInstanceMethodWithX:)
    public func instanceMethodWithACustomObjCName(x: Int) {}

    @objc(customObjCNameClassMethodWithX:)
    public class func classMethodWithACustomObjCName(x: Int) {}
  
    @nonobjc func methodNotAvailableToObjC() {}
  
    @available(*, deprecated, renamed: "methodNotAvailableToObjC()")
    @objc public func deprecatedMethodRenamedToMethodNotAvailableToObjC() {}
    @available(macOS, deprecated, renamed: "methodNotAvailableToObjC()")
    @objc public func deprecatedOnMacOSMethodRenamedToMethodNotAvailableToObjC() {}

    @available(*, unavailable, renamed: "methodNotAvailableToObjC()")
    @objc public func unavailableMethodRenamedToMethodNotAvailableToObjC() {}
    @available(macOS, unavailable, renamed: "methodNotAvailableToObjC()")
    @objc public func unavailableOnMacOSMethodRenamedToMethodNotAvailableToObjC() {}

    @available(*, deprecated, renamed: "simpleProperty")
    @objc public func deprecatedMethodRenamedToSimpleProperty() {}
    @available(macOS, deprecated, renamed: "simpleProperty")
    @objc public func deprecatedOnMacOSMethodRenamedToSimpleProperty() {}

    @available(*, unavailable, renamed: "simpleProperty")
    @objc public func unavailableMethodRenamedToSimpleProperty() {}
    @available(macOS, unavailable, renamed: "simpleProperty")
    @objc public func unavailableOnMacOSMethodRenamedToSimpleProperty() {}

    @objc(methodReturningInt) public func simpleMethodReturningInt() -> Int { return -1 }

    @objc public func methodWithoutCustomObjCName(value: Int) -> Int { return -1 }

    @available(*, deprecated, renamed: "methodWithoutCustomObjCName(value:)")
    @objc public func deprecatedMethodRenamedToMethodWithoutCustomObjCName(value: Int) -> Int { return -1 }
    @available(macOS, deprecated, renamed: "methodWithoutCustomObjCName(value:)")
    @objc public func deprecatedOnMacOSMethodRenamedToMethodWithoutCustomObjCName(value: Int) -> Int { return -1 }

    @available(*, unavailable, renamed: "methodWithoutCustomObjCName(value:)")
    @objc public func unavailableMethodRenamedToMethodWithoutCustomObjCName(value: Int) -> Int { return -1 }
    @available(macOS, unavailable, renamed: "methodWithoutCustomObjCName(value:)")
    @objc public func unavailableOnMacOSMethodRenamedToMethodWithoutCustomObjCName(value: Int) -> Int { return -1 }


    @objc(unavailableAvailabilityWithValue:)
    public class func makeUnavailableAvailability(withValue value: Int) {}

    @available(*, deprecated,
    message: "use something else",
    renamed: "makeDeprecatedAvailability(withValue:)")
    @objc(makeDeprecatedAvailabilityWithValue:) public class func __makeDeprecatedAvailability(withValue value: Int) {}
    @available(macOS, deprecated,
    message: "use something else",
    renamed: "makeDeprecatedAvailability(withValue:)")
    @objc(makeDeprecatedOnMacOSAvailabilityWithValue:) public class func __makeDeprecatedOnMacOSAvailability(withValue value: Int) {}

    @available(*, unavailable,
    message: "use something else",
    renamed: "makeUnavailableAvailability(withValue:)")
    @objc(makeUnavailableAvailabilityWithValue:) public class func __makeUnavailableAvailability(withValue value: Int) {}
    @available(macOS, unavailable,
    message: "use something else",
    renamed: "makeUnavailableAvailability(withValue:)")
    @objc(makeUnavailableOnMacOSAvailabilityWithValue:) public class func __makeUnavailableOnMacOSAvailability(withValue value: Int) {}


    @objc init() {}
    @available(macOS 10.10, *)
    @objc init(x: Int) {}
    
    @objc init(first: Int, second: Int) {}
    init(first: Double, second: Double) {}
    
    @available(*, deprecated, renamed: "init(first:second:)")
    @objc init(deprecatedFirst first: Int, second: Int) {}
    
    @available(macOS, deprecated, renamed: "init(first:second:)")
    @objc init(deprecatedOnMacOSFirst first: Int, second: Int) {}
    
    @available(*, unavailable, renamed: "init(first:second:)")
    @objc init(unavailableFirst first: Int, second: Int) {}
    
    @available(macOS, unavailable, renamed: "init(first:second:)")
    @objc init(unavailableOnMacOSFirst first: Int, second: Int) {}
    
    @objc var simpleProperty: Int {
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

    @objc(replaceForDeprecatedObjCProperty) var __replaceForDeprecatedObjCProperty: Int {
        get {
            return -1
        }
    }
    @available(*, deprecated, message: "use something else", renamed: "__replaceForDeprecatedObjCProperty")
    @objc(numberOfReplaceableDeprecatedObjCProperty) var replaceableDeprecatedObjCProperty: Int {
        get {
            return -1
        }
    }
    @available(macOS, deprecated, message: "use something else", renamed: "__replaceForDeprecatedObjCProperty")
    @objc(numberOfReplaceableDeprecatedOnMacOSObjCProperty) var replaceableDeprecatedOnMacOSObjCProperty: Int {
        get {
            return -1
        }
    }

  
    @objc(replaceForUnavailableObjCProperty) var __replaceForUnavailableObjCProperty: Int {
      get {
        return -1
      }
    }
    @available(*, unavailable, message: "use something else", renamed: "__replaceForUnavailableObjCProperty")
    @objc(numberOfReplaceableUnavailableObjCProperty) var replaceableUnavailableObjCProperty: Int {
      get {
        return -1
      }
    }
    @available(macOS, unavailable, message: "use something else", renamed: "__replaceForUnavailableObjCProperty")
    @objc(numberOfReplaceableUnavailableOnMacOSObjCProperty) var replaceableUnavailableOnMacOSObjCProperty: Int {
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
    
    @available(*, deprecated, renamed: "simpleMethodReturningInt()")
    @objc var deprecatedPropertyRenamedToMethod: Int {
        get {
            return -1
        }
    }
    @available(macOS, deprecated, renamed: "simpleMethodReturningInt()")
    @objc var deprecatedOnMacOSPropertyRenamedToMethod: Int {
        get {
            return -1
        }
    }

    @available(*, unavailable, renamed: "simpleMethodReturningInt()")
    @objc var unavailablePropertyRenamedToMethod: Int {
        get {
            return -1
        }
    }
    @available(macOS, unavailable, renamed: "simpleMethodReturningInt()")
    @objc var unavailableOnMacOSPropertyRenamedToMethod: Int {
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
    @objc var propertyDeprecatedInsideExtension: Int {
        get {
            return 0
        }
    }
}

@objc class AvailabilitySub: Availability {
    private override init() { super.init() }
    @available(macOS 10.10, *)
    private override init(x: Int) { super.init() }
    @available(*, deprecated, message: "init(deprecatedZ:) was deprecated. Use the new one instead", renamed: "init(z:)")
    @objc init(deprecatedZ: Int) { super.init() }
    @objc(initWithNewZ:) init(z: Int) { super.init() }
}


@available(macOS 999, *)
@objc @objcMembers class WholeClassAvailability {
    func wholeClassAvailability(_: WholeProtoAvailability) {}
}

@available(macOS 999, *)
@objc protocol WholeProtoAvailability {
    func wholeProtoAvailability(_: WholeClassAvailability)
}

@objc(SWTReplacementAvailable) class ReplacementAvailable {
    @objc(replacingMethodInReplacementClassWithPrimitiveParametersWithFirst:second:)
    func methodReplacingInReplacementClassWithPrimitiveParameters(first: Int, second: Int) -> Void {}
    
    @objc(replacingMethodInReplacementClassWithClassObjectParametersWithFirst:second:)
    func methodReplacingInReplacementClassWithClassObjectParameters(first: ReplacementAvailable, second: ReplacementAvailable) -> Void {}
    
    @available(*, deprecated,
    message: "Deprecated method with the Context name in the renamed attribute - ContextName is self",
    renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func deprecatedMethodReplacingInReplacementClassWithPrimitiveParameters(first: Int, second: Int) -> Void {}
    
    @available(*, deprecated,
    message: "Deprecated method with the Context name in the renamed attribute - ContextName is self",
    renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)")
    @objc func deprecatedmethodReplacingInReplacementClassWithClassObjectParameters(first: ReplacementAvailable, second: ReplacementAvailable) -> Void {}
}

@available(macOS, deprecated, renamed: "ReplacementAvailable")
@objc class DeprecatedAvailability {
    @available(*, deprecated, message: "use method in another class instead",
    renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func deprecatedMethodInDeprecatedClassWithPrimitiveParameters(first: Int, second: Int) -> Void {}
    @available(macOS, deprecated, message: "use method in another class instead",
    renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func deprecatedOnMacOSMethodInDeprecatedClassWithPrimitiveParameters(first: Int, second: Int) -> Void {}
  
    @available(*, deprecated, message: "use method in another class instead",
    renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)")
    @objc func deprecatedMethodInDeprecatedClassWithClassObjectParameters(first: ReplacementAvailable, second: ReplacementAvailable) -> Void {}
    @available(macOS, deprecated, message: "use method in another class instead",
    renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)")
    @objc func deprecatedOnMacOSMethodInDeprecatedClassWithClassObjectParameters(first: ReplacementAvailable, second: ReplacementAvailable) -> Void {}
}

@available(macOS, unavailable, renamed: "ReplacementAvailable")
@objc class UnavailableAvailability {
    @available(*, unavailable, message: "use method in another class instead", renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func unavailableMethodInUnavailableClassWithPrimitiveParameters(first: Int, second: Int) -> Void {}
    @available(macOS, unavailable, message: "use method in another class instead", renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func unavailableOnMacOSMethodInUnavailableClassWithPrimitiveParameters(first: Int, second: Int) -> Void {}
  
    @available(*, unavailable, message: "use method in another class instead", renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)")
    @objc func unavailableMethodInUnavailableClassWithClassObjectParameters(first: ReplacementAvailable, second: ReplacementAvailable) -> Void {}
    @available(macOS, unavailable, message: "use method in another class instead", renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithClassObjectParameters(first:second:)")
    @objc func unavailableOnMacOSMethodInUnavailableClassWithClassObjectParameters(first: ReplacementAvailable, second: ReplacementAvailable) -> Void {}
}

@objc(SWTReplacementAvailableProtocol) protocol ReplacementAvailableProtocol {
    @objc(replacingMethodInReplacementProtocolWithFirst:second:)
    func methodReplacingInReplacementProtocol(first: Int, second: Int) -> Void
}

@available(macOS, deprecated, renamed: "ReplacementAvailableProtocol")
@objc protocol DeprecatedAvailabilityProtocol {
    @available(*, deprecated, message: "use method in another class instead", renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func deprecatedMethodInDeprecatedClassWithPrimitiveParameters(first: Int, second: Int) -> Void
    @available(macOS, deprecated, message: "use method in another class instead", renamed: "ReplacementAvailableProtocol.methodReplacingInReplacementProtocol(first:second:)")
    @objc func deprecatedOnMacOSMethodInDeprecatedClassWithPrimitiveParameters(first: Int, second: Int) -> Void
}

@available(macOS, unavailable, renamed: "ReplacementAvailableProtocol")
@objc protocol UnavailableAvailabilityProtocol {
    @available(*, unavailable, message: "use method in another class instead", renamed: "ReplacementAvailable.methodReplacingInReplacementClassWithPrimitiveParameters(first:second:)")
    @objc func unavailableMethodInUnavailableClassWithPrimitiveParameters(first: Int, second: Int) -> Void
    @available(macOS, unavailable, message: "use method in another class instead", renamed: "ReplacementAvailableProtocol.methodReplacingInReplacementProtocol(first:second:)")
    @objc func unavailableOnMacOSMethodInUnavailableClassWithPrimitiveParameters(first: Int, second: Int) -> Void
}


