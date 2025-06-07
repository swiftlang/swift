// Test that C++ access specifiers are honored.

// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -enable-experimental-cxx-interop -enable-experimental-feature ImportNonPublicCxxMembers
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

import AccessSpecifiers

var v = PublicPrivate()

// Can access all public members and types.

v.PublicMemberVar = 1
PublicPrivate.PublicStaticMemberVar = 1
v.publicMemberFunc()

var publicTypedefVar: PublicPrivate.PublicTypedef
var publicStructVar: PublicPrivate.PublicStruct
var publicEnumVar: PublicPrivate.PublicEnum
var publicClosedEnumVar: PublicPrivate.PublicClosedEnum
var publicOpenEnumVar: PublicPrivate.PublicOpenEnum
var publicFlagEnumVar: PublicPrivate.PublicFlagEnum

// TODO: nested enum members aren't being imported correctly yet (#54905)
// Once they are, verify that they are accessible.
// print(PublicPrivate.PublicEnumValue1)
// print(PublicPrivate.PublicAnonymousEnumValue)
print(PublicPrivate.PublicClosedEnum.value1)
print(PublicPrivate.PublicOpenEnum.value1)

// Cannot access any private members and types.

v.PrivateMemberVar = 1 // expected-error {{'PrivateMemberVar' is inaccessible due to 'private' protection level}}
PublicPrivate.PrivateStaticMemberVar = 1 // expected-error {{'PrivateStaticMemberVar' is inaccessible due to 'private' protection level}}
v.privateMemberFunc() // expected-error {{'privateMemberFunc' is inaccessible due to 'private' protection level}}

var privateTypedefVar: PublicPrivate.PrivateTypedef // expected-error {{'PrivateTypedef' is inaccessible due to 'private' protection level}}
var privateStructVar: PublicPrivate.PrivateStruct // expected-error {{'PrivateStruct' is inaccessible due to 'private' protection level}}
var privateEnumVar: PublicPrivate.PrivateEnum // expected-error {{'PrivateEnum' is inaccessible due to 'private' protection level}}
var privateClosedEnumVar: PublicPrivate.PrivateClosedEnum // expected-error {{'PrivateClosedEnum' is inaccessible due to 'private' protection level}}
var privateOpenEnumVar: PublicPrivate.PrivateOpenEnum // expected-error {{'PrivateOpenEnum' is inaccessible due to 'private' protection level}}
var privateFlagEnumVar: PublicPrivate.PrivateFlagEnum // expected-error {{'PrivateFlagEnum' is inaccessible due to 'private' protection level}}

// TODO: nested enum members aren't being imported correctly yet (#54905)
// Once they are, verify that this throws an error (similar to above).
// print(PublicPrivate.PrivateEnumValue1)
// print(PublicPrivate.PrivateAnonymousEnumValue1)
print(PublicPrivate.PrivateOpenEnum.value1) // expected-error {{'PrivateOpenEnum' is inaccessible due to 'private' protection level}}
print(PublicPrivate.PrivateClosedEnum.value1) // expected-error {{'PrivateClosedEnum' is inaccessible due to 'private' protection level}}
