// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -import-objc-header %S/Inputs/c_implementation.h -swift-version 6 %s -target %target-stable-abi-triple > %t
// RUN: %FileCheck --input-file %t %s

// REQUIRES: objc_interop

import Foundation

@_silgen_name("getString")
func getString() -> String

@c @implementation
func returns_retained() -> CFString? {
  return getString() as CFString
}

// CHECK-LABEL: sil{{.*}} @$s16c_implementation16returns_retainedSo11CFStringRefaSgyFTo : $@convention(c) () -> @owned Optional<CFString> {

@c @implementation
func returns_not_retained() -> CFString? {
  return getString() as CFString
}

// CHECK-LABEL: sil{{.*}} @$s16c_implementation20returns_not_retainedSo11CFStringRefaSgyFTo : $@convention(c) () -> @autoreleased Optional<CFString> {

@c @implementation
func passes_borrowed(_ string: CFString?) {

}

// CHECK-LABEL: sil{{.*}} @$s16c_implementation15passes_borrowedyySo11CFStringRefaSgFTo : $@convention(c) (Optional<CFString>) -> () {

@c @implementation
func passes_consumed(_ string: consuming CFString?) {

}

// CHECK-LABEL: sil{{.*}} @$s16c_implementation15passes_consumedyySo11CFStringRefaSgnFTo : $@convention(c) (@owned Optional<CFString>) -> () {

