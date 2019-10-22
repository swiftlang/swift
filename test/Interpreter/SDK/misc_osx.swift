// RUN: %target-build-swift -typecheck %s -Xfrontend -verify
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import CoreServices

func testFSEventStreamRef(stream: FSEventStreamRef) {
  // FIXME: These should be distinct types, constructible from one another.
  // works by coincidence because both are currently OpaquePointer 
  _ = stream as ConstFSEventStreamRef // expected-warning {{redundant cast from 'FSEventStreamRef' (aka 'OpaquePointer') to 'ConstFSEventStreamRef' (aka 'OpaquePointer') has no effect}} {{14-39=}}
  _ = ConstFSEventStreamRef(stream) // expected-error {{cannot invoke initializer for type 'ConstFSEventStreamRef' with an argument list of type '(FSEventStreamRef)'}}
  // expected-note @-1 {{overloads for 'ConstFSEventStreamRef' exist with these partially matching parameter lists:}}

  // This is not a CF object.
  FSEventStreamRetain(stream) // no-warning
  FSEventStreamRelease(stream)

  let _: AnyObject = stream // expected-error {{value of type 'FSEventStreamRef' (aka 'OpaquePointer') does not conform to specified type 'AnyObject'}}
}
