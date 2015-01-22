// RUN: %target-build-swift -parse %s -Xfrontend -verify
// REQUIRES: OS=macosx

import CoreServices

func testFSEventStreamRef(stream: FSEventStreamRef) {
  // FIXME: These should be distinct types, constructible from one another.
  let _: ConstFSEventStreamRef = stream // works by coincidence because both are currently COpaquePointer
  let _ = ConstFSEventStreamRef(stream) // expected-error {{cannot find an initializer for type 'ConstFSEventStreamRef' that accepts an argument list of type '(FSEventStreamRef)'}}

  // This is not a CF object.
  FSEventStreamRetain(stream) // no-warning
  FSEventStreamRelease(stream)

  let _: AnyObject = stream // expected-error {{'FSEventStreamRef' does not conform to protocol 'AnyObject'}}
}
