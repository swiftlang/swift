// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -verify -wmo

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

@_cdecl("posix_memalign")
func posix_memalign(_ resultPtr:UnsafeMutablePointer<UnsafeMutableRawPointer?>, _ :Int, _ :Int) -> Int32 { // expected-error {{function has wrong linkage to be called from}}
  return 0
}
