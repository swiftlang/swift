// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -verify -wmo

// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx || OS=wasip1

@_cdecl("posix_memalign")
func posix_memalign(_ resultPtr:UnsafeMutablePointer<UnsafeMutableRawPointer?>, _ :Int, _ :Int) -> Int32 { // okay: @_extern(c) declaration is separate
  return 0
}
