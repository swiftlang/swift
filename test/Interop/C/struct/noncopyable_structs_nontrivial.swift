// Split out from noncopyable_structs.swift due to an issue with Windows.

// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ -I %swift_src_root/lib/ClangImporter/SwiftBridging %s -verify -DERRORS -verify-additional-prefix conly-
// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/ -I %swift_src_root/lib/ClangImporter/SwiftBridging %s -verify -DERRORS -DCPLUSPLUS -verify-additional-prefix cplusplus- -cxx-interoperability-mode=default

// XFAIL: OS=windows-msvc
import NoncopyableStructs

#if CPLUSPLUS
// expected-cplusplus-warning@+1{{'ExtraDestroy' is deprecated: destroy operation 'extraDestroy' is not allowed on types with a non-trivial destructor}}
func extra(_: borrowing ExtraDestroy) { }
#endif
