// Check that a failed type lookup isn't logged when the type is a construct the
// runtime doesn't support, and is logged when it should have succeeded.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: env %env-SWIFT_DEBUG_FAILED_TYPE_LOOKUP=1 %target-run %t/main 2>&1 | %FileCheck %s

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Android)
import Android
#elseif os(WASI)
import WASILibc
#elseif os(Windows)
import CRT
#endif

@_silgen_name("swift_getTypeByMangledNameInContext")
func swift_getTypeByMangledNameInContext(
    _ name: UnsafePointer<CChar>,
    _ length: UInt,
    _ context: UnsafeRawPointer?,
    _ args: UnsafePointer<UnsafeRawPointer?>?
) -> UnsafeRawPointer?

func lookup(_ name: String) {
  fputs("lookup \(name)\n", stderr)
  let result = name.withCString {
    swift_getTypeByMangledNameInContext($0, UInt(strlen($0)), nil, nil)
  }
  fputs("result \(result == nil ? "nil" : "non-nil")\n", stderr)
}

// CHECK-LABEL: lookup Siz_Xx
// CHECK-NEXT: result nil
lookup("Siz_Xx")

// CHECK-LABEL: lookup SiSgXwz_Xx
// CHECK-NEXT: result nil
lookup("SiSgXwz_Xx")

// CHECK-LABEL: lookup SiXb
// CHECK-NEXT: result nil
lookup("SiXb")

// CHECK-LABEL: lookup SiIegl_
// CHECK-NEXT: result nil
lookup("SiIegl_")

// CHECK-LABEL: lookup SiXSq
// CHECK-NEXT: result nil
lookup("SiXSq")

// CHECK-LABEL: lookup 4main12DoesNotExistV
// CHECK-NEXT: failed type lookup for main.DoesNotExist:
// CHECK-NEXT: result nil
lookup("4main12DoesNotExistV")

// CHECK-LABEL: lookup 4main12DoesNotExistVz_Xx
// CHECK-NEXT: failed type lookup for { var main.DoesNotExist }:
// CHECK-NEXT: result nil
lookup("4main12DoesNotExistVz_Xx")
