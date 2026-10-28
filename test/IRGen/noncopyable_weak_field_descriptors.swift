// RUN: %empty-directory(%t)
// RUN: %swift -emit-ir -o - %s -module-name test -target %target-cpu-apple-macosx14 > %t/test_old.irgen
// RUN: %swift -emit-ir -o - %s -module-name test -target %target-cpu-apple-macosx15 > %t/test_new.irgen
// RUN: %FileCheck --check-prefix=OLD %s < %t/test_old.irgen
// RUN: %FileCheck --check-prefix=NEW %s < %t/test_new.irgen

// A weak/unowned reference to a generic class with a ~Copyable constraint
// used to crash IRGen (visitReferenceStorageType abort in MetadataRequest.cpp)
// when the deployment target's runtime is too old to demangle the class's
// inverse-requirement generic signature directly, because the
// getTypeRefByFunction fallback tried to compute type metadata for the
// weak-storage type itself, which does not exist, only the referent has
// metadata.
//
// rdar://186251439
// REQUIRES: OS=macosx
// arm64e ptrauth-signs the accessor's function pointer, changing the IR below.
// UNSUPPORTED: CPU=arm64e

public final class Foo<T: ~Copyable> {
  public struct Bar: ~Copyable {
    // The getTypeRefByFunction fallback:
    // OLD: @"$s4test3FooCAARi_zrlE3BarVMF" =
    // OLD-SAME: @"get_type_metadata{{.*}}Foo{{.*}}Xw{{.*}}"
    // NEW: @"$s4test3FooCAARi_zrlE3BarVMF" =
    // The plain mangled name:
    // NEW-SAME: @"symbolic{{.*}}Xw{{.*}}Foo{{.*}}"
    weak var foo: Foo<T>?
  }
}

// The getTypeRefByFunction fallback used on old deployment targets hides the
// field as an empty tuple, matching how unreflectable noncopyable fields are
// hidden from old Mirror.
// OLD: define private ptr @"get_type_metadata{{.*}}Xw"(ptr %0)
// OLD-NEXT: entry:
// OLD-NEXT: ret ptr getelementptr inbounds (%swift.full_existential_type, ptr @"$sytN", i32 0, i32 1)
