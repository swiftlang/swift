// RUN: %empty-directory(%t)
// RUN: %swift-frontend %s -target %target-cpu-apple-macosx15 -module-name main -emit-ir -o %t/new.ir
// RUN: %swift-frontend %s -target %target-cpu-apple-macosx14 -module-name main -emit-ir -o %t/old.ir

// RUN: %FileCheck %s --check-prefix=NEW < %t/new.ir
// RUN: %FileCheck %s --check-prefix=OLD < %t/old.ir

// Check that we add extra type metadata accessors for types with generic
// parameters that have an inverse. These are used instead of using demangling
// cache variables since old runtimes cannot synthesize type metadata based on
// the new mangling.

// RUN: %target-build-swift %s -o %t/test_mangling
// RUN: %target-run %t/test_mangling | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: executable_test


// This type's generic parameter is noncopyable, so older runtimes can't
// demangle the type's name to build the metadata.
struct Foo<T: ~Copyable>: ~Copyable {
  mutating func bar(_ i: Int) { print("Foo.bar(\(i))") }
}

func test() {
  var foo = Foo<Int>()
  foo.bar(1)
}
test()
// CHECK: Foo.bar(1)

// NEW: define hidden swiftcc void @"$s4main4testyyF"()
// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s4main3FooVySiGMD")
// NEW: }

// OLD: define hidden swiftcc void @"$s4main4testyyF"()
// OLD: call swiftcc %swift.metadata_response @"$s4main3FooVySiGMa"(i64 0)
// OLD: }

struct NC: ~Copyable {}

// NEW: define hidden swiftcc void @"$s4main10testWithNCyyF"()
// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s4main3FooVyAA2NCVGMD")
// NEW: }

// OLD: define hidden swiftcc void @"$s4main10testWithNCyyF"()
// OLD: call swiftcc %swift.metadata_response @"$s4main3FooVyAA2NCVGMa"
// OLD: }
func testWithNC() {
  var foo = Foo<NC>()
  foo.bar(2)
}
testWithNC()
// CHECK: Foo.bar(2)


// NEW: define hidden swiftcc void @"$s4main17testWithNCGenericyyxnRi_zlF"
// NEW: call swiftcc %swift.metadata_response @"$s4main3FooVMa"
// NEW: }

// OLD: define hidden swiftcc void @"$s4main17testWithNCGenericyyxnRi_zlF"
// OLD: call swiftcc %swift.metadata_response @"$s4main3FooVMa"
// OLD: }
func testWithNCGeneric<T: ~Copyable>(_ t: consuming T) {
  var foo = Foo<T>()
  foo.bar(3)
}
testWithNCGeneric(Foo<NC>())
// CHECK: Foo.bar(3)


// This type does not need a Swift 6.0 runtime, despite being noncopyable,
// because it doesn't have a noncopyable generic parameter.
struct JustNoncopyable<T>: ~Copyable {
  mutating func bar() { print("JustNoncopyable.bar") }
}

func testNonGeneric() {
    var ng = JustNoncopyable<Int>()
    ng.bar()
}
testNonGeneric()
// CHECK: JustNoncopyable.bar

// NEW: define hidden swiftcc void @"$s4main14testNonGenericyyF"()
// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s4main15JustNoncopyableVySiGMD")
// NEW: }

// OLD: define hidden swiftcc void @"$s4main14testNonGenericyyF"()
// OLD: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s4main15JustNoncopyableVySiGMD")
// OLD: }


/// Check that Optional still uses `__swift_instantiateConcreteTypeFromMangledName`
/// even when calling a method available to a noncopyable Optional.
extension Optional where Wrapped: ~Copyable {
  mutating func bar(_ i: Int) { print("Optional.bar(\(i))") }
}

// NEW: define hidden swiftcc void @"$s4main20testCopyableOptionalyyF"()
// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sSiSgMD")
// NEW: }

// OLD: define hidden swiftcc void @"$s4main20testCopyableOptionalyyF"()
// OLD: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sSiSgMD")
// OLD: }
func testCopyableOptional() {
    var opt = Optional<Int>(94103)
    opt.bar(1)
}
testCopyableOptional()
// CHECK: Optional.bar(1)


// NEW: define hidden swiftcc void @"$s4main23testNOTCopyableOptionalyyF"()
// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s4main2NCVSgMD")
// NEW: }

// OLD: define hidden swiftcc void @"$s4main23testNOTCopyableOptionalyyF"()
// OLD: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s4main2NCVSgMD")
// OLD: }
func testNOTCopyableOptional() {
    var opt = Optional<NC>(NC())
    opt.bar(2)
}
testNOTCopyableOptional()
// CHECK: Optional.bar(2)


// NEW: define hidden swiftcc void @"$s4main26check_existential_metatype4withyypRi_s_XPXpSg_tF"
// NEW: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sypRi_s_XPXpSgMD")
// NEW: }

// OLD: define hidden swiftcc void @"$s4main26check_existential_metatype4withyypRi_s_XPXpSg_tF"
// OLD: call swiftcc %swift.metadata_response @"$sypRi_s_XPXpSgMa"
// OLD: }
func check_existential_metatype(with x: (any ~Copyable.Type)?) {
  x.map { print("passed type = \($0)") }
}
check_existential_metatype(with: NC.self)
// CHECK: passed type = NC
