// RUN: %target-swift-emit-ir -I %S/Inputs -enable-cxx-interop %s | %FileCheck %s

import Reference

public func getCxxRef() -> UnsafeMutablePointer<CInt> {
  return getStaticIntRef()
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i8* @"$s4main9getCxxRefSpys5Int32VGyF"()
// CHECK: call i32* @{{_Z15getStaticIntRefv|"\?getStaticIntRef@@YAAEAHXZ"}}()

public func getConstCxxRef() -> UnsafePointer<CInt> {
  return getConstStaticIntRef()
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i8* @"$s4main14getConstCxxRefSPys5Int32VGyF"()
// CHECK: call i32* @{{_Z20getConstStaticIntRefv|"\?getConstStaticIntRef@@YAAEBHXZ"}}()

public func getCxxRvalueRef() -> UnsafeMutablePointer<CInt> {
  return getStaticIntRvalueRef()
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i8* @"$s4main15getCxxRvalueRefSpys5Int32VGyF"()
// CHECK: call i32* @{{_Z21getStaticIntRvalueRefv|"\?getStaticIntRvalueRef@@YA\$\$QEAHXZ"}}()

public func getConstCxxRvalueRef() -> UnsafePointer<CInt> {
  return getConstStaticIntRvalueRef()
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i8* @"$s4main20getConstCxxRvalueRefSPys5Int32VGyF"()
// CHECK: call i32* @{{_Z26getConstStaticIntRvalueRefv|"\?getConstStaticIntRvalueRef@@YA\$\$QEBHXZ"}}()

public func setCxxRef() {
  var val: CInt = 21
  withUnsafeMutablePointer(to: &val) {
    setStaticIntRef($0)
  }
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main9setCxxRefyyF"()
// CHECK: call void @{{_Z15setStaticIntRefRi|"\?setStaticIntRef@@YAXAEAH@Z"}}(i32* {{nonnull %val|%2}})

public func setCxxConstRef() {
  var val: CInt = 21
  withUnsafePointer(to: &val) {
    setConstStaticIntRef($0)
  }
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main14setCxxConstRefyyF"()
// CHECK: call void @{{_Z20setConstStaticIntRefRKi|"\?setConstStaticIntRef@@YAXAEBH@Z"}}(i32* {{nonnull %val|%2}})

public func setCxxRvalueRef() {
  var val: CInt = 21
  withUnsafeMutablePointer(to: &val) {
    setStaticIntRvalueRef($0)
  }
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main15setCxxRvalueRefyyF"()
// CHECK: call void @{{_Z21setStaticIntRvalueRefOi|"\?setStaticIntRvalueRef@@YAX\$\$QEAH@Z"}}(i32* {{nonnull %val|%2}})

public func setCxxConstRvalueRef() {
  var val: CInt = 21
  withUnsafePointer(to: &val) {
    setConstStaticIntRvalueRef($0)
  }
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main20setCxxConstRvalueRefyyF"()
// CHECK: call void @{{_Z26setConstStaticIntRvalueRefOKi|"\?setConstStaticIntRvalueRef@@YAX\$\$QEBH@Z"}}(i32* {{nonnull %val|%2}})
