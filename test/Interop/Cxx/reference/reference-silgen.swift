// RUN: %target-swift-emit-sil -I %S/Inputs -enable-cxx-interop %s | %FileCheck %s

import Reference

func getCxxRef() -> UnsafeMutablePointer<CInt> {
  return getStaticIntRef()
}

// CHECK: sil hidden @$s4main9getCxxRefSpys5Int32VGyF : $@convention(thin) () -> UnsafeMutablePointer<Int32>
// CHECK: [[REF:%.*]] = function_ref @{{_Z15getStaticIntRefv|\?getStaticIntRef@@YAAEAHXZ}} : $@convention(c) () -> UnsafeMutablePointer<Int32>
// CHECK: apply [[REF]]() : $@convention(c) () -> UnsafeMutablePointer<Int32>

func getConstCxxRef() -> UnsafePointer<CInt> {
  return getConstStaticIntRef()
}

// CHECK: sil hidden @$s4main14getConstCxxRefSPys5Int32VGyF : $@convention(thin) () -> UnsafePointer<Int32>
// CHECK: [[REF:%.*]] = function_ref @{{_Z20getConstStaticIntRefv|\?getConstStaticIntRef@@YAAEBHXZ}} : $@convention(c) () -> UnsafePointer<Int32>
// CHECK: apply [[REF]]() : $@convention(c) () -> UnsafePointer<Int32>

func getCxxRvalueRef() -> UnsafeMutablePointer<CInt> {
  return getStaticIntRvalueRef()
}

// CHECK: sil hidden @$s4main15getCxxRvalueRefSpys5Int32VGyF : $@convention(thin) () -> UnsafeMutablePointer<Int32>
// CHECK: [[REF:%.*]] = function_ref @{{_Z21getStaticIntRvalueRefv|\?getStaticIntRvalueRef@@YA\$\$QEAHXZ}} : $@convention(c) () -> UnsafeMutablePointer<Int32>
// CHECK: apply [[REF]]() : $@convention(c) () -> UnsafeMutablePointer<Int32>

func getConstCxxRvalueRef() -> UnsafePointer<CInt> {
  return getConstStaticIntRvalueRef()
}

// CHECK: sil hidden @$s4main20getConstCxxRvalueRefSPys5Int32VGyF : $@convention(thin) () -> UnsafePointer<Int32>
// CHECK: [[REF:%.*]] = function_ref @{{_Z26getConstStaticIntRvalueRefv|\?getConstStaticIntRvalueRef@@YA\$\$QEBHXZ}} : $@convention(c) () -> UnsafePointer<Int32>
// CHECK: apply [[REF]]() : $@convention(c) () -> UnsafePointer<Int32>

func setCxxRef() {
  var val: CInt = 21
  withUnsafeMutablePointer(to: &val) {
    setStaticIntRef($0)
  }
}

// CHECK: // closure #1 in setCxxRef()
// CHECK: sil private @$s4main9setCxxRefyyFySpys5Int32VGXEfU_ : $@convention(thin) (UnsafeMutablePointer<Int32>) -> ()
// CHECK: [[REF:%.*]] = function_ref @{{_Z15setStaticIntRefRi|\?setStaticIntRef@@YAXAEAH@Z}} : $@convention(c) (UnsafeMutablePointer<Int32>) -> ()
// CHECK: apply [[REF]](%0) : $@convention(c) (UnsafeMutablePointer<Int32>) -> ()

func setCxxConstRef() {
  var val: CInt = 21
  withUnsafePointer(to: &val) {
    setConstStaticIntRef($0)
  }
}

// CHECK: // closure #1 in setCxxConstRef()
// CHECK: sil private @$s4main14setCxxConstRefyyFySPys5Int32VGXEfU_ : $@convention(thin) (UnsafePointer<Int32>) -> ()
// CHECK: [[REF:%.*]] = function_ref @{{_Z20setConstStaticIntRefRKi|\?setConstStaticIntRef@@YAXAEBH@Z}} : $@convention(c) (UnsafePointer<Int32>) -> ()
// CHECK: apply [[REF]](%0) : $@convention(c) (UnsafePointer<Int32>) -> ()

func setCxxRvalueRef() {
  var val: CInt = 21
  withUnsafeMutablePointer(to: &val) {
    setStaticIntRvalueRef($0)
  }
}

// CHECK: // closure #1 in setCxxRvalueRef()
// CHECK: sil private @$s4main15setCxxRvalueRefyyFySpys5Int32VGXEfU_ : $@convention(thin) (UnsafeMutablePointer<Int32>) -> ()
// CHECK: [[REF:%.*]] = function_ref @{{_Z21setStaticIntRvalueRefOi|\?setStaticIntRvalueRef@@YAX\$\$QEAH@Z}} : $@convention(c) (UnsafeMutablePointer<Int32>) -> ()
// CHECK: apply [[REF]](%0) : $@convention(c) (UnsafeMutablePointer<Int32>) -> ()

func setCxxConstRvalueRef() {
  var val: CInt = 21
  withUnsafePointer(to: &val) {
    setConstStaticIntRvalueRef($0)
  }
}

// CHECK: // closure #1 in setCxxConstRvalueRef()
// CHECK: sil private @$s4main20setCxxConstRvalueRefyyFySPys5Int32VGXEfU_ : $@convention(thin) (UnsafePointer<Int32>) -> ()
// CHECK: [[REF:%.*]] = function_ref @{{_Z26setConstStaticIntRvalueRefOKi|\?setConstStaticIntRvalueRef@@YAX\$\$QEBH@Z}} : $@convention(c) (UnsafePointer<Int32>) -> ()
// CHECK: apply [[REF]](%0) : $@convention(c) (UnsafePointer<Int32>) -> ()
