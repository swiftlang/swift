// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultedTemplateTypeParameter -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct X {
// CHECK:   init<T>(_: T)
// CHECK:   init()
// CHECK: }

// CHECK: func defaultedTemplateTypeParam()
// CHECK: func defaultedTemplateTypeParamUsedInArgs<T>(_: T)
// CHECK: func defaultedTemplateTypeParamUsedInReturn<T>() -> T
// CHECK: func defaultedTemplateTypeParamAndDefaultedParam<T>(_: T)
// CHECK: func functionTemplateWithDefaultedParam<T>(_: T)
// CHECK: func defaultedTemplateTypeParamUsedInSignatureAndUnrelatedParam<T>(_: Int32, _: T)
// CHECK: func defaultedTemplateTypeParamAndUnrelatedParam(_: Int32)
// CHECK: func overloadedDefaultedTemplate<T>(_: T)
// CHECK: func overloadedDefaultedTemplate(_: Int32)
// CHECK: func defaultedTemplateReferenceTypeParam<T>(_ t: inout T)
// The following types aren't imported correctly, but that does not have to do
// with the fact that the template type parameters are defaulted.
// TODO: reenable the following checks: (rdar://90587703)
// TODO-CHECK: func defaultedTemplatePointerTypeParam<T>(_ t: UnsafeMutablePointer<T>)
// TODO-CHECK: func defaultedTemplatePointerPointerTypeParam<T>(_ t: UnsafeMutablePointer<OpaquePointer?>!)

// CHECK: func defaultedTemplatePointerTypeParam<T>(_ t: UnsafeMutablePointer<T>!)
// We don't support references to dependent types (rdar://89034440).
// CHECK-NOT: defaultedTemplatePointerReferenceTypeParam
