// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateTemplateParameter -cxx-interoperability-mode=default -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: typealias TemplatedWrappedMagicInt = TemplatedMagicWrapper<MagicWrapper>
// CHECK: typealias HasTraits1 = HasTemplateTemplateParam<Traits1>
// CHECK: typealias HasTraits2 = HasTemplateTemplateParam<Traits2>
// CHECK: typealias HasNestedTraits = HasTemplateTemplateParam<Outer.NestedTraits>
