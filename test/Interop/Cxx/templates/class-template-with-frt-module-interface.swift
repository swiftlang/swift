// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithFrt -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:  typealias MagicWrapperFrt = MagicWrapper<Foo>
// CHECK:  typealias MagicWrapperConstFrt = MagicWrapper<__cxxConst<Foo>>
// CHECK:  typealias MagicWrapperVolatileFrt = MagicWrapper<__cxxVolatile<Foo>>
// CHECK:  typealias MagicWrapperVolatileFrtRef = MagicWrapper<__cxxLRef<__cxxVolatile<Foo>>>