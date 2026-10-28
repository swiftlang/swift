// RUN: %target-swift-ide-test \
// RUN:   -print-module \
// RUN:   -module-to-print=FunctionTemplateParameterPack \
// RUN:   -I %S/Inputs \
// RUN:   -source-filename=x \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   | %FileCheck %s --implicit-check-not Pack --implicit-check-not pack

// Ensure that function templates with a template parameter pack are not
// imported. In our test, those contain 'pack'/'Pack' in their names, so we use
// --implicit-check-not to verify that.  The struct HasVariadicTemplateMembers
// itself should be printed.

// CHECK: struct HasVariadicTemplateMembers
