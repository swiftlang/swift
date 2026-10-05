// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/stats)

// Building a PCM must not populate the ForeignReferenceTypeInfo request cache.
// Emitting the PCM name-imports Wrapper<Frt *>, which has to know whether Frt
// is a foreign reference type. That query must not go through the evaluator.

// RUN: %target-swift-frontend -emit-pcm -module-name TemplateWithFrt -o %t/TemplateWithFrt.pcm %t/Inputs/module.modulemap -cxx-interoperability-mode=default -stats-output-dir %t/stats -print-zero-stats
// RUN: %{python} %utils/process-stats-dir.py --evaluate 'ForeignReferenceTypeInfoRequest == 0' %t/stats

// Confirm those names really do depend on foreign reference type info, so that
// the counter above cannot be zero for the wrong reason.
// RUN: %target-swift-ide-test -print-module -module-to-print=TemplateWithFrt -I %t/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: typealias WrappedFrt = Wrapper<Frt>
// CHECK: typealias WrappedValue = Wrapper<UnsafeMutablePointer<Value>>

//--- Inputs/module.modulemap
module TemplateWithFrt {
  header "template-with-frt.h"
  requires cplusplus
}

//--- Inputs/template-with-frt.h
template <class T>
struct Wrapper {};

class __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Frt {};

struct Value {};

using WrappedFrt = Wrapper<Frt *>;
using WrappedValue = Wrapper<Value *>;
