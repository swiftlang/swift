// Emit the explicit module.
// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-pcm -module-name ExplicitClangModule -o %t/ExplicitClangModule.pcm %S/Inputs/custom-modules/module.map

// Verify some of the output of the -dump-pcm flag.
// RUN: %swift-dump-pcm %t/ExplicitClangModule.pcm | %FileCheck %s --check-prefix=CHECK-DUMP
// CHECK-DUMP: Information for module file '{{.*}}/ExplicitClangModule.pcm':
// CHECK-DUMP:   Module name: ExplicitClangModule
// CHECK-DUMP:   Module map file: {{.*[/\\]}}Inputs{{/|\\}}custom-modules{{/|\\}}module.map

// Make sourcekitd can handle the explicit module too.
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DOT_FIELD_val -Xcc -fmodule-file=%t/ExplicitClangModule.pcm | %FileCheck %s -check-prefix=CHECK-COMPL -strict-whitespace

import ExplicitClangModule
var someClangVar: ExplicitClangModuleType
someClangVar.#^DOT_FIELD_val^#

// CHECK-COMPL: Begin completions, 2 items
// CHECK-COMPL: Keyword[self]/CurrNominal:          self[#ExplicitClangModuleType#];
// CHECK-COMPL: Decl[InstanceVar]/CurrNominal:      value[#Int32#];
// CHECK-COMPL: End completions
// CHECK-COMPL: LookedupTypeNames: ['ExplicitClangModule.ExplicitClangModuleType']
