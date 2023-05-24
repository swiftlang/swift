//--- test.swift
@DelegatedConformance
@wrapAllProperties
struct Generic<Element> {

  @myPropertyWrapper
  @otherAttr
  var value: Int

  func member() {}
  var otherVal: Int = 1

  #bitwidthNumberedStructs("blah")
}

//--- DelegatedConformance.json
{
  key.macro_roles: [source.lang.swift.macrorole.conformance, source.lang.swift.macrorole.member],
  key.modulename: "MacroDefinition",
  key.typename: "DelegatedConformanceMacro",
}

//--- myPropertyWrapper.json
{
  key.macro_roles: [source.lang.swift.macrorole.accessor, source.lang.swift.macrorole.peer],
  key.modulename: "MacroDefinition",
  key.typename: "PropertyWrapperMacro",
}

//--- wrapAllProperties.json
{
  key.macro_roles: [source.lang.swift.macrorole.memberattribute],
  key.modulename: "MacroDefinition",
  key.typename: "WrapAllProperties",
}

//--- bitwidthNumberedStructs.json
{
  key.macro_roles: [source.lang.swift.macrorole.declaration],
  key.modulename: "MacroDefinition",
  key.typename: "DefineBitwidthNumberedStructsMacro",
}

//--- dummy.script
// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/plugins
// RUN: split-file %s %t

//##-- Prepare the macro plugin.
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/plugins/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %sourcekitd-test -req=syntactic-expandmacro \
// RUN:   -req-opts=1:1:%t/DelegatedConformance.json \
// RUN:   -req-opts=5:3:%t/myPropertyWrapper.json \
// RUN:   -req-opts=2:1:%t/wrapAllProperties.json \
// RUN:   -req-opts=12:3:%t/bitwidthNumberedStructs.json \
// RUN:   %t/test.swift \
// RUN:   -- \
// RUN:   %t/test.swift \
// RUN:   -plugin-path %t/plugins -Xfrontend -dump-macro-expansions \
// RUN:   -module-name TestModule \
// RUN:   | tee %t.response

// RUN: diff -u %s.expected %t.response
