macro(add_swift_module_depends results target)
  list(APPEND ${results} "${target}-macosx")
  list(APPEND ${results} "${target}-macosx-x86_64")
  list(APPEND ${results} "${target}-swiftmodule-macosx-x86_64")
endmacro()

add_swift_module_depends(all_depends swiftTestUtils)
add_swift_module_depends(all_depends swiftSwiftSyntax)

add_swift_host_tool(swift-swiftsyntax-test
  main.swift
  ClassifiedSyntaxTreePrinter.swift
  empty.c # FIXME: If there is no C file in the target Xcode skips the linking phase and doesn't create the executable
  COMPILE_FLAGS "-module-name" "main"
  DEPENDS ${all_depends}
)
