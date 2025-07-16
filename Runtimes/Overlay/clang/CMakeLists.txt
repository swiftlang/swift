
gyb_expand(float.swift.gyb float.swift)

add_library(swift_Builtin_float
  float.swift
  "${PROJECT_SOURCE_DIR}/linker-support/magic-symbols-for-install-name.c")
set_target_properties(swift_Builtin_float PROPERTIES
  Swift_MODULE_NAME _Builtin_float)
target_compile_options(swift_Builtin_float PRIVATE
  "$<$<AND:$<PLATFORM_ID:Darwin>,$<COMPILE_LANGUAGE:Swift>>:SHELL:-Xfrontend -module-abi-name -Xfrontend Darwin>"
  $<$<COMPILE_LANGUAGE:Swift>:-autolink-force-load>)
target_link_libraries(swift_Builtin_float PRIVATE
  $<$<PLATFORM_ID:Windows>:ClangModules>
  swiftCore)

install(TARGETS swift_Builtin_float
  ARCHIVE DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  LIBRARY DESTINATION "${SwiftOverlay_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")
emit_swift_interface(swift_Builtin_float)
install_swift_interface(swift_Builtin_float)

embed_manifest(swift_Builtin_float)
