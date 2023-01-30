set(datafiles
    _SwiftCxxInteroperability.h
    _SwiftStdlibCxxOverlay.h
    experimental-interoperability-version.json
)

set(SWIFTLIB_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib/swift")
set(output_dir "${SWIFTLIB_DIR}/swiftToCxx")

add_custom_command(
    OUTPUT "${output_dir}"
    COMMAND ${CMAKE_COMMAND} "-E" "make_directory" "${output_dir}")

set (outputs)

foreach(input ${datafiles})
  set(source "${CMAKE_CURRENT_SOURCE_DIR}/${input}")
  set(dest "${output_dir}/${input}")
  add_custom_command(OUTPUT
                       "${output_dir}/${input}"
                     DEPENDS
                       "${CMAKE_CURRENT_SOURCE_DIR}/${input}"
                     COMMAND
                       "${CMAKE_COMMAND}" "-E" "copy" "${source}" "${dest}")
  list(APPEND outputs "${output_dir}/${input}")
endforeach()
list(APPEND outputs "${output_dir}")

add_custom_target("copy_swiftToCxx_shims"
    DEPENDS "${output_dir}" "${outputs}"
    COMMENT "Copying Swift to C++ shims to ${output_dir}")

swift_install_in_component(FILES ${datafiles}
                           DESTINATION "lib/swift/swiftToCxx"
                           COMPONENT compiler)

add_swift_host_library(swiftPrintAsClang STATIC
  ClangSyntaxPrinter.cpp
  DeclAndTypePrinter.cpp
  ModuleContentsWriter.cpp
  PrimitiveTypeMapping.cpp
  PrintAsClang.cpp
  PrintClangClassType.cpp
  PrintClangFunction.cpp
  PrintClangValueType.cpp
  PrintSwiftToClangCoreScaffold.cpp
  SwiftToClangInteropContext.cpp)
target_link_libraries(swiftPrintAsClang PRIVATE
  swiftAST
  swiftClangImporter
  swiftFrontend
  swiftIDE
  swiftIRGen)

add_dependencies(swiftPrintAsClang
  "copy_swiftToCxx_shims")

set_swift_llvm_is_available(swiftPrintAsClang)
