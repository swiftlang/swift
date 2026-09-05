set(llvm_components
  Support
  Demangle
  TargetParser
  Telemetry
  Core
  ExecutionEngine
  Object
  ObjectYAML
  Option
  BinaryFormat
  MC
  MCDisassembler
  DebugInfoPDB
  DebugInfoCodeView
  DebugInfoDWARF
  RuntimeDyld
  ipo
  MCJIT
  ${LLVM_TARGETS_TO_BUILD}
)
llvm_map_components_to_libnames(llvm_libraries ${llvm_components})
# LLVMDebugInfod is not a component library, must be added explicitly.
list(APPEND llvm_libraries LLVMDebuginfod)

set(clang_libraries
  clangAST
  clangBasic
  clangCodeGen
  clangDriver
  clangEdit
  clangFrontend
  clangLex
  clangParse
  clangRewrite
  clangRewriteFrontend
  clangSema
  clangSerialization
  clangTooling
)

set(swift_libraries
  _CompilerSwiftBasicFormat
  _CompilerSwiftCompilerPluginMessageHandling
  _CompilerSwiftDiagnostics
  _CompilerSwiftIDEUtils
  _CompilerSwiftIfConfig
  _CompilerSwiftLexicalLookup
  _CompilerSwiftOperators
  _CompilerSwiftParser
  _CompilerSwiftParserDiagnostics
  _CompilerSwiftSyntax
  _CompilerSwiftSyntaxBuilder
  _CompilerSwiftSyntaxMacroExpansion
  _CompilerSwiftSyntaxMacros
  _CompilerSwiftWarningControl
  swiftAST
  swiftASTSectionImporter
  swiftBasic
  swiftClangImporter
  swiftCompilerModules
  swiftDependencyScan
  swiftFrontend
  swiftIDE
  swiftIDETool
  swiftParse
  swiftRemoteInspection
  swiftRemoteAST
  swiftSIL
  swiftSILOptimizer
  swiftSerialization
)

add_swift_host_library(SwiftCompilerShared SHARED
  libswiftcompiler.cpp
)

set_output_directory(SwiftCompilerShared
  LIBRARY_DIR "${SWIFT_HOST_LIBRARIES_DEST_DIR}/compiler")

# FIXME: Support non-Apple platforms

# Can't use -all_load because of autolinking. Several of the swift libraries
# have LC_LINKER_OPTIONS (.e.g -lswiftASTGen) which forces the linker to load
# those static archives an additional time. Because the link arguments will
# pass a full path to the archive and the autolink mechanism uses a relative
# path, the linker cannot reason that they are the same and loads everything
# twice.

set(link_args)
foreach(lib ${llvm_libraries} ${clang_libraries})
  list(APPEND link_args ${lib} -Wl,-force_load,$<TARGET_LINKER_FILE:${lib}>)
endforeach()

foreach(lib ${swift_libraries})
  get_target_property(_is_pure_swift ${lib} PURE_SWIFT_LIB)
  get_target_property(_type ${lib} TYPE)
  if (${_is_pure_swift})
    list(APPEND link_args ${lib})
  else()
    if (_type STREQUAL "STATIC_LIBRARY")
      list(APPEND link_args ${lib} -Wl,-force_load,$<TARGET_LINKER_FILE:${lib}>)
    else()
      list(APPEND link_args ${lib})
    endif()
  endif()
endforeach()

target_link_libraries(SwiftCompilerShared PRIVATE ${link_args})

set_property(
  TARGET SwiftCompilerShared
  APPEND PROPERTY INSTALL_RPATH "@loader_path/swift/host/compiler"
)

swift_install_in_component(TARGETS SwiftCompilerShared
  ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/compiler" COMPONENT compiler
  LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/host/compiler" COMPONENT compiler
  RUNTIME DESTINATION "bin" COMPONENT compiler)
