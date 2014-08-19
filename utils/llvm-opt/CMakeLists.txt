set(OPT ${PATH_TO_LLVM_BUILD}/bin/opt)
set(SWIFT_LLVM_DYLIB ${SWIFT_BINARY_DIR}/lib/SwiftARCPasses.dylib)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/llvm-opt.in
  ${CMAKE_CURRENT_BINARY_DIR}/llvm-opt
  @ONLY
  NEWLINE_STYLE UNIX)
add_custom_target(llvm-opt-copy ALL
                  COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/llvm-opt ${SWIFT_BINARY_DIR}/bin/llvm-opt
                  COMMAND chmod u+x ${SWIFT_BINARY_DIR}/bin/llvm-opt
                  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/llvm-opt)
