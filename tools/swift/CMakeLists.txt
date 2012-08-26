add_swift_executable(swift
  Frontend.cpp
  Frontend.h
  Immediate.cpp
  Immediate.h
  PrintingDiagnosticConsumer.cpp
  PrintingDiagnosticConsumer.h
  swift.cpp
  DEPENDS swiftIRGen swiftParse swiftSema swiftAST swiftCFG swiftCFGLowering
  COMPONENT_DEPENDS bitwriter codegen ipo jit linker mcjit asmparser
                                        selectiondag ${LLVM_TARGETS_TO_BUILD})

target_link_libraries(swift edit)

install(TARGETS swift
  RUNTIME DESTINATION bin)
