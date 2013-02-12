add_swift_executable(swift
  Completion.cpp
  Frontend.cpp
  Immediate.cpp
  PrintingDiagnosticConsumer.cpp
  swift.cpp
  DEPENDS swiftIRGen swiftParse swiftSema swiftClangImporter swiftAST swiftSIL
          swiftSILGen
  COMPONENT_DEPENDS bitreader bitwriter codegen ipo jit linker mcjit asmparser
                                        selectiondag ${LLVM_TARGETS_TO_BUILD})

target_link_libraries(swift edit)

if(HAVE_MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)
