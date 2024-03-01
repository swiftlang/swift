add_swift_host_library(swiftGenericMetadataBuilder
                       SHARED
                       ExternalGenericMetadataBuilder.cpp
                       LLVM_LINK_COMPONENTS object support
                       )

target_include_directories(swiftGenericMetadataBuilder
                           PRIVATE
                           ${LLVM_MAIN_INCLUDE_DIR})

target_link_libraries(swiftGenericMetadataBuilder
                      PRIVATE
                      swiftDemangling)

swift_install_in_component(TARGETS swiftGenericMetadataBuilder
  COMPONENT compiler
  LIBRARY
    DESTINATION "lib${LLVM_LIBDIR_SUFFIX}"
  ARCHIVE
    DESTINATION "lib${LLVM_LIBDIR_SUFFIX}")
add_dependencies(compiler swiftGenericMetadataBuilder)
