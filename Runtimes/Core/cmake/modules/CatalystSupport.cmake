# Add flags for generating the zippered target variant in the build

if(SwiftCore_COMPILER_VARIANT_TARGET)
  add_compile_options(
    "$<$<COMPILE_LANGUAGE:C,CXX>:SHELL:-darwin-target-variant ${SwiftCore_COMPILER_VARIANT_TARGET}>"
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-target-variant ${SwiftCore_COMPILER_VARIANT_TARGET}>"

    # TODO: Remove me once we have a driver with
    #   https://github.com/swiftlang/swift-driver/pull/1803
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xclang-linker -darwin-target-variant -Xclang-linker ${SwiftCore_COMPILER_VARIANT_TARGET}>")

  add_link_options(
    "$<$<LINK_LANGUAGE:C,CXX>:SHELL:-darwin-target-variant ${SwiftCore_COMPILER_VARIANT_TARGET}>"
    "$<$<LINK_LANGUAGE:Swift>:SHELL:-target-variant ${SwiftCore_COMPILER_VARIANT_TARGET}>"

    # TODO: Remove me once we have a driver with
    #   https://github.com/swiftlang/swift-driver/pull/1803
    "$<$<LINK_LANGUAGE:Swift>:SHELL:-Xclang-linker -darwin-target-variant -Xclang-linker ${SwiftCore_COMPILER_VARIANT_TARGET}>")
endif()
