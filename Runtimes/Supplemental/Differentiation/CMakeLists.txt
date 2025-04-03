cmake_minimum_required(VERSION 3.29)

set(CMAKE_POSITION_INDEPENDENT_CODE YES)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules")

if($ENV{BUILD_NUMBER})
  math(EXPR BUILD_NUMBER "$ENV{BUILD_NUMBER} % 65535")
  set(BUILD_NUMBER ".${BUILD_NUMBER}")
endif()
project(SwiftDifferentiation
  LANGUAGES Swift C
  VERSION 6.1.0${BUILD_NUMBER})

if(NOT PROJECT_IS_TOP_LEVEL)
  message(SEND_ERROR "Swift Differentiation must build as a standalone project")
endif()

set(${PROJECT_NAME}_SWIFTC_SOURCE_DIR
  "${PROJECT_SOURCE_DIR}/../../../"
  CACHE FILEPATH "Path to the root source directory of the Swift compiler")

find_package(SwiftCore)

include(gyb)

option(SwiftDifferentiation_ENABLE_VECTOR_TYPES "Enable vector support"
  ${SwiftCore_ENABLE_VECTOR_TYPES})

add_compile_options(
  $<$<COMPILE_LANGUAGE:Swift>:-explicit-module-build>
  $<$<COMPILE_LANGUAGE:Swift>:-nostdlibimport>
  $<$<COMPILE_LANGUAGE:Swift>:-parse-stdlib>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-library-level api>"
  $<$<COMPILE_LANGUAGE:Swift>:-enforce-exclusivity=unchecked>
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-target-min-inlining-version min>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NoncopyableGenerics2>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SuppressedAssociatedTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature SE427NoInferenceOnExtension>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature NonescapableTypes>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature LifetimeDependence>"
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-enable-experimental-feature MemberImportVisibility>")

if(SwiftDifferentiation_ENABLE_VECTOR_TYPES)
  gyb_expand(SIMDDifferentiation.swift.gyb SIMDDifferentiation.swift)
endif()
gyb_expand(TgmathDerivatives.swift.gyb TgmathDerivatives.swift)
gyb_expand(FloatingPointDifferentiation.swift.gyb
  FloatingPointDifferentiation.swift)

add_library(swift_Differentiation
  AnyDifferentiable.swift
  ArrayDifferentiation.swift
  Differentiable.swift
  DifferentialOperators.swift
  DifferentiationUtilities.swift
  OptionalDifferentiation.swift
  $<$<BOOL:${SwiftDifferentiation_ENABLE_VECTOR_TYPES}>:SIMDDifferentiation.swift>
  TgmathDerivatives.swift
  FloatingPointDifferentiation.swift
  linker-support/magic-symbols-for-install-name.c)

set_target_properties(swift_Differentiation PROPERTIES
  Swift_MODULE_NAME _Differentiation
  Swift_COMPILATION_MODE wholemodule)

  target_link_libraries(swift_Differentiation PRIVATE swiftCore)
