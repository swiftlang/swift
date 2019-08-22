cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftAccelerate ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Accelerate.swift
  vImage_Error.swift
  vImage_Options.swift
  vImage_Buffer.swift
  vImage_CVImageFormat.swift
  vImage_CGImageFormat.swift
  vImage_Converter.swift
  AccelerateBuffer.swift
  Quadrature.swift
  vDSP_Arithmetic.swift
  vDSP_Biquad.swift
  vDSP_ClippingLimitThreshold.swift
  vDSP_ComplexConversion.swift
  vDSP_ComplexOperations.swift
  vDSP_Conversion.swift
  vDSP_Convolution.swift
  vDSP_DCT.swift
  vDSP_DFT.swift
  vDSP_DecibelConversion.swift
  vDSP_FFT.swift
  vDSP_FFT_DFT_Common.swift
  vDSP_FIR.swift
  vDSP_FillClearGenerate.swift
  vDSP_Geometry.swift
  vDSP_Integration.swift
  vDSP_Interpolation.swift
  vDSP_PolarRectangularConversion.swift
  vDSP_PolynomialEvaluation.swift
  vDSP_RecursiveFilters.swift
  vDSP_Reduction.swift
  vDSP_SingleVectorOperations.swift
  vDSP_SlidingWindow.swift
  vForce_Operations.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    BNNS.swift.gyb

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Metal Dispatch IOKit Foundation os XPC CoreFoundation ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreFoundation CoreGraphics Dispatch Foundation Metal ObjectiveC os # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreFoundation CoreGraphics Dispatch Foundation Metal ObjectiveC os # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreGraphics Dispatch os CoreFoundation ObjectiveC # auto-updated

  FRAMEWORK_DEPENDS Accelerate

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_WATCHOS}
  INSTALL_IN_COMPONENT sdk-overlay
)
