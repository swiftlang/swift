cmake_minimum_required(VERSION 3.28)

project(swift-inspect
  LANGUAGES CXX Swift)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

find_package(ArgumentParser CONFIG REQUIRED)

add_library(SymbolicationShims INTERFACE)
target_include_directories(SymbolicationShims INTERFACE
  Sources/SymbolicationShims)

if(WIN32)
  add_library(SwiftInspectClientInterface INTERFACE)
  target_include_directories(SwiftInspectClientInterface INTERFACE
    Sources/SwiftInspectClientInterface)

  add_library(SwiftInspectClient SHARED
    Sources/SwiftInspectClient/SwiftInspectClient.cpp)
  target_link_libraries(SwiftInspectClient PRIVATE
    SwiftInspectClientInterface)
elseif(LINUX)
  add_library(LinuxSystemHeaders INTERFACE)
  target_include_directories(LinuxSystemHeaders INTERFACE
    Sources/SwiftInspectLinux/SystemHeaders)

  add_library(SwiftInspectLinux STATIC
    Sources/SwiftInspectLinux/ElfFile.swift
    Sources/SwiftInspectLinux/LinkMap.swift
    Sources/SwiftInspectLinux/MemoryMap.swift
    Sources/SwiftInspectLinux/Process.swift
    Sources/SwiftInspectLinux/ProcFS.swift
    Sources/SwiftInspectLinux/SymbolCache.swift)
  target_compile_options(SwiftInspectLinux PRIVATE
    -Xcc -D_GNU_SOURCE)
  target_link_libraries(SwiftInspectLinux PUBLIC
    LinuxSystemHeaders)
endif()

add_executable(swift-inspect
  Sources/swift-inspect/Operations/DumpArray.swift
  Sources/swift-inspect/Operations/DumpCacheNodes.swift
  Sources/swift-inspect/Operations/DumpConcurrency.swift
  Sources/swift-inspect/Operations/DumpConformanceCache.swift
  Sources/swift-inspect/Operations/DumpGenericMetadata.swift
  Sources/swift-inspect/Operations/DumpRawMetadata.swift
  Sources/swift-inspect/Backtrace.swift
  Sources/swift-inspect/DarwinRemoteProcess.swift
  Sources/swift-inspect/LinuxRemoteProcess.swift
  Sources/swift-inspect/main.swift
  Sources/swift-inspect/Process.swift
  Sources/swift-inspect/RemoteMirror+Extensions.swift
  Sources/swift-inspect/RemoteProcess.swift
  Sources/swift-inspect/String+Extensions.swift
  Sources/swift-inspect/Symbolication+Extensions.swift
  Sources/swift-inspect/WindowsRemoteProcess.swift
  Sources/swift-inspect/WinSDK+Extentions.swift)
target_compile_options(swift-inspect PRIVATE
  -parse-as-library)
target_link_libraries(swift-inspect PRIVATE
  ArgumentParser
  swiftRemoteMirror)
if(WIN32)
  target_link_libraries(swift-inspect PRIVATE
    SwiftInspectClientInterface)
elseif(LINUX)
  target_link_libraries(swift-inspect PRIVATE
    SwiftInspectLinux)
endif()

install(TARGETS swift-inspect
  DESTINATION bin)
if(WIN32)
  install(TARGETS SwiftInspectClient
    RUNTIME DESTINATION bin)
endif()
