# This source file is part of the Swift.org open source project
#
# Copyright (c) 2022 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

set(dependencies Basic AST)
if(SWIFT_BUILD_REGEX_PARSER_IN_COMPILER)
  list(APPEND dependencies _CompilerRegexParser)
endif()

add_swift_compiler_module(Parse
  DEPENDS
    ${dependencies}
  SOURCES
    Parse.swift
    Regex.swift)
