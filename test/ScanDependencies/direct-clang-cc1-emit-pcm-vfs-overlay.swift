// REQUIRES: OS=windows-msvc

// Test that the -direct-clang-cc1-module-build is able to create a module from the VC runtime with an overlaid modulemap file

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift_frontend_plain -target %target-triple -module-cache-path %t/clang-module-cache -scan-dependencies -module-name Test -sdk %S/Inputs/WinSDK %t/Test.swift -o %t/deps.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SAL > %t/SAL.cmd
// RUN: %swift_frontend_plain @%t/SAL.cmd

//--- Test.swift
import SAL
