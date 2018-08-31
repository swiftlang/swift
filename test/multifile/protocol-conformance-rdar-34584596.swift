// RUN: not --crash %target-swift-frontend -emit-ir -O -o - -primary-file %S/Inputs/rdar-34584596-A.swift %S/Inputs/rdar-34584596-B.swift %S/Inputs/rdar-34584596-C.swift -module-name main

