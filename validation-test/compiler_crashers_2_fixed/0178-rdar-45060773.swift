// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -c -primary-file %s %S/Inputs/0178-rdar-45060773-other.swift -o /dev/null

// REQUIRES: objc_interop

func doit() {
  MyCls().something(true)
}
