// RUN: %empty-directory(%t)

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// RUN: %target-build-swift -swift-version 5 -emit-module -Xfrontend -playground -I=%t %s
// RUN: %target-build-swift -swift-version 6 -emit-module -Xfrontend -playground -I=%t %s

// RUN: %target-build-swift -swift-version 5 -emit-module -Xfrontend -playground -Xfrontend -pc-macro -I=%t %s
// RUN: %target-build-swift -swift-version 6 -emit-module -Xfrontend -playground -Xfrontend -pc-macro -I=%t %s

// REQUIRES: executable_test

import PlaygroundSupport

func test1() {
    func buildBlock<Content>(content: Content) {
        content
    }
}

func test2() {
    func buildBlock<Content>(content: Content) -> Content {
        content
        return content
    }
}

func test3<Content>(_ content: Content) {
    func buildBlock<Content2>(_ content2: Content2) -> Content2 {
       return content2
    }
}
