// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -D CONFIG1 -dump-ast -parse-as-library %s | %FileCheck %s --check-prefixes=CHECK,CHECK-CONFIG1
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -D CONFIG2 -dump-ast -parse-as-library %s | %FileCheck %s --check-prefixes=CHECK,CHECK-CONFIG2
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -D CONFIG3 -dump-ast -parse-as-library %s | %FileCheck %s --check-prefixes=CHECK,CHECK-CONFIG3

// REQUIRES: concurrency

protocol AppConfiguration { }

struct Config1: AppConfiguration {}
struct Config2: AppConfiguration {}
struct Config3: AppConfiguration {}

protocol App {
    associatedtype Configuration: AppConfiguration
}

// Load in the source file name and grab line numbers for default main funcs
// CHECK: (source_file "[[SOURCE_FILE:[^"]+]]"
// CHECK: (extension_decl {{.*}} range={{\[}}[[SOURCE_FILE]]:{{[0-9]+}}:{{[0-9]+}} - line:{{[0-9]+}}:{{[0-9]+}}{{\]}} "App" where
// CHECK: (extension_decl {{.*}} range={{\[}}[[SOURCE_FILE]]:{{[0-9]+}}:{{[0-9]+}} - line:{{[0-9]+}}:{{[0-9]+}}{{\]}} "App" where
// CHECK: (extension_decl {{.*}} range={{\[}}[[SOURCE_FILE]]:{{[0-9]+}}:{{[0-9]+}} - line:{{[0-9]+}}:{{[0-9]+}}{{\]}} "App" where
// CHECK: (extension_decl {{.*}} range={{\[}}[[SOURCE_FILE]]:{{[0-9]+}}:{{[0-9]+}} - line:{{[0-9]+}}:{{[0-9]+}}{{\]}}
// CHECK-NOT: where
// CHECK-NEXT: (func_decl {{.*}} range={{\[}}[[SOURCE_FILE]]:[[DEFAULT_ASYNCHRONOUS_MAIN_LINE:[0-9]+]]:{{[0-9]+}} - line:{{[0-9]+}}:{{[0-9]+}}{{\]}} "main()"
// CHECK-SAME: interface_type="<Self where Self : App> (Self.Type) -> () async -> ()"

extension App where Configuration == Config1 {
// CHECK-CONFIG1: (func_decl {{.*}}implicit range=[{{.*}}:[[@LINE+20]]:1 - line:[[@LINE+20]]:1] "$main()" interface_type="(MainType.Type) -> () -> ()"
// CHECK-CONFIG1: [[SOURCE_FILE]]:[[# @LINE+1 ]]
    static func main() { }
}

extension App where Configuration == Config2 {
// CHECK-CONFIG2: (func_decl {{.*}}implicit range=[{{.*}}:[[@LINE+14]]:1 - line:[[@LINE+14]]:1] "$main()" interface_type="(MainType.Type) -> () async -> ()"
// CHECK-CONFIG2: [[SOURCE_FILE]]:[[# @LINE+1 ]]
    static func main() async { }
}

extension App where Configuration == Config3 {
// CHECK-CONFIG3-ASYNC: (func_decl {{.*}}implicit range=[{{.*}}:[[@LINE+8]]:1 - line:[[@LINE+8]]:1] "$main()" interface_type="(MainType.Type) -> () async -> ()"
// CHECK-CONFIG3-ASYNC: [[SOURCE_FILE]]:[[DEFAULT_ASYNCHRONOUS_MAIN_LINE]]
}

extension App {
    static func main() async { }
}

@main
struct MainType : App {

#if CONFIG1
    typealias Configuration = Config1
#elseif CONFIG2
    typealias Configuration = Config2
#elseif CONFIG3
    typealias Configuration = Config3
#endif
}
