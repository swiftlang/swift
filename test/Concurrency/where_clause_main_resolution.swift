// RUN: %target-swift-frontend -disable-availability-checking -D CONFIG1 -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-CONFIG1
// RUN: %target-swift-frontend -disable-availability-checking -D CONFIG2 -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-CONFIG2
// RUN: %target-swift-frontend -disable-availability-checking -D CONFIG3 -dump-ast -parse-as-library %s | %FileCheck %s --check-prefix=CHECK-CONFIG3

// REQUIRES: concurrency

protocol AppConfiguration { }

struct Config1: AppConfiguration {}
struct Config2: AppConfiguration {}
struct Config3: AppConfiguration {}

protocol App {
    associatedtype Configuration: AppConfiguration
}

extension App where Configuration == Config1 {
// CHECK-CONFIG1: (func_decl implicit "$main()" interface type='(MainType.Type) -> () -> ()'
// CHECK-CONFIG1: where_clause_main_resolution.swift:[[# @LINE+1 ]]
    static func main() { }
}

extension App where Configuration == Config2 {
// CHECK-CONFIG2: (func_decl implicit "$main()" interface type='(MainType.Type) -> () async -> ()'
// CHECK-CONFIG2: where_clause_main_resolution.swift:[[# @LINE+1 ]]
    static func main() async { }
}

extension App {
// CHECK-CONFIG3: (func_decl implicit "$main()" interface type='(MainType.Type) -> () async -> ()'
// CHECK-CONFIG3: where_clause_main_resolution.swift:[[# @LINE+1 ]]
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
