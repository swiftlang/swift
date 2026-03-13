// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// Test that properties used as subscript keys are treated as reads (getter), not writes (setter).
let key = "key"
var dictionary: [String: Any] = [:]
// CHECK: [[@LINE+2]]:12 | variable/Swift | key | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE+1]]:12 | function/acc-get/Swift | getter:key | {{.*}} | Ref,Call,Impl | rel: 0
dictionary[key] = 42

// Test that chained property accesses used as subscript keys are treated as reads.
struct Config {
  var settings: Settings
}
struct Settings {
  var apiKey: String
}
let config = Config(settings: Settings(apiKey: "secret"))
var apiKeyDict: [String: String] = [:]
// CHECK: [[@LINE+6]]:12 | variable/Swift | config | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE+5]]:12 | function/acc-get/Swift | getter:config | {{.*}} | Ref,Call,Impl | rel: 0
// CHECK: [[@LINE+4]]:19 | instance-property/Swift | settings | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE+3]]:19 | instance-method/acc-get/Swift | getter:settings | {{.*}} | Ref,Call,Impl | rel: 0
// CHECK: [[@LINE+2]]:28 | instance-property/Swift | apiKey | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE+1]]:28 | instance-method/acc-get/Swift | getter:apiKey | {{.*}} | Ref,Call,Impl | rel: 0
apiKeyDict[config.settings.apiKey] = "new-secret"

// Test that keypaths used as subscript keys are also treated as reads.
struct KeyPathContainer {
  var value: Int = 0
}
let container = KeyPathContainer()
var keyPathDict: [KeyPath<KeyPathContainer, Int>: Int] = [:]
// CHECK: [[@LINE+3]]:31 | instance-property/Swift | value | {{.*}} | Ref,Read | rel: 0
// CHECK: [[@LINE+2]]:31 | instance-method/acc-get/Swift | getter:value | {{.*}} | Ref,Call,Impl | rel: 0
// CHECK: [[@LINE+1]]:14 | struct/Swift | KeyPathContainer | {{.*}} | Ref | rel: 0
keyPathDict[\KeyPathContainer.value] = 42
