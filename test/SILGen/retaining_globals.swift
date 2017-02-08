// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/globals.h -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop


// This test makes sure loading from globals properly retains/releases loads from globals.
// NSString was the only real problem, as the compiler treats NSString globals specially.
// The rest of these are just hedges against future changes.

// From header:
// globalString: __strong NSString*
// globalObject: __strong NSObject*
// globalID: __strong id
// globalArray: __strong NSArray*
// globalConstArray: __strong NSArray *const

func main() {
  Globals.sharedInstance() // Initialize globals (dispatch_once)

  // CHECK: global_addr @globalConstArray : $*Optional<NSArray>
  // CHECK: global_addr @globalArray : $*Optional<NSArray>
  // CHECK: global_addr @globalId : $*Optional<AnyObject>
  // CHECK: global_addr @globalObject : $*Optional<NSObject>
  // CHECK: global_addr @globalString : $*NSString



  // CHECK: [[globalString:%.*]] = load [copy] {{%.*}} : $*NSString
  // CHECK: [[bridgeStringFunc:%.*]] = function_ref @{{.*}} : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK: [[wrappedString:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[globalString]] : $NSString
  // CHECK: [[stringMetaType:%.*]] = metatype $@thin String.Type
  // CHECK: [[bridgedString:%.*]] = apply [[bridgeStringFunc]]([[wrappedString]], [[stringMetaType]]) : $@convention(method) (@owned Optional<NSString>, @thin String.Type) -> @owned String
  let string = globalString // Problematic case, wasn't being retained

  // CHECK: load [copy] {{%.*}} : $*Optional<NSObject>
  let object = globalObject
  
  // CHECK: load [copy] {{%.*}} : $*Optional<AnyObject>
  let id = globalId
  
  // CHECK: load [copy] {{%.*}} : $*Optional<NSArray>
  let arr = globalArray
  
  // CHECK: load [copy] {{%.*}} : $*Optional<NSArray>
  let constArr = globalConstArray

  // Make sure there's no more copies
  // CHECK-NOT: load [copy]

  print(string as Any)
  print(object as Any)
  print(id as Any)
  print(arr as Any)
  print(constArr as Any)

  // CHECK: destroy_value
  // CHECK: destroy_value
  // CHECK: destroy_value
  // CHECK: destroy_value
  // CHECK: destroy_value

  // Make sure there's no more destroys
  // CHECK-NOT: destroy_value
}


main()
main()  // Used to crash here, due to use-after-free.
main()
main()
main()
main()
