
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -module-name retaining_globals -import-objc-header %S/Inputs/globals.h %s | %FileCheck %s
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
  // CHECK: [[bridgeStringFunc:%.*]] = function_ref @{{.*}} : $@convention(method) (@guaranteed Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK: [[wrappedString:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt, [[globalString]] : $NSString
  // CHECK: [[stringMetaType:%.*]] = metatype $@thin String.Type
  // CHECK: [[bridgedString:%.*]] = apply [[bridgeStringFunc]]([[wrappedString]], [[stringMetaType]]) : $@convention(method) (@guaranteed Optional<NSString>, @thin String.Type) -> @owned String
  // CHECK: [[movedBridgedString:%.*]] = move_value [var_decl] [[bridgedString]]
  let string = globalString // Problematic case, wasn't being retained

  // CHECK: [[load_1:%.*]] = load [copy] {{%.*}} : $*Optional<NSObject>
  // CHECK: [[move_1:%.*]] = move_value [lexical] [var_decl] [[load_1]]
  let object = globalObject
  
  // CHECK: [[load_2:%.*]] = load [copy] {{%.*}} : $*Optional<AnyObject>
  // CHECK: [[move_2:%.*]] = move_value [lexical] [var_decl] [[load_2]]
  let id = globalId
  
  // CHECK: [[load_3:%.*]] = load [copy] {{%.*}} : $*Optional<NSArray>
  // CHECK: [[move_3:%.*]] = move_value [lexical] [var_decl] [[load_3]]
  let arr = globalArray
  
  // CHECK: [[load_4:%.*]] = load [copy] {{%.*}} : $*Optional<NSArray>
  // CHECK: [[move_4:%.*]] = move_value [lexical] [var_decl] [[load_4]]
  let constArr = globalConstArray

  // Make sure there's no more copies
  // CHECK-NOT: load [copy]

  print(string as Any)
  print(object as Any)
  print(id as Any)
  print(arr as Any)
  print(constArr as Any)

  // CHECK: [[PRINT_FUN:%.*]] = function_ref @$ss5print_9separator10terminatoryypd_S2StF : $@convention(thin) (@guaranteed Array<Any>, @guaranteed String, @guaranteed String) -> ()
  // CHECK: apply [[PRINT_FUN]]({{.*}})
  // CHECK: destroy_value [[move_4]]
  // CHECK: destroy_value [[move_3]]
  // CHECK: destroy_value [[move_2]]
  // CHECK: destroy_value [[move_1]]
  // CHECK: destroy_value [[movedBridgedString]]

  // Make sure there's no more destroys
  // CHECK-NOT: destroy_value
  // CHECK: } // end sil function '$s17retaining_globals4mainyyF'
}


main()
main()  // Used to crash here, due to use-after-free.
main()
main()
main()
main()
