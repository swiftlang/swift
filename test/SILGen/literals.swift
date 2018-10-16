// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

func takesOptionalFunction(_: (() -> ())?) {}

struct CustomNull : ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

func takesANull(_: CustomNull) {}

// CHECK-LABEL: sil hidden @$s8literals4testyyF : $@convention(thin) () -> ()
func test() {
  // CHECK: [[NIL:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.none!enumelt
  // CHECK: [[FN:%.*]] = function_ref @$s8literals21takesOptionalFunctionyyyycSgF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesOptionalFunction(nil)

  // CHECK: [[METATYPE:%.*]] = metatype $@thin CustomNull.Type
  // CHECK: [[NIL_FN:%.*]] = function_ref @$s8literals10CustomNullV10nilLiteralACyt_tcfC
  // CHECK: [[NIL:%.*]] = apply [[NIL_FN]]([[METATYPE]])
  // CHECK: [[FN:%.*]] = function_ref @$s8literals10takesANullyyAA10CustomNullVF
  // CHECK: apply [[FN]]([[NIL]])
  _ = takesANull(nil)
}

class CustomStringClass : ExpressibleByStringLiteral {
  required init(stringLiteral value: String) {}
  required init(extendedGraphemeClusterLiteral value: String) {}
  required init(unicodeScalarLiteral value: String) {}
}

class CustomStringSubclass : CustomStringClass {}

// CHECK-LABEL: sil hidden @$s8literals27returnsCustomStringSubclassAA0cdE0CyF : $@convention(thin) () -> @owned CustomStringSubclass
// CHECK: [[METATYPE:%.*]] = metatype $@thick CustomStringSubclass.Type
// CHECK: [[UPCAST:%.*]] = upcast [[METATYPE]] : $@thick CustomStringSubclass.Type to $@thick CustomStringClass.Type
// CHECK: [[CTOR:%.*]] = class_method [[UPCAST]] : $@thick CustomStringClass.Type, #CustomStringClass.init!allocator.1 : (CustomStringClass.Type) -> (String) -> CustomStringClass, $@convention(method) (@owned String, @thick CustomStringClass.Type) -> @owned CustomStringClass
// CHECK: [[RESULT:%.*]] = apply [[CTOR]]({{%.*}}, [[UPCAST]])
// CHECK: [[DOWNCAST:%.*]] = unchecked_ref_cast [[RESULT]] : $CustomStringClass to $CustomStringSubclass
// CHECK: return [[DOWNCAST]]
func returnsCustomStringSubclass() -> CustomStringSubclass {
  return "hello world"
}
