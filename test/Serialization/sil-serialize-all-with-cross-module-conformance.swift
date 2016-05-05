// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-as-library -module-name ProtoModule -o %t/ProtoModule.swiftmodule %s -DPROTO
// RUN: %target-swift-frontend -emit-module -parse-as-library -module-name ModelModule -o %t/ModelModule.swiftmodule %s -DMODEL -I %t -sil-serialize-all
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -o /dev/null %s -DUSE -I %t

#if PROTO

public protocol Proto {
  func method<T>(_: T?)
}
  
extension Proto {
  public func method<T>(_: T?) {}
}

#elseif MODEL

import ProtoModule

public struct Model : Proto {
  public init() {}
}


#elseif USE

import ProtoModule
import ModelModule

struct OtherStruct {}

func test<T: Proto>(_ x: T) {
  x.method(OtherStruct())
}

func main() {
  test(Model())
}

#else

let _ = invalid_configuration()

#endif
