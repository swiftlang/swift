import _Concurrency

func printGeneric<T>(_ t: T) {
  print(t)
}
// CHECK-LL: @"$s4main6call_fyyAA1CCYFTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer 
// CHECK-LL: @"$s4main1CC1fyyYFTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer 

// CHECK-LL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s4main6call_fyyAA1CCYF"(%swift.task* {{%[0-9]+}}, %swift.executor* {{%[0-9]+}}, %swift.context* swiftasync {{%[0-9]+}}) {{#[0-9]*}} {
// CHECK-LL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s4main1CC1fyyYF"(%swift.task* {{%[0-9]+}}, %swift.executor* {{%[0-9]+}}, %swift.context* swiftasync {{%[0-9]+}}) {{#[0-9]*}} {
public func call_f(_ c: C) async {
  print("entering call_f")
  await c.f()
  print("exiting call_f")
}
open class C {
  public init() {}
  func f() async {
    printGeneric("entering f")
    printGeneric(Self.self)
    printGeneric(self)
    printGeneric("exiting f")
  }
}

