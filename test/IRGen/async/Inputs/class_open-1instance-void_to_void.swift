import _Concurrency

func printGeneric<T>(_ t: T) {
  print(t)
}
// CHECK-LL: @"$s4main6call_fyyAA1CCYaFTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer
// CHECK-LL: @"$s4main1CC1fyyYaFTu" = {{(dllexport )?}}{{(protected )?}}global %swift.async_func_pointer

// CHECK-LL: define {{(dllexport )?}}{{(protected )?}}swift{{(tail)?}}cc void @"$s4main6call_fyyAA1CCYaF"(
// CHECK-LL: define {{(dllexport )?}}{{(protected )?}}swift{{(tail)?}}cc void @"$s4main1CC1fyyYaF"(
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

