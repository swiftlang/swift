// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -package-name MyModule | %FileCheck %s

private func myPrivateFunc() {}
// CHECK: [[@LINE-1]]:14 | function(fileprivate)/Swift | myPrivateFunc() | {{.*}} | Def | rel: 0

fileprivate func myFileprivateFunc() {}
// CHECK: [[@LINE-1]]:18 | function(fileprivate)/Swift | myFileprivateFunc() | {{.*}} | Def | rel: 0

func myInternalFunc() {}
// CHECK: [[@LINE-1]]:6 | function(internal)/Swift | myInternalFunc() | {{.*}} | Def | rel: 0

package func myPackageFunc() {}
// CHECK: [[@LINE-1]]:14 | function(package)/Swift | myPackageFunc() | {{.*}} | Def | rel: 0

public func myPublicFunc() {}
// CHECK: [[@LINE-1]]:13 | function(public)/Swift | myPublicFunc() | {{.*}} | Def | rel: 0

open class MyOpenClass {}
// CHECK: [[@LINE-1]]:12 | class(public)/Swift | MyOpenClass | {{.*}} | Def | rel: 0

@_spi(MySPI)
public class MyPublicSPIClass {
// CHECK: [[@LINE-1]]:14 | class(SPI)/Swift | MyPublicSPIClass | {{.*}} | Def | rel: 0

  public func publicInSPI() {}
  // CHECK: [[@LINE-1]]:15 | instance-method(SPI)/Swift | publicInSPI() | {{.*}} | Def,Dyn,RelChild | rel: 1
}

@_spi(MySPI)
open class MyOpenSPIClass {}
// CHECK: [[@LINE-1]]:12 | class(SPI)/Swift | MyOpenSPIClass | {{.*}} | Def | rel: 0

private class Foo {
  public func publicInPrivate() {}
  // CHECK: [[@LINE-1]]:15 | instance-method(fileprivate)/Swift | publicInPrivate() | {{.*}} | Def,Dyn,RelChild | rel: 1
}

private enum PrivateEnum {
  // CHECK: [[@LINE-1]]:14 | enum(fileprivate)/Swift | PrivateEnum | {{.*}} | Def | rel: 0
  case myCase
  // CHECK: [[@LINE-1]]:8 | enumerator(fileprivate)/Swift | myCase | {{.*}} | Def,RelChild | rel: 1
}
extension PrivateEnum {
  private func f() { print("Hello") }
  // CHECK: [[@LINE-1]]:16 | instance-method(less_than_private)/Swift | f() | {{.*}} | Def,RelChild | rel: 1
}

enum InternalEnum {
  // CHECK: [[@LINE-1]]:6 | enum(internal)/Swift | InternalEnum | {{.*}} | Def | rel: 0
  case myCase
  // CHECK: [[@LINE-1]]:8 | enumerator(internal)/Swift | myCase | {{.*}} | Def,RelChild | rel: 1
}
