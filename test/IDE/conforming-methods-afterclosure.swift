// RUN: %target-swift-ide-test -conforming-methods -source-filename %s -code-completion-token=AFTER_TRAILINGCLOSURE -module-name MyModule -conforming-methods-expected-types 's:8MyModule7TargetPP' | %FileCheck %s -check-prefix=AFTER_TRAILINGCLOSURE

public protocol TargetP {}
struct ConcreteP: TargetP {}

public struct MyStruct {
    init(arg1: Int = 0, fn: () -> Int) {}

    public func returnSomeP -> some TargetP { ConcreteP() }
    public func returnConcreteP -> ConcreteP { ConcreteP() }
    public func reutrnInt -> Int { 1 }
}

func test() {
    MyStruct {
        1
    } #^AFTER_TRAILINGCLOSURE^#
}

//AFTER_TRAILINGCLOSURE:      -----BEGIN CONFORMING METHOD LIST-----
//AFTER_TRAILINGCLOSURE-NEXT: - TypeName: MyStruct
//AFTER_TRAILINGCLOSURE-NEXT: - Members:
//AFTER_TRAILINGCLOSURE-NEXT:    - Name: returnSomeP()
//AFTER_TRAILINGCLOSURE-NEXT:      TypeName: some TargetP
//AFTER_TRAILINGCLOSURE-NEXT:    - Name: returnConcreteP()
//AFTER_TRAILINGCLOSURE-NEXT:      TypeName: ConcreteP
//AFTER_TRAILINGCLOSURE-NEXT: -----END CONFORMING METHOD LIST-----
