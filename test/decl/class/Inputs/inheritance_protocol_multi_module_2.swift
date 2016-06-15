import Mod

class ClassLevel3: ClassLevel2 {
    override init() {
        super.init()
    }
}

public func createClassLevel3() -> MyProtocol {
    let class3 =  ClassLevel3()
    return class3 // <- Compiler error. Says doesn't conform to MyProtocol
}
