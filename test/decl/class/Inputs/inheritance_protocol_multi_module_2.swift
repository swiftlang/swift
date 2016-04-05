import Mod

class ClassLevel3: ClassLevel2 {
    override init() {
        super.init()
    }
}

public func createClassLevel3() -> MyProtocol {
    return ClassLevel3()
}

public func createClassLevel3() -> MyProtocol2 {
    return ClassLevel3()
}

