// RUN: %target-swift-frontend -primary-file %s -O -emit-sil
// Derived from rdar://22936260

class TTTT {}

class XXXXXX {
    var data: TTTT
    init (data: TTTT) { self.data = data }
    func LLLL () -> KKKK? {
        if let EEEE = PPPPP () { return KKKK (LLLL: [EEEE]) }
        return nil
    }

    func PPPPP () -> JJJJJ? {
        if let EEEE = LLLL () { return EEEE }
        return nil
    }
}

class JJJJJ {
    var SSS: JJJJJ { return self }
    func PPPPP (data: TTTT) -> JJJJJ? {
        return XXXXXX (data: data).PPPPP ()
    }
}

class KKKK: JJJJJ {
    var LLLL: [JJJJJ]
    init (LLLL: [JJJJJ]) { self.LLLL = LLLL }
}
