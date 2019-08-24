let globalSec : Int = NOTYPECHECK_initglobal

func secFn() {
  NOTYPECHECK_secFn
}

class ClsSec {
  let member : Int = NOTYPECHECK_member
}

struct SSec {
  let NOTYPECHECK_member = NOTYPECHECK_SSecmember
}

private class NOTYPECHECK_privateClass {}
private let NOTYPECHECK_privateGlobal = NOTYPECHECK_initprivateGlobal
