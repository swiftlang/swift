// RUN: %target-swift-frontend -c -primary-file %s -I %S/Inputs -cxx-interoperability-mode=default -O -module-name test

import StdSingleFieldAggregate

func test() -> Bool {
  let opt = makeOptionalSingleFieldArray()
  if let sfa = opt.value {
    return sfa.elements[0].value != nil
  }
  return false
}
