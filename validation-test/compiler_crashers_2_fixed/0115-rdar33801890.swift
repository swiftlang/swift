// RUN: %target-swift-frontend %s -typecheck

class C { var property: String? }
func foo(_ cs : [C?]) -> [String?] {
  return cs.map({ c in
    let a = c.propertyWithTypo ?? "default"
    let b = "\(a)"
    return b
  })
}
