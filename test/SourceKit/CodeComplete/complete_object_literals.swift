func test(color: String) {
    
}

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=2:1 %s -- %s == \
// RUN:   -req=complete -pos=2:1 -req-opts=includeobjectliterals=1 %s -- %s == \
// RUN:   -req=complete -pos=2:1 -req-opts=includeobjectliterals=0 %s -- %s == \
// RUN:   -req=complete -pos=2:1 %s -- %s \
// RUN: | tee %t.out | %FileCheck --check-prefix=CHECK1 %s 

// CHECK1-LABEL: key.results: [
// CHECK1:     {
// CHECK1:       key.kind: source.lang.swift.literal.color,
// CHECK1:       key.name: "#colorLiteral(red:green:blue:alpha:)",
// CHECK1:       key.description: "#colorLiteral(red: Float, green: Float, blue: Float, alpha: Float)",
// CHECK1:       key.sourcetext: "#colorLiteral(red: <#T##Float#>, green: <#T##Float#>, blue: <#T##Float#>, alpha: <#T##Float#>)"
// CHECK1:     },
// CHECK1:     {
// CHECK1:       key.kind: source.lang.swift.literal.image,
// CHECK1:       key.name: "#imageLiteral(resourceName:)",
// CHECK1:       key.description: "#imageLiteral(resourceName: String)",
// CHECK1:       key.sourcetext: "#imageLiteral(resourceName: <#T##String#>)"
// CHECK1:     },

// CHECK1-LABEL: key.results: [
// CHECK1:     {
// CHECK1:       key.kind: source.lang.swift.literal.color,
// CHECK1:       key.name: "#colorLiteral(red:green:blue:alpha:)",
// CHECK1:       key.description: "#colorLiteral(red: Float, green: Float, blue: Float, alpha: Float)",
// CHECK1:       key.sourcetext: "#colorLiteral(red: <#T##Float#>, green: <#T##Float#>, blue: <#T##Float#>, alpha: <#T##Float#>)"
// CHECK1:     },
// CHECK1:     {
// CHECK1:       key.kind: source.lang.swift.literal.image,
// CHECK1:       key.name: "#imageLiteral(resourceName:)",
// CHECK1:       key.description: "#imageLiteral(resourceName: String)",
// CHECK1:       key.sourcetext: "#imageLiteral(resourceName: <#T##String#>)"
// CHECK1:     },

// CHECK1-LABEL: key.results: [
// CHECK1-NOT: source.lang.swift.literal.color
// CHECK1-NOT: colorLiteral
// CHECK1-NOT: source.lang.swift.literal.image,
// CHECK1-NOT: imageLiteral 

// CHECK1-LABEL: key.results: [
// CHECK1:     {
// CHECK1:       key.kind: source.lang.swift.literal.color,
// CHECK1:       key.name: "#colorLiteral(red:green:blue:alpha:)",
// CHECK1:       key.description: "#colorLiteral(red: Float, green: Float, blue: Float, alpha: Float)",
// CHECK1:       key.sourcetext: "#colorLiteral(red: <#T##Float#>, green: <#T##Float#>, blue: <#T##Float#>, alpha: <#T##Float#>)"
// CHECK1:     },
// CHECK1:     {
// CHECK1:       key.kind: source.lang.swift.literal.image,
// CHECK1:       key.name: "#imageLiteral(resourceName:)",
// CHECK1:       key.description: "#imageLiteral(resourceName: String)",
// CHECK1:       key.sourcetext: "#imageLiteral(resourceName: <#T##String#>)"
// CHECK1:     },
