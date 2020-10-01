// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -O -wmo -save-optimization-record=bitstream -save-optimization-record-path %t/optrecordmod.opt.bitstream %s -module-name optrecordmod -o %t/opt-record.o 2>&1 | %FileCheck -allow-empty %s
// RUN: llvm-bcanalyzer -dump %t/optrecordmod.opt.bitstream | %FileCheck -check-prefix=BITSTREAM %s
// RUN: otool -l %t/opt-record.o | %FileCheck -check-prefix=OBJ %s

// REQUIRES: VENDOR=apple

// Ensure we emitted the appropriate section

// OBJ: sectname __remarks

// CHECK-NOT: remark

var a: Int = 1

#sourceLocation(file: "custom.swift", line: 2000)
func foo() {
  a = 2
}
#sourceLocation() // reset

public func bar() {
  foo()
  // BITSTREAM: <Remark NumWords=13 BlockCodeSize=4>
  // BITSTREAM-NEXT:   <Remark header abbrevid=4 op0=1 op1=0 op2=1 op3=2/>
  // BITSTREAM-NEXT:   <Remark debug location abbrevid=5 op0=3 op1=[[@LINE-3]] op2=3/>
  // BITSTREAM-NEXT:   <Argument with debug location abbrevid=7 op0=4 op1=5 op2=6 op3=2000 op4=6/>
  // BITSTREAM-NEXT:   <Argument abbrevid=8 op0=7 op1=8/>
  // BITSTREAM-NEXT:   <Argument with debug location abbrevid=7 op0=9 op1=10 op2=3 op3=[[@LINE-7]] op4=13/>
  // BITSTREAM-NEXT:   <Argument abbrevid=8 op0=7 op1=11/>
  // BITSTREAM-NEXT:   <Argument abbrevid=8 op0=12 op1=13/>
  // BITSTREAM-NEXT:   <Argument abbrevid=8 op0=7 op1=14/>
  // BITSTREAM-NEXT:   <Argument abbrevid=8 op0=15 op1=16/>
  // BITSTREAM-NEXT:   <Argument abbrevid=8 op0=7 op1=17/>
  // BITSTREAM-NEXT: </Remark>
}
