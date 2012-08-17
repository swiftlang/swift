// RUN: %swift -cfg-dump %s > %t 2>&1
// RUN: FileCheck --input-file=%t %s

func bar(x:Int) {
  for i in 1..100 {
   println(i + x)
  }
}

func foo() {
  bar(42);
}

// CHECK: func_decl foo
// CHECK-NEXT:   b0:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=convertFromIntegerLiteral)
// CHECK-NEXT:     %i3 = TypeOf(type=metatype<Int64>)
// CHECK-NEXT:     %i4 = ThisApply(fn=%b0.i2,arg=%b0.i3)
// CHECK-NEXT:     %i5 = Integer(val=42,width=64)
// CHECK-NEXT:     %i6 = Call(fn=%b0.i4,args=(%b0.i5))
// CHECK-NEXT:     %i7 = Tuple(%b0.i6)
// CHECK-NEXT:     %i8 = Call(fn=%b0.i1,args=(%b0.i6))
// CHECK-NEXT:     %i9 = Return
// CHECK-NEXT:     Preds:
// CHECK-NEXT:     Succs:

func foo(x:Int, y:Bool) {
  if (y) {
   bar(x);
  }
  bar(x);
}

// CHECK: func_decl foo
// CHECK-NEXT:   b0:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b1,b2))
// CHECK-NEXT:     Preds:
// CHECK-NEXT:     Succs: b1 b2
// CHECK-NEXT:   b1:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b1.i2)
// CHECK-NEXT:     %i4 = Tuple(%b1.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b1.i1,args=(%b1.i3))
// CHECK-NEXT:     %i6 = br b2
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b2
// CHECK-NEXT:   b2:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b2.i2)
// CHECK-NEXT:     %i4 = Tuple(%b2.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b2.i1,args=(%b2.i3))
// CHECK-NEXT:     %i6 = Return
// CHECK-NEXT:     Preds: b1 b0
// CHECK-NEXT:     Succs:

func taz(x:Int, y:Bool) {
  if (y) {
   bar(x);
  }
  else {
   foo(x, y);
  }
  bar(x);
}

// CHECK: func_decl taz
// CHECK-NEXT:   b0:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b1,b2))
// CHECK-NEXT:     Preds:
// CHECK-NEXT:     Succs: b1 b2
// CHECK-NEXT:   b1:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b1.i2)
// CHECK-NEXT:     %i4 = Tuple(%b1.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b1.i1,args=(%b1.i3))
// CHECK-NEXT:     %i6 = br b3
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b3
// CHECK-NEXT:   b2:
// CHECK-NEXT:     %i1 = DeclRef(decl=foo)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b2.i2)
// CHECK-NEXT:     %i4 = DeclRef(decl=y)
// CHECK-NEXT:     %i5 = Load(lvalue=%b2.i4)
// CHECK-NEXT:     %i6 = Tuple(%b2.i3,%b2.i5)
// CHECK-NEXT:     %i7 = Call(fn=%b2.i1,args=(%b2.i3 %b2.i5))
// CHECK-NEXT:     %i8 = br b3
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b3
// CHECK-NEXT:   b3:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b3.i2)
// CHECK-NEXT:     %i4 = Tuple(%b3.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b3.i1,args=(%b3.i3))
// CHECK-NEXT:     %i6 = Return
// CHECK-NEXT:     Preds: b1 b2
// CHECK-NEXT:     Succs:

func nested_if(x:Int, y:Bool, z:Bool) {
  if (y) {
    if (z) {
      bar(x);
    }
  }
  else {
    if (z) {
      foo(x, y);
    }
  }
  bar(x);
}

// CHECK: func_decl nested_if
// CHECK-NEXT:   b0:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b1,b3))
// CHECK-NEXT:     Preds:
// CHECK-NEXT:     Succs: b1 b3
// CHECK-NEXT:   b1:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b2,b5))
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b2 b5
// CHECK-NEXT:   b2:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b2.i2)
// CHECK-NEXT:     %i4 = Tuple(%b2.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b2.i1,args=(%b2.i3))
// CHECK-NEXT:     %i6 = br b5
// CHECK-NEXT:     Preds: b1
// CHECK-NEXT:     Succs: b5
// CHECK-NEXT:   b3:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b4,b5))
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b4 b5
// CHECK-NEXT:   b4:
// CHECK-NEXT:     %i1 = DeclRef(decl=foo)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b4.i2)
// CHECK-NEXT:     %i4 = DeclRef(decl=y)
// CHECK-NEXT:     %i5 = Load(lvalue=%b4.i4)
// CHECK-NEXT:     %i6 = Tuple(%b4.i3,%b4.i5)
// CHECK-NEXT:     %i7 = Call(fn=%b4.i1,args=(%b4.i3 %b4.i5))
// CHECK-NEXT:     %i8 = br b5
// CHECK-NEXT:     Preds: b3
// CHECK-NEXT:     Succs: b5
// CHECK-NEXT:   b5:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b5.i2)
// CHECK-NEXT:     %i4 = Tuple(%b5.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b5.i1,args=(%b5.i3))
// CHECK-NEXT:     %i6 = Return
// CHECK-NEXT:     Preds: b2 b1 b4 b3
// CHECK-NEXT:     Succs:

func nested_if_merge_noret(x:Int, y:Bool, z:Bool) {
  if (y) {
    if (z) {
      bar(x);
    }
  }
  else {
    if (z) {
      foo(x, y);
    }
  }
}

// CHECK: func_decl nested_if_merge_noret
// CHECK-NEXT:   b0:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b1,b3))
// CHECK-NEXT:     Preds:
// CHECK-NEXT:     Succs: b1 b3
// CHECK-NEXT:   b1:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b2,b5))
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b2 b5
// CHECK-NEXT:   b2:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b2.i2)
// CHECK-NEXT:     %i4 = Tuple(%b2.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b2.i1,args=(%b2.i3))
// CHECK-NEXT:     %i6 = br b5
// CHECK-NEXT:     Preds: b1
// CHECK-NEXT:     Succs: b5
// CHECK-NEXT:   b3:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b4,b5))
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b4 b5
// CHECK-NEXT:   b4:
// CHECK-NEXT:     %i1 = DeclRef(decl=foo)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b4.i2)
// CHECK-NEXT:     %i4 = DeclRef(decl=y)
// CHECK-NEXT:     %i5 = Load(lvalue=%b4.i4)
// CHECK-NEXT:     %i6 = Tuple(%b4.i3,%b4.i5)
// CHECK-NEXT:     %i7 = Call(fn=%b4.i1,args=(%b4.i3 %b4.i5))
// CHECK-NEXT:     %i8 = br b5
// CHECK-NEXT:     Preds: b3
// CHECK-NEXT:     Succs: b5
// CHECK-NEXT:   b5:
// CHECK-NEXT:     %i1 = Return
// CHECK-NEXT:     Preds: b2 b1 b4 b3
// CHECK-NEXT:     Succs:

func nested_if_merge_ret(x:Int, y:Bool, z:Bool) -> Int {
  if (y) {
    if (z) {
      bar(x);
    }
    return 1;
  }
  else {
    if (z) {
      foo(x, y);
    }
  }
  return 2;
}

// CHECK: func_decl nested_if_merge_ret
// CHECK-NEXT:   b0:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b1,b4))
// CHECK-NEXT:     Preds:
// CHECK-NEXT:     Succs: b1 b4
// CHECK-NEXT:   b1:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b2,b3))
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b2 b3
// CHECK-NEXT:   b2:
// CHECK-NEXT:     %i1 = DeclRef(decl=bar)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b2.i2)
// CHECK-NEXT:     %i4 = Tuple(%b2.i3)
// CHECK-NEXT:     %i5 = Call(fn=%b2.i1,args=(%b2.i3))
// CHECK-NEXT:     %i6 = br b3
// CHECK-NEXT:     Preds: b1
// CHECK-NEXT:     Succs: b3
// CHECK-NEXT:   b3:
// CHECK-NEXT:     %i1 = DeclRef(decl=convertFromIntegerLiteral)
// CHECK-NEXT:     %i2 = TypeOf(type=metatype<Int64>)
// CHECK-NEXT:     %i3 = ThisApply(fn=%b3.i1,arg=%b3.i2)
// CHECK-NEXT:     %i4 = Integer(val=1,width=64)
// CHECK-NEXT:     %i5 = Call(fn=%b3.i3,args=(%b3.i4))
// CHECK-NEXT:     %i6 = Return(%b3.i5)
// CHECK-NEXT:     Preds: b2 b1
// CHECK-NEXT:     Succs:
// CHECK-NEXT:   b4:
// CHECK-NEXT:     %i1 = cond_br(cond=?,branches=(b5,b6))
// CHECK-NEXT:     Preds: b0
// CHECK-NEXT:     Succs: b5 b6
// CHECK-NEXT:   b5:
// CHECK-NEXT:     %i1 = DeclRef(decl=foo)
// CHECK-NEXT:     %i2 = DeclRef(decl=x)
// CHECK-NEXT:     %i3 = Load(lvalue=%b5.i2)
// CHECK-NEXT:     %i4 = DeclRef(decl=y)
// CHECK-NEXT:     %i5 = Load(lvalue=%b5.i4)
// CHECK-NEXT:     %i6 = Tuple(%b5.i3,%b5.i5)
// CHECK-NEXT:     %i7 = Call(fn=%b5.i1,args=(%b5.i3 %b5.i5))
// CHECK-NEXT:     %i8 = br b6
// CHECK-NEXT:     Preds: b4
// CHECK-NEXT:     Succs: b6
// CHECK-NEXT:   b6:
// CHECK-NEXT:     %i1 = DeclRef(decl=convertFromIntegerLiteral)
// CHECK-NEXT:     %i2 = TypeOf(type=metatype<Int64>)
// CHECK-NEXT:     %i3 = ThisApply(fn=%b6.i1,arg=%b6.i2)
// CHECK-NEXT:     %i4 = Integer(val=2,width=64)
// CHECK-NEXT:     %i5 = Call(fn=%b6.i3,args=(%b6.i4))
// CHECK-NEXT:     %i6 = Return(%b6.i5)
// CHECK-NEXT:     Preds: b5 b4
// CHECK-NEXT:     Succs:

