// RUN: %target-swift-frontend -primary-file %s -emit-ir > %t.ir
// RUN: %FileCheck %s --input-file=%t.ir

// This test checks that `@.str` string constants have names accurately
// reflecting their contents. Other tests rely on this behavior so they can
// simply test the names of constants and assume that their values match.

func strings() {
  // String literal containing only identifier-safe characters.
  blackHole("01234567890123456789")
  // CHECK-DAG: @.str.20.01234567890123456789 = private unnamed_addr constant [21 x i8] c"01234567890123456789\00"

  // String literal containing spaces and punctuation.
  blackHole("Hello, world! Padding past 16.")
  // CHECK-DAG: @".str.30.Hello, world! Padding past 16." = private unnamed_addr constant [31 x i8] c"Hello, world! Padding past 16.\00"
  
  // String literal containing non-ASCII characters.
  blackHole("🏳️‍⚧️👩‍❤️‍💋‍👩🥹")
  // CHECK-DAG:  @".str.47.\F0\9F\8F\B3\EF\B8\8F\E2\80\8D\E2\9A\A7\EF\B8\8F\F0\9F\91\A9\E2\80\8D\E2\9D\A4\EF\B8\8F\E2\80\8D\F0\9F\92\8B\E2\80\8D\F0\9F\91\A9\F0\9F\A5\B9" = private unnamed_addr constant [48 x i8] c"\F0\9F\8F\B3\EF\B8\8F\E2\80\8D\E2\9A\A7\EF\B8\8F\F0\9F\91\A9\E2\80\8D\E2\9D\A4\EF\B8\8F\E2\80\8D\F0\9F\92\8B\E2\80\8D\F0\9F\91\A9\F0\9F\A5\B9\00"

  // String literal containing nul characters. These require special encoding.
  blackHole("\01\02\03\04\05\06\07\08")
  // CHECK-DAG: @.str.16._1_2_3_4_5_6_7_8.nul8.nul10.nul12.nul14.nul16.nul18.nul20.nul22 = private unnamed_addr constant [17 x i8] c"\001\002\003\004\005\006\007\008\00"
}

@inline(never) func blackHole(_: String) {}
