// RUN: %target-swift-ide-test -print-comments -source-filename %s | FileCheck %s

///
func briefLine1() {}

/// Aaa.
func briefLine2() {}

/// Aaa.
/// Bbb.
func briefLine3() {}

/// Aaa.
///
/// Bbb.
func briefLine4() {}

/***/
func briefBlock1() {}

/**
*/
func briefBlock2() {}

/**
  Aaa.
*/
func briefBlock3() {}

/**
  Aaa.
 */
func briefBlock4() {}

/**
  Aaa.
  Bbb.
 */
func briefBlock5() {}

/**
  Aaa.

  Bbb.
 */
func briefBlock6() {}

/** Aaa.
 * Bbb.
 */
func briefBlock7() {}

/** Aaa.
 * Bbb.
 * Ccc.
 */
func briefBlock8() {}

/** Aaa.
 * Bbb.
 Ccc.
 */
func briefBlock9() {}

/**
 * Aaa.
 */
func briefBlockWithASCIIArt1() {}

/**
 *
 */
func briefBlockWithASCIIArt2() {}

/**
 * Aaa.
 * Bbb.
 */
func briefBlockWithASCIIArt3() {}

/**
 *Aaa.
 */
func briefBlockWithASCIIArt4() {}

/**
 * Aaa.
Bbb.
 *Ccc.
 */
func briefBlockWithASCIIArt5() {}

/**
 * Aaa.

 * Bbb.
 */
func briefBlockWithASCIIArt6() {}


/// Aaa.
/** Bbb. */
func briefMixed1() {}

/// Aaa.
/**
  Bbb.
 */
func briefMixed2() {}

/**
  Aaa.
*/
/**
  Bbb.
*/
func briefMixed3() {}

struct Indentation {
  /**
   * Aaa.
   */
  func briefBlockWithASCIIArt1() {}
}


// CHECK: Func/briefLine1 {{.*}} BriefComment=none
// CHECK-NEXT: Func/briefLine2 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefLine3 {{.*}} BriefComment=[Aaa. Bbb.]
// CHECK-NEXT: Func/briefLine4 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlock1 {{.*}} BriefComment=none
// CHECK-NEXT: Func/briefBlock2 {{.*}} BriefComment=none
// CHECK-NEXT: Func/briefBlock3 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlock4 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlock5 {{.*}} BriefComment=[Aaa. Bbb.]
// CHECK-NEXT: Func/briefBlock6 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlock7 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlock8 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlock9 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlockWithASCIIArt1 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefBlockWithASCIIArt2 {{.*}} BriefComment=none
// CHECK-NEXT: Func/briefBlockWithASCIIArt3 {{.*}} BriefComment=[Aaa. Bbb.]
// CHECK-NEXT: Func/briefBlockWithASCIIArt4 {{.*}} BriefComment=[*Aaa.]
// CHECK-NEXT: Func/briefBlockWithASCIIArt5 {{.*}} BriefComment=[Aaa. Bbb. *Ccc.]
// CHECK-NEXT: Func/briefBlockWithASCIIArt6 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefMixed1 {{.*}} BriefComment=[Aaa. Bbb.]
// CHECK-NEXT: Func/briefMixed2 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Func/briefMixed3 {{.*}} BriefComment=[Aaa.]
// CHECK-NEXT: Struct/Indentation RawComment=none
// CHECK-NEXT: Func/Indentation.briefBlockWithASCIIArt1 {{.*}} BriefComment=[Aaa.]

