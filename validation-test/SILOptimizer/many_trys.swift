// The compiler should finish in less than 5 seconds. To give some slack, specify a timeout of 30 seconds.
// If the compiler needs more than that, there is probably a real problem.
// So please don't just increase the timeout in case this fails.

// RUN: %{python} %S/../../test/Inputs/timeout.py 30 %target-swift-frontend -O -parse-as-library -sil-verify-none %s -emit-sil | %FileCheck %s

// REQUIRES: tools-release,no_asan

// For some reason this test times out sometimes when building for iOS: rdar://106375480
// UNSUPPORTED: OS=ios

public var gg = false

enum SomeError : Error {
  case E
}

public class X {
  @inline(never)
  init() throws {
    if gg {
      throw SomeError.E
    }
  }
}

// CHECK-LABEL: testit
public func testit(_ i: Int) throws -> (Int, X) {
  let arr: [(Int, X)] = [
      (0, try X()),
      (1, try X()),
      (2, try X()),
      (3, try X()),
      (4, try X()),
      (5, try X()),
      (6, try X()),
      (7, try X()),
      (8, try X()),
      (9, try X()),
      (10, try X()),
      (11, try X()),
      (12, try X()),
      (13, try X()),
      (14, try X()),
      (15, try X()),
      (16, try X()),
      (17, try X()),
      (18, try X()),
      (19, try X()),
      (20, try X()),
      (21, try X()),
      (22, try X()),
      (23, try X()),
      (24, try X()),
      (25, try X()),
      (26, try X()),
      (27, try X()),
      (28, try X()),
      (29, try X()),
      (30, try X()),
      (31, try X()),
      (32, try X()),
      (33, try X()),
      (34, try X()),
      (35, try X()),
      (36, try X()),
      (37, try X()),
      (38, try X()),
      (39, try X()),
      (40, try X()),
      (41, try X()),
      (42, try X()),
      (43, try X()),
      (44, try X()),
      (45, try X()),
      (46, try X()),
      (47, try X()),
      (48, try X()),
      (49, try X()),
      (50, try X()),
      (51, try X()),
      (52, try X()),
      (53, try X()),
      (54, try X()),
      (55, try X()),
      (56, try X()),
      (57, try X()),
      (58, try X()),
      (59, try X()),
      (60, try X()),
      (61, try X()),
      (62, try X()),
      (63, try X()),
      (64, try X()),
      (65, try X()),
      (66, try X()),
      (67, try X()),
      (68, try X()),
      (69, try X()),
      (70, try X()),
      (71, try X()),
      (72, try X()),
      (73, try X()),
      (74, try X()),
      (75, try X()),
      (76, try X()),
      (77, try X()),
      (78, try X()),
      (79, try X()),
      (80, try X()),
      (81, try X()),
      (82, try X()),
      (83, try X()),
      (84, try X()),
      (85, try X()),
      (86, try X()),
      (87, try X()),
      (88, try X()),
      (89, try X()),
      (90, try X()),
      (91, try X()),
      (92, try X()),
      (93, try X()),
      (94, try X()),
      (95, try X()),
      (96, try X()),
      (97, try X()),
      (98, try X()),
      (99, try X()),
      (100, try X()),
      (101, try X()),
      (102, try X()),
      (103, try X()),
      (104, try X()),
      (105, try X()),
      (106, try X()),
      (107, try X()),
      (108, try X()),
      (109, try X()),
      (110, try X()),
      (111, try X()),
      (112, try X()),
      (113, try X()),
      (114, try X()),
      (115, try X()),
      (116, try X()),
      (117, try X()),
      (118, try X()),
      (119, try X()),
      (120, try X()),
      (121, try X()),
      (122, try X()),
      (123, try X()),
      (124, try X()),
      (125, try X()),
      (126, try X()),
      (127, try X()),
      (128, try X()),
    ]

  return arr[i]
} 

