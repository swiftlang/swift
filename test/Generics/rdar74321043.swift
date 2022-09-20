// RUN: %target-typecheck-verify-swift -requirement-machine-max-rule-count=20000

// This test case used to blow up the GSB due to exponential behavior.

protocol P0: Codable {
  associatedtype A1: P72
  associatedtype A2: P69
  associatedtype A3: P74
  associatedtype A4: P76
  associatedtype A5: P77
  associatedtype A6: P71
  associatedtype A7: P70
  associatedtype A8: P82
  associatedtype A9: P68
  associatedtype A10: P81
  associatedtype A11: P79
}

protocol P1: Codable {
  associatedtype A1: P72
  associatedtype A2: P69
  associatedtype A3: P74
  associatedtype A4: P76
  associatedtype A5: P77
  associatedtype A6: P71
  associatedtype A7: P70
  associatedtype A8: P82
  associatedtype A10: P81
  associatedtype A12: P73
}

protocol P3: Codable {}

protocol P4: Codable {
  associatedtype A13: P5
}

protocol P5: Codable {
  associatedtype A14: P6
}

protocol P6: Codable {
  associatedtype A15: P7
}

protocol P7: Codable {
}

protocol P8: Codable {
  associatedtype A16: P66
}

protocol P9: Codable {
  associatedtype A17: P10
  associatedtype A18: P11
  associatedtype A19: P12
  associatedtype A20: P13
  associatedtype A21: P14
  associatedtype A22: P15
  associatedtype A23: P16
  associatedtype A24: P17
  associatedtype A25: P18
  associatedtype A26: P19
  associatedtype A27: P20
}

protocol P10: Codable {
  associatedtype A28: P67
}

protocol P11: Codable {
}

protocol P12: Codable {
}

protocol P13: Codable {
}

protocol P14: Codable {
}

protocol P15: Codable {
  associatedtype A29: P67
}

protocol P16: Codable {
  associatedtype A30: P36
}

protocol P17: Codable {
}

protocol P18: Codable {
}

protocol P19: Codable {
}

protocol P20: Codable {
  associatedtype A31: P40
}

protocol P21: Codable {
  associatedtype A32: P26
  associatedtype A33: P29
}

protocol P22: Codable {
  associatedtype A34: P32
  associatedtype A35: P31
}

protocol P23: Codable {
  associatedtype A36: P45
  associatedtype A37: P44
  associatedtype A38: P43
  associatedtype A39: P56
  associatedtype A40: P42
  associatedtype A29: P55
  associatedtype A41: P41
  associatedtype A42: P53
  associatedtype A43: P51
  associatedtype A44: P54
}

protocol P24: P90 {
  associatedtype A1: P72
  associatedtype A2: P69
  associatedtype A3: P74
  associatedtype A4: P76
  associatedtype A5: P77
  associatedtype A6: P71
}

protocol P25: P8 {
  associatedtype A45: P3
}

protocol P26: Codable {
}

protocol P27: P8 {
  associatedtype A45: P3
}

protocol P28: P8 {
}

protocol P29: Codable {
  associatedtype A46: P30
}

protocol P30: Codable {
}

protocol P31: Codable {
  associatedtype A34: P32
  associatedtype A47: P28
  associatedtype A48: P27
}

protocol P32: P8 {
  associatedtype A45: P3
}

protocol P33: Codable {
  associatedtype A49: P67
  associatedtype A50: P67
}

protocol P34: Codable {
}

protocol P35: Codable {
}

protocol P36: Codable {
}

protocol P37: Codable {
  associatedtype A51: P38
  associatedtype A52: P39
  associatedtype A53: P34
  associatedtype A54: P35
  associatedtype A55: P33
}

protocol P38: Codable {
}

protocol P39: Codable {
}

protocol P40: Codable {
}

protocol P41: P8 {
}

protocol P42: P8 {
}

protocol P43: Codable {
}

protocol P44: P8 {
}

protocol P45: Codable {
  associatedtype A56: P58
  associatedtype A57: P58
  associatedtype A58: P59
}

protocol P46: P8 {
}

protocol P47: Codable {
}

protocol P48: Codable {
}

protocol P49: Codable {
  associatedtype A45: P3
}

protocol P50: Codable {
}

protocol P51: P8 {
}

protocol P52: P8 {
}

protocol P53: P8 {
}

protocol P54: Codable {
}

protocol P55: P8 {
}

protocol P56: P8 {
}

protocol P57: P8 {
}

protocol P58: P8 {
}

protocol P59: Codable {
}

protocol P60: P84, P85, P86 {
  associatedtype A59: P65
}

protocol P61: P84, P86, P85 {
  associatedtype A59: P65
}

protocol P62: Codable {
}

protocol P63: Codable {
  associatedtype A60: P64
}

protocol P64: Codable {
}

protocol P65: Codable {
  associatedtype A61: P62
  associatedtype A62: P62
}

protocol P66: Codable {
  associatedtype A61: P62
  associatedtype A62: P62
  associatedtype A63: P63
}

protocol P67: Codable {
}

protocol P68: P88 {
  associatedtype A64: P80
}

protocol P69: P87, P21, P91, P4, P22, P85, P37, P0 {
  associatedtype A65: P60
}

protocol P70: P88, P21, P85, P37, P0 {
  associatedtype A66: P57
}

protocol P71: P83 {
  associatedtype A1: P72
  associatedtype A2: P69
  associatedtype A4: P76
  associatedtype A5: P77
  associatedtype A7: P70
  associatedtype A8: P82
  associatedtype A10: P81
}

protocol P72: P88, P21, P91, P22, P85, P1, P37, P4 {
  associatedtype A9: P68
  associatedtype A11: P79
  associatedtype A65: P60
}

protocol P73: P0, P37, P4 {
}

protocol P74: P88, P21, P86, P85, P4, P37, P0 {
  associatedtype A67: P75
}

protocol P75: P23, P22 {
  associatedtype A68: P61
}

protocol P76: P88, P21, P91, P22, P85, P37, P4, P0 {
  associatedtype A69: P61
}

protocol P77: P88, P21, P91, P22, P85, P1, P37, P4 {
  associatedtype A9: P68
  associatedtype A11: P79
  associatedtype A70: P78
}

protocol P78: Codable {
  associatedtype A69: P61
}

protocol P79: P1, P83 {
}

protocol P80: Codable {
}

protocol P81: P88, P23, P37, P22, P4, P8, P0 {
  associatedtype A64: P80
}

protocol P82: P88, P21, P23, P37, P22, P8, P0 {
}

protocol P83: Codable {
}

protocol P84: Codable {
  associatedtype A71: P48
  associatedtype A72: P50
  associatedtype A73: P47
  associatedtype A74: P46
  associatedtype A75: P49
}

protocol P85: Codable {
  associatedtype A76: P25
}

protocol P86: Codable {
  associatedtype A66: P57
  associatedtype A77: P52
}

protocol P87: P83 {
}

protocol P88: P83 {
}

protocol P89: P90 {
}

protocol P90: Codable {
  associatedtype A78: P89
  associatedtype A79: P9
}

protocol P91: P86, P23 {
}

