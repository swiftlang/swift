// REQUIRES: OS=macosx
// REQUIRES: no_asan
// RUN: %empty-directory(%t)
// RUN: touch  %t/f_1_1.swift %t/f_1_2.swift %t/f_1_3.swift %t/f_1_4.swift %t/f_1_5.swift %t/f_1_6.swift %t/f_1_7.swift %t/f_1_8.swift %t/f_1_9.swift %t/f_1_10.swift
// RUN: touch  %t/f_2_1.swift %t/f_2_2.swift %t/f_2_3.swift %t/f_2_4.swift %t/f_2_5.swift %t/f_2_6.swift %t/f_2_7.swift %t/f_2_8.swift %t/f_2_9.swift %t/f_2_10.swift
// RUN: touch  %t/f_3_1.swift %t/f_3_2.swift %t/f_3_3.swift %t/f_3_4.swift %t/f_3_5.swift %t/f_3_6.swift %t/f_3_7.swift %t/f_3_8.swift %t/f_3_9.swift %t/f_3_10.swift
// RUN: touch  %t/f_4_1.swift %t/f_4_2.swift %t/f_4_3.swift %t/f_4_4.swift %t/f_4_5.swift %t/f_4_6.swift %t/f_4_7.swift %t/f_4_8.swift %t/f_4_9.swift %t/f_4_10.swift
// RUN: touch  %t/f_5_1.swift %t/f_5_2.swift %t/f_5_3.swift %t/f_5_4.swift %t/f_5_5.swift %t/f_5_6.swift %t/f_5_7.swift %t/f_5_8.swift %t/f_5_9.swift %t/f_5_10.swift
// RUN: touch  %t/f_6_1.swift %t/f_6_2.swift %t/f_6_3.swift %t/f_6_4.swift %t/f_6_5.swift %t/f_6_6.swift %t/f_6_7.swift %t/f_6_8.swift %t/f_6_9.swift %t/f_6_10.swift
// RUN: touch  %t/f_7_1.swift %t/f_7_2.swift %t/f_7_3.swift %t/f_7_4.swift %t/f_7_5.swift %t/f_7_6.swift %t/f_7_7.swift %t/f_7_8.swift %t/f_7_9.swift %t/f_7_10.swift
// RUN: touch  %t/f_8_1.swift %t/f_8_2.swift %t/f_8_3.swift %t/f_8_4.swift %t/f_8_5.swift %t/f_8_6.swift %t/f_8_7.swift %t/f_8_8.swift %t/f_8_9.swift %t/f_8_10.swift
// RUN: touch  %t/f_9_1.swift %t/f_9_2.swift %t/f_9_3.swift %t/f_9_4.swift %t/f_9_5.swift %t/f_9_6.swift %t/f_9_7.swift %t/f_9_8.swift %t/f_9_9.swift %t/f_9_10.swift
// RUN: touch  %t/f_10_1.swift %t/f_10_2.swift %t/f_10_3.swift %t/f_10_4.swift %t/f_10_5.swift %t/f_10_6.swift %t/f_10_7.swift %t/f_10_8.swift %t/f_10_9.swift %t/f_10_10.swift
// RUN: touch  %t/f_11_1.swift %t/f_11_2.swift %t/f_11_3.swift %t/f_11_4.swift %t/f_11_5.swift %t/f_11_6.swift %t/f_11_7.swift %t/f_11_8.swift %t/f_11_9.swift %t/f_11_10.swift
// RUN: touch  %t/f_12_1.swift %t/f_12_2.swift %t/f_12_3.swift %t/f_12_4.swift %t/f_12_5.swift %t/f_12_6.swift %t/f_12_7.swift %t/f_12_8.swift %t/f_12_9.swift %t/f_12_10.swift
// RUN: touch  %t/f_13_1.swift %t/f_13_2.swift %t/f_13_3.swift %t/f_13_4.swift %t/f_13_5.swift %t/f_13_6.swift %t/f_13_7.swift %t/f_13_8.swift %t/f_13_9.swift %t/f_13_10.swift
// RUN: touch  %t/f_14_1.swift %t/f_14_2.swift %t/f_14_3.swift %t/f_14_4.swift %t/f_14_5.swift %t/f_14_6.swift %t/f_14_7.swift %t/f_14_8.swift %t/f_14_9.swift %t/f_14_10.swift
// RUN: touch  %t/f_15_1.swift %t/f_15_2.swift %t/f_15_3.swift %t/f_15_4.swift %t/f_15_5.swift %t/f_15_6.swift %t/f_15_7.swift %t/f_15_8.swift %t/f_15_9.swift %t/f_15_10.swift
// RUN: touch  %t/f_16_1.swift %t/f_16_2.swift %t/f_16_3.swift %t/f_16_4.swift %t/f_16_5.swift %t/f_16_6.swift %t/f_16_7.swift %t/f_16_8.swift %t/f_16_9.swift %t/f_16_10.swift
// RUN: touch  %t/f_17_1.swift %t/f_17_2.swift %t/f_17_3.swift %t/f_17_4.swift %t/f_17_5.swift %t/f_17_6.swift %t/f_17_7.swift %t/f_17_8.swift %t/f_17_9.swift %t/f_17_10.swift
// RUN: touch  %t/f_18_1.swift %t/f_18_2.swift %t/f_18_3.swift %t/f_18_4.swift %t/f_18_5.swift %t/f_18_6.swift %t/f_18_7.swift %t/f_18_8.swift %t/f_18_9.swift %t/f_18_10.swift
// RUN: touch  %t/f_19_1.swift %t/f_19_2.swift %t/f_19_3.swift %t/f_19_4.swift %t/f_19_5.swift %t/f_19_6.swift %t/f_19_7.swift %t/f_19_8.swift %t/f_19_9.swift %t/f_19_10.swift
// RUN: touch  %t/f_20_1.swift %t/f_20_2.swift %t/f_20_3.swift %t/f_20_4.swift %t/f_20_5.swift %t/f_20_6.swift %t/f_20_7.swift %t/f_20_8.swift %t/f_20_9.swift %t/f_20_10.swift
// RUN: touch  %t/f_21_1.swift %t/f_21_2.swift %t/f_21_3.swift %t/f_21_4.swift %t/f_21_5.swift %t/f_21_6.swift %t/f_21_7.swift %t/f_21_8.swift %t/f_21_9.swift %t/f_21_10.swift
// RUN: touch  %t/f_22_1.swift %t/f_22_2.swift %t/f_22_3.swift %t/f_22_4.swift %t/f_22_5.swift %t/f_22_6.swift %t/f_22_7.swift %t/f_22_8.swift %t/f_22_9.swift %t/f_22_10.swift
// RUN: touch  %t/f_23_1.swift %t/f_23_2.swift %t/f_23_3.swift %t/f_23_4.swift %t/f_23_5.swift %t/f_23_6.swift %t/f_23_7.swift %t/f_23_8.swift %t/f_23_9.swift %t/f_23_10.swift
// RUN: touch  %t/f_24_1.swift %t/f_24_2.swift %t/f_24_3.swift %t/f_24_4.swift %t/f_24_5.swift %t/f_24_6.swift %t/f_24_7.swift %t/f_24_8.swift %t/f_24_9.swift %t/f_24_10.swift
// RUN: touch  %t/f_25_1.swift %t/f_25_2.swift %t/f_25_3.swift %t/f_25_4.swift %t/f_25_5.swift %t/f_25_6.swift %t/f_25_7.swift %t/f_25_8.swift %t/f_25_9.swift %t/f_25_10.swift
// RUN: touch  %t/f_26_1.swift %t/f_26_2.swift %t/f_26_3.swift %t/f_26_4.swift %t/f_26_5.swift %t/f_26_6.swift %t/f_26_7.swift %t/f_26_8.swift %t/f_26_9.swift %t/f_26_10.swift
// RUN: touch  %t/f_27_1.swift %t/f_27_2.swift %t/f_27_3.swift %t/f_27_4.swift %t/f_27_5.swift %t/f_27_6.swift %t/f_27_7.swift %t/f_27_8.swift %t/f_27_9.swift %t/f_27_10.swift
// RUN: touch  %t/f_28_1.swift %t/f_28_2.swift %t/f_28_3.swift %t/f_28_4.swift %t/f_28_5.swift %t/f_28_6.swift %t/f_28_7.swift %t/f_28_8.swift %t/f_28_9.swift %t/f_28_10.swift
// RUN: touch  %t/f_29_1.swift %t/f_29_2.swift %t/f_29_3.swift %t/f_29_4.swift %t/f_29_5.swift %t/f_29_6.swift %t/f_29_7.swift %t/f_29_8.swift %t/f_29_9.swift %t/f_29_10.swift
// RUN: touch  %t/f_30_1.swift %t/f_30_2.swift %t/f_30_3.swift %t/f_30_4.swift %t/f_30_5.swift %t/f_30_6.swift %t/f_30_7.swift %t/f_30_8.swift %t/f_30_9.swift %t/f_30_10.swift
// RUN: touch  %t/f_31_1.swift %t/f_31_2.swift %t/f_31_3.swift %t/f_31_4.swift %t/f_31_5.swift %t/f_31_6.swift %t/f_31_7.swift %t/f_31_8.swift %t/f_31_9.swift %t/f_31_10.swift
// RUN: touch  %t/f_32_1.swift %t/f_32_2.swift %t/f_32_3.swift %t/f_32_4.swift %t/f_32_5.swift %t/f_32_6.swift %t/f_32_7.swift %t/f_32_8.swift %t/f_32_9.swift %t/f_32_10.swift
// RUN: touch  %t/f_33_1.swift %t/f_33_2.swift %t/f_33_3.swift %t/f_33_4.swift %t/f_33_5.swift %t/f_33_6.swift %t/f_33_7.swift %t/f_33_8.swift %t/f_33_9.swift %t/f_33_10.swift
// RUN: touch  %t/f_34_1.swift %t/f_34_2.swift %t/f_34_3.swift %t/f_34_4.swift %t/f_34_5.swift %t/f_34_6.swift %t/f_34_7.swift %t/f_34_8.swift %t/f_34_9.swift %t/f_34_10.swift
// RUN: touch  %t/f_35_1.swift %t/f_35_2.swift %t/f_35_3.swift %t/f_35_4.swift %t/f_35_5.swift %t/f_35_6.swift %t/f_35_7.swift %t/f_35_8.swift %t/f_35_9.swift %t/f_35_10.swift
// RUN: touch  %t/f_36_1.swift %t/f_36_2.swift %t/f_36_3.swift %t/f_36_4.swift %t/f_36_5.swift %t/f_36_6.swift %t/f_36_7.swift %t/f_36_8.swift %t/f_36_9.swift %t/f_36_10.swift
// RUN: touch  %t/f_37_1.swift %t/f_37_2.swift %t/f_37_3.swift %t/f_37_4.swift %t/f_37_5.swift %t/f_37_6.swift %t/f_37_7.swift %t/f_37_8.swift %t/f_37_9.swift %t/f_37_10.swift
// RUN: touch  %t/f_38_1.swift %t/f_38_2.swift %t/f_38_3.swift %t/f_38_4.swift %t/f_38_5.swift %t/f_38_6.swift %t/f_38_7.swift %t/f_38_8.swift %t/f_38_9.swift %t/f_38_10.swift
// RUN: touch  %t/f_39_1.swift %t/f_39_2.swift %t/f_39_3.swift %t/f_39_4.swift %t/f_39_5.swift %t/f_39_6.swift %t/f_39_7.swift %t/f_39_8.swift %t/f_39_9.swift %t/f_39_10.swift
// RUN: touch  %t/f_40_1.swift %t/f_40_2.swift %t/f_40_3.swift %t/f_40_4.swift %t/f_40_5.swift %t/f_40_6.swift %t/f_40_7.swift %t/f_40_8.swift %t/f_40_9.swift %t/f_40_10.swift
// RUN: touch  %t/f_41_1.swift %t/f_41_2.swift %t/f_41_3.swift %t/f_41_4.swift %t/f_41_5.swift %t/f_41_6.swift %t/f_41_7.swift %t/f_41_8.swift %t/f_41_9.swift %t/f_41_10.swift
// RUN: touch  %t/f_42_1.swift %t/f_42_2.swift %t/f_42_3.swift %t/f_42_4.swift %t/f_42_5.swift %t/f_42_6.swift %t/f_42_7.swift %t/f_42_8.swift %t/f_42_9.swift %t/f_42_10.swift
// RUN: touch  %t/f_43_1.swift %t/f_43_2.swift %t/f_43_3.swift %t/f_43_4.swift %t/f_43_5.swift %t/f_43_6.swift %t/f_43_7.swift %t/f_43_8.swift %t/f_43_9.swift %t/f_43_10.swift
// RUN: touch  %t/f_44_1.swift %t/f_44_2.swift %t/f_44_3.swift %t/f_44_4.swift %t/f_44_5.swift %t/f_44_6.swift %t/f_44_7.swift %t/f_44_8.swift %t/f_44_9.swift %t/f_44_10.swift
// RUN: touch  %t/f_45_1.swift %t/f_45_2.swift %t/f_45_3.swift %t/f_45_4.swift %t/f_45_5.swift %t/f_45_6.swift %t/f_45_7.swift %t/f_45_8.swift %t/f_45_9.swift %t/f_45_10.swift
// RUN: touch  %t/f_46_1.swift %t/f_46_2.swift %t/f_46_3.swift %t/f_46_4.swift %t/f_46_5.swift %t/f_46_6.swift %t/f_46_7.swift %t/f_46_8.swift %t/f_46_9.swift %t/f_46_10.swift
// RUN: touch  %t/f_47_1.swift %t/f_47_2.swift %t/f_47_3.swift %t/f_47_4.swift %t/f_47_5.swift %t/f_47_6.swift %t/f_47_7.swift %t/f_47_8.swift %t/f_47_9.swift %t/f_47_10.swift
// RUN: touch  %t/f_48_1.swift %t/f_48_2.swift %t/f_48_3.swift %t/f_48_4.swift %t/f_48_5.swift %t/f_48_6.swift %t/f_48_7.swift %t/f_48_8.swift %t/f_48_9.swift %t/f_48_10.swift
// RUN: touch  %t/f_49_1.swift %t/f_49_2.swift %t/f_49_3.swift %t/f_49_4.swift %t/f_49_5.swift %t/f_49_6.swift %t/f_49_7.swift %t/f_49_8.swift %t/f_49_9.swift %t/f_49_10.swift
// RUN: touch  %t/f_50_1.swift %t/f_50_2.swift %t/f_50_3.swift %t/f_50_4.swift %t/f_50_5.swift %t/f_50_6.swift %t/f_50_7.swift %t/f_50_8.swift %t/f_50_9.swift %t/f_50_10.swift
// RUN: touch  %t/f_51_1.swift %t/f_51_2.swift %t/f_51_3.swift %t/f_51_4.swift %t/f_51_5.swift %t/f_51_6.swift %t/f_51_7.swift %t/f_51_8.swift %t/f_51_9.swift %t/f_51_10.swift
// RUN: touch  %t/f_52_1.swift %t/f_52_2.swift %t/f_52_3.swift %t/f_52_4.swift %t/f_52_5.swift %t/f_52_6.swift %t/f_52_7.swift %t/f_52_8.swift %t/f_52_9.swift %t/f_52_10.swift
// RUN: touch  %t/f_53_1.swift %t/f_53_2.swift %t/f_53_3.swift %t/f_53_4.swift %t/f_53_5.swift %t/f_53_6.swift %t/f_53_7.swift %t/f_53_8.swift %t/f_53_9.swift %t/f_53_10.swift
// RUN: touch  %t/f_54_1.swift %t/f_54_2.swift %t/f_54_3.swift %t/f_54_4.swift %t/f_54_5.swift %t/f_54_6.swift %t/f_54_7.swift %t/f_54_8.swift %t/f_54_9.swift %t/f_54_10.swift
// RUN: touch  %t/f_55_1.swift %t/f_55_2.swift %t/f_55_3.swift %t/f_55_4.swift %t/f_55_5.swift %t/f_55_6.swift %t/f_55_7.swift %t/f_55_8.swift %t/f_55_9.swift %t/f_55_10.swift
// RUN: touch  %t/f_56_1.swift %t/f_56_2.swift %t/f_56_3.swift %t/f_56_4.swift %t/f_56_5.swift %t/f_56_6.swift %t/f_56_7.swift %t/f_56_8.swift %t/f_56_9.swift %t/f_56_10.swift
// RUN: touch  %t/f_57_1.swift %t/f_57_2.swift %t/f_57_3.swift %t/f_57_4.swift %t/f_57_5.swift %t/f_57_6.swift %t/f_57_7.swift %t/f_57_8.swift %t/f_57_9.swift %t/f_57_10.swift
// RUN: touch  %t/f_58_1.swift %t/f_58_2.swift %t/f_58_3.swift %t/f_58_4.swift %t/f_58_5.swift %t/f_58_6.swift %t/f_58_7.swift %t/f_58_8.swift %t/f_58_9.swift %t/f_58_10.swift
// RUN: touch  %t/f_59_1.swift %t/f_59_2.swift %t/f_59_3.swift %t/f_59_4.swift %t/f_59_5.swift %t/f_59_6.swift %t/f_59_7.swift %t/f_59_8.swift %t/f_59_9.swift %t/f_59_10.swift
// RUN: touch  %t/f_60_1.swift %t/f_60_2.swift %t/f_60_3.swift %t/f_60_4.swift %t/f_60_5.swift %t/f_60_6.swift %t/f_60_7.swift %t/f_60_8.swift %t/f_60_9.swift %t/f_60_10.swift
// RUN: touch  %t/f_61_1.swift %t/f_61_2.swift %t/f_61_3.swift %t/f_61_4.swift %t/f_61_5.swift %t/f_61_6.swift %t/f_61_7.swift %t/f_61_8.swift %t/f_61_9.swift %t/f_61_10.swift
// RUN: touch  %t/f_62_1.swift %t/f_62_2.swift %t/f_62_3.swift %t/f_62_4.swift %t/f_62_5.swift %t/f_62_6.swift %t/f_62_7.swift %t/f_62_8.swift %t/f_62_9.swift %t/f_62_10.swift
// RUN: touch  %t/f_63_1.swift %t/f_63_2.swift %t/f_63_3.swift %t/f_63_4.swift %t/f_63_5.swift %t/f_63_6.swift %t/f_63_7.swift %t/f_63_8.swift %t/f_63_9.swift %t/f_63_10.swift
// RUN: touch  %t/f_64_1.swift %t/f_64_2.swift %t/f_64_3.swift %t/f_64_4.swift %t/f_64_5.swift %t/f_64_6.swift %t/f_64_7.swift %t/f_64_8.swift %t/f_64_9.swift %t/f_64_10.swift
// RUN: touch  %t/f_65_1.swift %t/f_65_2.swift %t/f_65_3.swift %t/f_65_4.swift %t/f_65_5.swift %t/f_65_6.swift %t/f_65_7.swift %t/f_65_8.swift %t/f_65_9.swift %t/f_65_10.swift
// RUN: touch  %t/f_66_1.swift %t/f_66_2.swift %t/f_66_3.swift %t/f_66_4.swift %t/f_66_5.swift %t/f_66_6.swift %t/f_66_7.swift %t/f_66_8.swift %t/f_66_9.swift %t/f_66_10.swift
// RUN: touch  %t/f_67_1.swift %t/f_67_2.swift %t/f_67_3.swift %t/f_67_4.swift %t/f_67_5.swift %t/f_67_6.swift %t/f_67_7.swift %t/f_67_8.swift %t/f_67_9.swift %t/f_67_10.swift
// RUN: touch  %t/f_68_1.swift %t/f_68_2.swift %t/f_68_3.swift %t/f_68_4.swift %t/f_68_5.swift %t/f_68_6.swift %t/f_68_7.swift %t/f_68_8.swift %t/f_68_9.swift %t/f_68_10.swift
// RUN: touch  %t/f_69_1.swift %t/f_69_2.swift %t/f_69_3.swift %t/f_69_4.swift %t/f_69_5.swift %t/f_69_6.swift %t/f_69_7.swift %t/f_69_8.swift %t/f_69_9.swift %t/f_69_10.swift
// RUN: touch  %t/f_70_1.swift %t/f_70_2.swift %t/f_70_3.swift %t/f_70_4.swift %t/f_70_5.swift %t/f_70_6.swift %t/f_70_7.swift %t/f_70_8.swift %t/f_70_9.swift %t/f_70_10.swift
// RUN: touch  %t/f_71_1.swift %t/f_71_2.swift %t/f_71_3.swift %t/f_71_4.swift %t/f_71_5.swift %t/f_71_6.swift %t/f_71_7.swift %t/f_71_8.swift %t/f_71_9.swift %t/f_71_10.swift
// RUN: touch  %t/f_72_1.swift %t/f_72_2.swift %t/f_72_3.swift %t/f_72_4.swift %t/f_72_5.swift %t/f_72_6.swift %t/f_72_7.swift %t/f_72_8.swift %t/f_72_9.swift %t/f_72_10.swift
// RUN: touch  %t/f_73_1.swift %t/f_73_2.swift %t/f_73_3.swift %t/f_73_4.swift %t/f_73_5.swift %t/f_73_6.swift %t/f_73_7.swift %t/f_73_8.swift %t/f_73_9.swift %t/f_73_10.swift
// RUN: touch  %t/f_74_1.swift %t/f_74_2.swift %t/f_74_3.swift %t/f_74_4.swift %t/f_74_5.swift %t/f_74_6.swift %t/f_74_7.swift %t/f_74_8.swift %t/f_74_9.swift %t/f_74_10.swift
// RUN: touch  %t/f_75_1.swift %t/f_75_2.swift %t/f_75_3.swift %t/f_75_4.swift %t/f_75_5.swift %t/f_75_6.swift %t/f_75_7.swift %t/f_75_8.swift %t/f_75_9.swift %t/f_75_10.swift
// RUN: touch  %t/f_76_1.swift %t/f_76_2.swift %t/f_76_3.swift %t/f_76_4.swift %t/f_76_5.swift %t/f_76_6.swift %t/f_76_7.swift %t/f_76_8.swift %t/f_76_9.swift %t/f_76_10.swift
// RUN: touch  %t/f_77_1.swift %t/f_77_2.swift %t/f_77_3.swift %t/f_77_4.swift %t/f_77_5.swift %t/f_77_6.swift %t/f_77_7.swift %t/f_77_8.swift %t/f_77_9.swift %t/f_77_10.swift
// RUN: touch  %t/f_78_1.swift %t/f_78_2.swift %t/f_78_3.swift %t/f_78_4.swift %t/f_78_5.swift %t/f_78_6.swift %t/f_78_7.swift %t/f_78_8.swift %t/f_78_9.swift %t/f_78_10.swift
// RUN: touch  %t/f_79_1.swift %t/f_79_2.swift %t/f_79_3.swift %t/f_79_4.swift %t/f_79_5.swift %t/f_79_6.swift %t/f_79_7.swift %t/f_79_8.swift %t/f_79_9.swift %t/f_79_10.swift
// RUN: touch  %t/f_80_1.swift %t/f_80_2.swift %t/f_80_3.swift %t/f_80_4.swift %t/f_80_5.swift %t/f_80_6.swift %t/f_80_7.swift %t/f_80_8.swift %t/f_80_9.swift %t/f_80_10.swift
// RUN: touch  %t/f_81_1.swift %t/f_81_2.swift %t/f_81_3.swift %t/f_81_4.swift %t/f_81_5.swift %t/f_81_6.swift %t/f_81_7.swift %t/f_81_8.swift %t/f_81_9.swift %t/f_81_10.swift
// RUN: touch  %t/f_82_1.swift %t/f_82_2.swift %t/f_82_3.swift %t/f_82_4.swift %t/f_82_5.swift %t/f_82_6.swift %t/f_82_7.swift %t/f_82_8.swift %t/f_82_9.swift %t/f_82_10.swift
// RUN: touch  %t/f_83_1.swift %t/f_83_2.swift %t/f_83_3.swift %t/f_83_4.swift %t/f_83_5.swift %t/f_83_6.swift %t/f_83_7.swift %t/f_83_8.swift %t/f_83_9.swift %t/f_83_10.swift
// RUN: touch  %t/f_84_1.swift %t/f_84_2.swift %t/f_84_3.swift %t/f_84_4.swift %t/f_84_5.swift %t/f_84_6.swift %t/f_84_7.swift %t/f_84_8.swift %t/f_84_9.swift %t/f_84_10.swift
// RUN: touch  %t/f_85_1.swift %t/f_85_2.swift %t/f_85_3.swift %t/f_85_4.swift %t/f_85_5.swift %t/f_85_6.swift %t/f_85_7.swift %t/f_85_8.swift %t/f_85_9.swift %t/f_85_10.swift
// RUN: touch  %t/f_86_1.swift %t/f_86_2.swift %t/f_86_3.swift %t/f_86_4.swift %t/f_86_5.swift %t/f_86_6.swift %t/f_86_7.swift %t/f_86_8.swift %t/f_86_9.swift %t/f_86_10.swift
// RUN: touch  %t/f_87_1.swift %t/f_87_2.swift %t/f_87_3.swift %t/f_87_4.swift %t/f_87_5.swift %t/f_87_6.swift %t/f_87_7.swift %t/f_87_8.swift %t/f_87_9.swift %t/f_87_10.swift
// RUN: touch  %t/f_88_1.swift %t/f_88_2.swift %t/f_88_3.swift %t/f_88_4.swift %t/f_88_5.swift %t/f_88_6.swift %t/f_88_7.swift %t/f_88_8.swift %t/f_88_9.swift %t/f_88_10.swift
// RUN: touch  %t/f_89_1.swift %t/f_89_2.swift %t/f_89_3.swift %t/f_89_4.swift %t/f_89_5.swift %t/f_89_6.swift %t/f_89_7.swift %t/f_89_8.swift %t/f_89_9.swift %t/f_89_10.swift
// RUN: touch  %t/f_90_1.swift %t/f_90_2.swift %t/f_90_3.swift %t/f_90_4.swift %t/f_90_5.swift %t/f_90_6.swift %t/f_90_7.swift %t/f_90_8.swift %t/f_90_9.swift %t/f_90_10.swift
// RUN: touch  %t/f_91_1.swift %t/f_91_2.swift %t/f_91_3.swift %t/f_91_4.swift %t/f_91_5.swift %t/f_91_6.swift %t/f_91_7.swift %t/f_91_8.swift %t/f_91_9.swift %t/f_91_10.swift
// RUN: touch  %t/f_92_1.swift %t/f_92_2.swift %t/f_92_3.swift %t/f_92_4.swift %t/f_92_5.swift %t/f_92_6.swift %t/f_92_7.swift %t/f_92_8.swift %t/f_92_9.swift %t/f_92_10.swift
// RUN: touch  %t/f_93_1.swift %t/f_93_2.swift %t/f_93_3.swift %t/f_93_4.swift %t/f_93_5.swift %t/f_93_6.swift %t/f_93_7.swift %t/f_93_8.swift %t/f_93_9.swift %t/f_93_10.swift
// RUN: touch  %t/f_94_1.swift %t/f_94_2.swift %t/f_94_3.swift %t/f_94_4.swift %t/f_94_5.swift %t/f_94_6.swift %t/f_94_7.swift %t/f_94_8.swift %t/f_94_9.swift %t/f_94_10.swift
// RUN: touch  %t/f_95_1.swift %t/f_95_2.swift %t/f_95_3.swift %t/f_95_4.swift %t/f_95_5.swift %t/f_95_6.swift %t/f_95_7.swift %t/f_95_8.swift %t/f_95_9.swift %t/f_95_10.swift
// RUN: touch  %t/f_96_1.swift %t/f_96_2.swift %t/f_96_3.swift %t/f_96_4.swift %t/f_96_5.swift %t/f_96_6.swift %t/f_96_7.swift %t/f_96_8.swift %t/f_96_9.swift %t/f_96_10.swift
// RUN: touch  %t/f_97_1.swift %t/f_97_2.swift %t/f_97_3.swift %t/f_97_4.swift %t/f_97_5.swift %t/f_97_6.swift %t/f_97_7.swift %t/f_97_8.swift %t/f_97_9.swift %t/f_97_10.swift
// RUN: touch  %t/f_98_1.swift %t/f_98_2.swift %t/f_98_3.swift %t/f_98_4.swift %t/f_98_5.swift %t/f_98_6.swift %t/f_98_7.swift %t/f_98_8.swift %t/f_98_9.swift %t/f_98_10.swift
// RUN: touch  %t/f_99_1.swift %t/f_99_2.swift %t/f_99_3.swift %t/f_99_4.swift %t/f_99_5.swift %t/f_99_6.swift %t/f_99_7.swift %t/f_99_8.swift %t/f_99_9.swift %t/f_99_10.swift
// RUN: touch  %t/f_100_1.swift %t/f_100_2.swift %t/f_100_3.swift %t/f_100_4.swift %t/f_100_5.swift %t/f_100_6.swift %t/f_100_7.swift %t/f_100_8.swift %t/f_100_9.swift %t/f_100_10.swift
// RUN: mkdir -p %t/additional/path/elements/often/make/filenames/longer/than/one/might/expect/especially/given/output/directories/deep/within/a/derived/data/folder/of/a/CI/machine/
// Force the repartitioning:
// RUN: %swiftc_driver -driver-show-job-lifecycle -driver-force-one-batch-repartition -v -c -module-name foo -o %t/additional/path/elements/often/make/filenames/longer/than/one/might/expect/especially/given/output/directories/deep/within/a/derived/data/folder/of/a/CI/machine/foo.o -emit-module -serialize-diagnostics -emit-dependencies -j 1 -enable-batch-mode %t/f_*.swift >%t/out.txt 2>&1
// RUN: %FileCheck %s <%t/out.txt
// CHECK-NOT: unable to execute command
// CHECK: Forming into 1 batches
// CHECK: Forming batch job from 1000 constituents
// CHECK: Forming into 2 batches
// CHECK: Forming batch job from 500 constituents
// CHECK: Forming batch job from 500 constituents
//
// Try it without the force; supplementary output file maps should obviate the repartition:
// RUN: %swiftc_driver -driver-show-job-lifecycle -v -c -module-name foo -o %t/additional/path/elements/often/make/filenames/longer/than/one/might/expect/especially/given/output/directories/deep/within/a/derived/data/folder/of/a/CI/machine/foo.o -emit-module -serialize-diagnostics -emit-dependencies -j 1 -enable-batch-mode %t/f_*.swift >%t/out2.txt 2>&1
// RUN: %FileCheck %s <%t/out2.txt -check-prefix=NO-REPARTITION
// CHECK-NOT: unable to execute command
// NO-REPARTITION: Forming into 1 batches
// NO-REPARTITION: Forming batch job from 1000 constituents
// NO-REPARTITION-NOT: Forming into 2 batches
// NO-REPARTITION-NOT: Forming batch job from 500 constituents
// NO-REPARTITION-NOT: Forming batch job from 500 constituents

func thing() {
    print(1)
}
