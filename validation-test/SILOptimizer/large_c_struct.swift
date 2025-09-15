// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The compiler should finish in less than 20 seconds. To give some slack,
// specify a timeout of 3 minutes.
// If the compiler needs more than 3 minutes, there is probably a real problem.
// So please don't just increase the timeout in case this fails.

// RUN: %{python} %S/../../test/Inputs/timeout.py 180 %target-swift-frontend -c -o %t/out.o -primary-file %t/test.swift -import-objc-header %t/bigstruct.h

// REQUIRES: long_test

//--- test.swift

func createTestABigStruct1() -> test_a_big_struct_1 {
    var testABigStruct1 = test_a_big_struct_1()

    testABigStruct1.var1 = 0

    return testABigStruct1
}

func createTestABigStruct2() -> test_a_big_struct_2 {
    var testABigStruct2 = test_a_big_struct_2()

    testABigStruct2.var1 = 0

    return testABigStruct2
}

var testABigStruct1 = createTestABigStruct1()
var testABigStruct2 = createTestABigStruct2()

test_a_print(&testABigStruct1, &testABigStruct2)

//--- bigstruct.h

#define TEST_A_SIZE_BIG 1024
#define TEST_A_SIZE_SMALL 8

typedef struct
{
    int var1;
    int var2;
    int var3;
}
test_a_other_struct_1;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_2;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_3;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_4;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_5;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_6;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_7;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_8;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_9;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_10;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_11;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_12;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_13;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_14;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_15;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_16;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_17;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_18;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_19;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_20;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_21;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_22;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_23;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_24;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_25;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_26;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_27;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_28;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_29;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_30;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_31;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_32;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_33;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_34;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_35;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_36;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_37;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_38;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_39;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_40;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_41;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_42;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_43;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_44;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_45;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_46;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_47;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_48;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_49;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_50;

typedef struct
{
    int var1;
    int var2;
    int var3;
    int var4;
    int var5;
    int var6;
    int var7;
    int var8;
    int var9;
    int var10;
}
test_a_other_struct_51;

typedef struct
{
    int var1;
    int arr1_2[TEST_A_SIZE_BIG];
    int arr1_3[TEST_A_SIZE_BIG];
    int arr1_4[TEST_A_SIZE_BIG];
    int arr1_5[TEST_A_SIZE_BIG];
    int arr1_6[TEST_A_SIZE_BIG];
    int arr1_7[TEST_A_SIZE_BIG];
    int arr1_8[TEST_A_SIZE_BIG];
    int arr1_9[TEST_A_SIZE_BIG];
    int arr1_10[TEST_A_SIZE_BIG];
    int arr1_11[TEST_A_SIZE_BIG];
    int arr1_12[TEST_A_SIZE_BIG];
    int arr1_13[TEST_A_SIZE_BIG];
    int arr1_14[TEST_A_SIZE_BIG];
    int arr1_15[TEST_A_SIZE_BIG];
    int arr1_16[TEST_A_SIZE_BIG];
    int arr1_17[TEST_A_SIZE_BIG];
    int arr1_18[TEST_A_SIZE_BIG];
    int arr1_19[TEST_A_SIZE_BIG];
    int arr1_20[TEST_A_SIZE_BIG];
    int arr1_21[TEST_A_SIZE_BIG];
    int arr1_22[TEST_A_SIZE_BIG];
    int arr1_23[TEST_A_SIZE_BIG];
    int arr1_24[TEST_A_SIZE_BIG];
    int arr1_25[TEST_A_SIZE_BIG];
    int arr1_26[TEST_A_SIZE_BIG];
    int arr1_27[TEST_A_SIZE_BIG];
    int arr1_28[TEST_A_SIZE_BIG];
    int arr1_29[TEST_A_SIZE_BIG];
    int arr1_30[TEST_A_SIZE_BIG];
    int arr1_31[TEST_A_SIZE_BIG];
    int arr1_32[TEST_A_SIZE_BIG];
    int arr1_33[TEST_A_SIZE_BIG];
    int arr1_34[TEST_A_SIZE_BIG];
    int arr1_35[TEST_A_SIZE_BIG];
    int arr1_36[TEST_A_SIZE_BIG];
    int arr1_37[TEST_A_SIZE_BIG];
    int arr1_38[TEST_A_SIZE_BIG];
    int arr1_39[TEST_A_SIZE_BIG];
    int arr1_40[TEST_A_SIZE_BIG];
    int arr1_41[TEST_A_SIZE_BIG];
    int arr1_42[TEST_A_SIZE_BIG];
    int arr1_43[TEST_A_SIZE_BIG];
    int arr1_44[TEST_A_SIZE_BIG];
    int arr1_45[TEST_A_SIZE_BIG];
    int arr1_46[TEST_A_SIZE_BIG];
    int arr1_47[TEST_A_SIZE_BIG];
    int arr1_48[TEST_A_SIZE_BIG];
    int arr1_49[TEST_A_SIZE_BIG];
    int arr1_50[TEST_A_SIZE_BIG];
    int arr1_51[TEST_A_SIZE_BIG];

    int var2;
    test_a_other_struct_2 arr2[TEST_A_SIZE_BIG];

    int var3;
    test_a_other_struct_3 arr3[TEST_A_SIZE_BIG];

    int var4;
    test_a_other_struct_4 arr4[TEST_A_SIZE_BIG];

    int var5;
    test_a_other_struct_5 arr5[TEST_A_SIZE_BIG];

    int var6;
    test_a_other_struct_6 arr6[TEST_A_SIZE_BIG];

    int var7;
    test_a_other_struct_7 arr7[TEST_A_SIZE_BIG];

    int var8;
    test_a_other_struct_8 arr8[TEST_A_SIZE_BIG];

    int var9;
    test_a_other_struct_9 arr9[TEST_A_SIZE_BIG];

    int var10;
    test_a_other_struct_10 arr10[TEST_A_SIZE_BIG];

    int var11;
    test_a_other_struct_11 arr11[TEST_A_SIZE_BIG];

    int var12;
    test_a_other_struct_12 arr12[TEST_A_SIZE_BIG];

    int var13;
    test_a_other_struct_13 arr13[TEST_A_SIZE_BIG];

    int var14;
    test_a_other_struct_14 arr14[TEST_A_SIZE_BIG];

    int var15;
    test_a_other_struct_15 arr15[TEST_A_SIZE_BIG];

    int var16;
    test_a_other_struct_16 arr16[TEST_A_SIZE_BIG];

    int var17;
    test_a_other_struct_17 arr17[TEST_A_SIZE_BIG];

    int var18;
    test_a_other_struct_18 arr18[TEST_A_SIZE_BIG];

    int var19;
    test_a_other_struct_19 arr19[TEST_A_SIZE_BIG];

    int var20;
    test_a_other_struct_20 arr20[TEST_A_SIZE_BIG];

    int var21;
    test_a_other_struct_21 arr21[TEST_A_SIZE_BIG];

    int var22;
    test_a_other_struct_22 arr22[TEST_A_SIZE_BIG];

    int var23;
    test_a_other_struct_23 arr23[TEST_A_SIZE_BIG];

    int var24;
    test_a_other_struct_24 arr24[TEST_A_SIZE_BIG];

    int var25;
    test_a_other_struct_25 arr25[TEST_A_SIZE_BIG];

    int var26;
    test_a_other_struct_26 arr26[TEST_A_SIZE_BIG];

    int var27;
    test_a_other_struct_27 arr27[TEST_A_SIZE_BIG];

    int var28;
    test_a_other_struct_28 arr28[TEST_A_SIZE_BIG];

    int var29;
    test_a_other_struct_29 arr29[TEST_A_SIZE_BIG];

    int var30;
    test_a_other_struct_30 arr30[TEST_A_SIZE_BIG];

    int var31;
    test_a_other_struct_31 arr31[TEST_A_SIZE_BIG];

    int var32;
    test_a_other_struct_32 arr32[TEST_A_SIZE_BIG];

    int var33;
    test_a_other_struct_33 arr33[TEST_A_SIZE_BIG];

    int var34;
    test_a_other_struct_34 arr34[TEST_A_SIZE_BIG];

    int var35;
    test_a_other_struct_35 arr35[TEST_A_SIZE_BIG];

    int var36;
    test_a_other_struct_36 arr36[TEST_A_SIZE_BIG];

    int var37;
    test_a_other_struct_37 arr37[TEST_A_SIZE_BIG];

    int var38;
    test_a_other_struct_38 arr38[TEST_A_SIZE_BIG];

    int var39;
    test_a_other_struct_39 arr39[TEST_A_SIZE_BIG];

    int var40;
    test_a_other_struct_40 arr40[TEST_A_SIZE_BIG];

    int var41;
    test_a_other_struct_41 arr41[TEST_A_SIZE_BIG];

    int var42;
    test_a_other_struct_42 arr42[TEST_A_SIZE_BIG];

    int var43;
    test_a_other_struct_43 arr43[TEST_A_SIZE_BIG];

    int var44;
    test_a_other_struct_44 arr44[TEST_A_SIZE_BIG];

    int var45;
    test_a_other_struct_45 arr45[TEST_A_SIZE_BIG];

    int var46;
    test_a_other_struct_46 arr46[TEST_A_SIZE_BIG];

    int var47;
    test_a_other_struct_47 arr47[TEST_A_SIZE_BIG];

    int var48;
    test_a_other_struct_48 arr48[TEST_A_SIZE_BIG];

    int var49;
    test_a_other_struct_49 arr49[TEST_A_SIZE_BIG];

    int var50;
    test_a_other_struct_50 arr50[TEST_A_SIZE_BIG];

    int var51;
    test_a_other_struct_51 arr51[TEST_A_SIZE_BIG];
}
test_a_big_struct_1;

typedef struct
{
    int var1;
    int arr1_1[TEST_A_SIZE_SMALL];
    int arr1_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr1_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr1_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var2;
    int arr2_1[TEST_A_SIZE_SMALL];
    int arr2_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr2_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr2_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var3;
    int arr3_1[TEST_A_SIZE_SMALL];
    int arr3_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr3_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr3_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var4;
    int arr4_1[TEST_A_SIZE_SMALL];
    int arr4_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr4_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr4_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var5;
    int arr5_1[TEST_A_SIZE_SMALL];
    int arr5_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr5_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr5_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var6;
    int arr6_1[TEST_A_SIZE_SMALL];
    int arr6_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr6_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr6_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var7;
    int arr7_1[TEST_A_SIZE_SMALL];
    int arr7_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr7_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr7_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var8;
    int arr8_1[TEST_A_SIZE_SMALL];
    int arr8_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr8_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr8_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var9;
    int arr9_1[TEST_A_SIZE_SMALL];
    int arr9_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr9_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr9_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var10;
    int arr10_1[TEST_A_SIZE_SMALL];
    int arr10_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr10_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr10_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var11;
    int arr11_1[TEST_A_SIZE_SMALL];
    int arr11_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr11_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr11_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var12;
    int arr12_1[TEST_A_SIZE_SMALL];
    int arr12_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr12_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr12_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var13;
    int arr13_1[TEST_A_SIZE_SMALL];
    int arr13_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr13_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr13_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var14;
    int arr14_1[TEST_A_SIZE_SMALL];
    int arr14_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr14_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr14_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var15;
    int arr15_1[TEST_A_SIZE_SMALL];
    int arr15_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr15_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr15_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var16;
    int arr16_1[TEST_A_SIZE_SMALL];
    int arr16_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr16_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr16_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var17;
    int arr17_1[TEST_A_SIZE_SMALL];
    int arr17_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr17_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr17_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var18;
    int arr18_1[TEST_A_SIZE_SMALL];
    int arr18_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr18_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr18_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var19;
    int arr19_1[TEST_A_SIZE_SMALL];
    int arr19_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr19_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr19_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var20;
    int arr20_1[TEST_A_SIZE_SMALL];
    int arr20_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr20_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr20_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var21;
    int arr21_1[TEST_A_SIZE_SMALL];
    int arr21_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr21_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr21_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var22;
    int arr22_1[TEST_A_SIZE_SMALL];
    int arr22_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr22_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr22_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var23;
    int arr23_1[TEST_A_SIZE_SMALL];
    int arr23_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr23_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr23_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var24;
    int arr24_1[TEST_A_SIZE_SMALL];
    int arr24_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr24_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr24_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var25;
    int arr25_1[TEST_A_SIZE_SMALL];
    int arr25_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr25_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr25_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var26;
    int arr26_1[TEST_A_SIZE_SMALL];
    int arr26_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr26_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr26_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var27;
    int arr27_1[TEST_A_SIZE_SMALL];
    int arr27_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr27_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr27_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var28;
    int arr28_1[TEST_A_SIZE_SMALL];
    int arr28_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr28_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr28_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var29;
    int arr29_1[TEST_A_SIZE_SMALL];
    int arr29_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr29_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr29_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var30;
    int arr30_1[TEST_A_SIZE_SMALL];
    int arr30_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr30_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr30_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var31;
    int arr31_1[TEST_A_SIZE_SMALL];
    int arr31_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr31_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr31_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var32;
    int arr32_1[TEST_A_SIZE_SMALL];
    int arr32_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr32_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr32_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var33;
    int arr33_1[TEST_A_SIZE_SMALL];
    int arr33_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr33_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr33_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var34;
    int arr34_1[TEST_A_SIZE_SMALL];
    int arr34_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr34_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr34_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var35;
    int arr35_1[TEST_A_SIZE_SMALL];
    int arr35_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr35_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr35_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var36;
    int arr36_1[TEST_A_SIZE_SMALL];
    int arr36_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr36_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr36_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var37;
    int arr37_1[TEST_A_SIZE_SMALL];
    int arr37_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr37_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr37_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var38;
    int arr38_1[TEST_A_SIZE_SMALL];
    int arr38_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr38_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr38_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var39;
    int arr39_1[TEST_A_SIZE_SMALL];
    int arr39_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr39_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr39_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var40;
    int arr40_1[TEST_A_SIZE_SMALL];
    int arr40_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr40_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr40_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var41;
    int arr41_1[TEST_A_SIZE_SMALL];
    int arr41_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr41_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr41_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var42;
    int arr42_1[TEST_A_SIZE_SMALL];
    int arr42_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr42_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr42_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var43;
    int arr43_1[TEST_A_SIZE_SMALL];
    int arr43_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr43_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr43_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var44;
    int arr44_1[TEST_A_SIZE_SMALL];
    int arr44_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr44_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr44_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var45;
    int arr45_1[TEST_A_SIZE_SMALL];
    int arr45_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr45_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr45_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var46;
    int arr46_1[TEST_A_SIZE_SMALL];
    int arr46_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr46_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr46_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var47;
    int arr47_1[TEST_A_SIZE_SMALL];
    int arr47_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr47_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr47_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var48;
    int arr48_1[TEST_A_SIZE_SMALL];
    int arr48_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr48_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr48_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var49;
    int arr49_1[TEST_A_SIZE_SMALL];
    int arr49_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr49_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr49_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];

    int var50;
    int arr50_1[TEST_A_SIZE_SMALL];
    int arr50_2[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
    int arr50_3[TEST_A_SIZE_SMALL];
    test_a_other_struct_1 arr50_4[TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL][TEST_A_SIZE_SMALL];
}
test_a_big_struct_2;

void test_a_print(test_a_big_struct_1 *big_struct_1, test_a_big_struct_2 *big_struct_2);
  
