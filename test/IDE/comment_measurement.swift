// RUN: %target-swift-ide-test -print-comments -source-filename %s -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng | %FileCheck %s

/// Brief.
///
/// This is not a code block.
func spaceLineMeasurement() {}

/**
  Brief.

  This is not a code block.
*/
func spaceBlockMeasurement() {}

/// Brief.
///
/// This is not a code block.
func tabLineMeasurement() {}

/**
	Brief.

	This is not a code block.
*/
func tabBlockMeasurement() {}

    /// Brief.
    ///
    /// This is not a code block.
    func spaceLineMeasurementIndented() {}

    /**
      Brief.

      This is not a code block.
    */
    func spaceBlockMeasurementIndented() {}

		/// Brief.
		///
		/// This is not a code block.
		func tabLineMeasurementIndented() {}

		/**
			Brief.

			This is not a code block.
		*/
		func tabBlockMeasurementIndented() {}

// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceLineMeasurement()</Name><USR>s:F14swift_ide_test20spaceLineMeasurementFT_T_</USR><Declaration>func spaceLineMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceBlockMeasurement()</Name><USR>s:F14swift_ide_test21spaceBlockMeasurementFT_T_</USR><Declaration>func spaceBlockMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabLineMeasurement()</Name><USR>s:F14swift_ide_test18tabLineMeasurementFT_T_</USR><Declaration>func tabLineMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabBlockMeasurement()</Name><USR>s:F14swift_ide_test19tabBlockMeasurementFT_T_</USR><Declaration>func tabBlockMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceLineMeasurementIndented()</Name><USR>s:F14swift_ide_test28spaceLineMeasurementIndentedFT_T_</USR><Declaration>func spaceLineMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceBlockMeasurementIndented()</Name><USR>s:F14swift_ide_test29spaceBlockMeasurementIndentedFT_T_</USR><Declaration>func spaceBlockMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabLineMeasurementIndented()</Name><USR>s:F14swift_ide_test26tabLineMeasurementIndentedFT_T_</USR><Declaration>func tabLineMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabBlockMeasurementIndented()</Name><USR>s:F14swift_ide_test27tabBlockMeasurementIndentedFT_T_</USR><Declaration>func tabBlockMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
