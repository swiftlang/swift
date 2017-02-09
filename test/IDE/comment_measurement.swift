// RUN: %target-swift-ide-test -new-mangling-for-tests -print-comments -source-filename %s -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng | %FileCheck %s

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

// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceLineMeasurement()</Name><USR>s:14swift_ide_test20spaceLineMeasurementyyF</USR><Declaration>func spaceLineMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceBlockMeasurement()</Name><USR>s:14swift_ide_test21spaceBlockMeasurementyyF</USR><Declaration>func spaceBlockMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabLineMeasurement()</Name><USR>s:14swift_ide_test18tabLineMeasurementyyF</USR><Declaration>func tabLineMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabBlockMeasurement()</Name><USR>s:14swift_ide_test19tabBlockMeasurementyyF</USR><Declaration>func tabBlockMeasurement()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceLineMeasurementIndented()</Name><USR>s:14swift_ide_test28spaceLineMeasurementIndentedyyF</USR><Declaration>func spaceLineMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>spaceBlockMeasurementIndented()</Name><USR>s:14swift_ide_test29spaceBlockMeasurementIndentedyyF</USR><Declaration>func spaceBlockMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabLineMeasurementIndented()</Name><USR>s:14swift_ide_test26tabLineMeasurementIndentedyyF</USR><Declaration>func tabLineMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
// CHECK: {{.*}} DocCommentAsXML=[<Function file="{{.*}}" line="{{.*}}" column="{{.*}}"><Name>tabBlockMeasurementIndented()</Name><USR>s:14swift_ide_test27tabBlockMeasurementIndentedyyF</USR><Declaration>func tabBlockMeasurementIndented()</Declaration><Abstract><Para>Brief.</Para></Abstract><Discussion><Para>This is not a code block.</Para></Discussion></Function>]
