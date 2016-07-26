
class CLKComplicationTemplate : NSObject, NSCopying {
  @NSCopying var tintColor: UIColor?
}
class CLKComplicationTemplateModularSmallSimpleText : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
}
class CLKComplicationTemplateModularSmallSimpleImage : CLKComplicationTemplate {
  @NSCopying var imageProvider: CLKImageProvider
}
class CLKComplicationTemplateModularSmallRingText : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
  var fillFraction: Float
  var ringStyle: CLKComplicationRingStyle
}
class CLKComplicationTemplateModularSmallRingImage : CLKComplicationTemplate {
  @NSCopying var imageProvider: CLKImageProvider
  var fillFraction: Float
  var ringStyle: CLKComplicationRingStyle
}
class CLKComplicationTemplateModularSmallStackText : CLKComplicationTemplate {
  @NSCopying var line1TextProvider: CLKTextProvider
  @NSCopying var line2TextProvider: CLKTextProvider
  var highlightLine2: Bool
}
class CLKComplicationTemplateModularSmallStackImage : CLKComplicationTemplate {
  @NSCopying var line1ImageProvider: CLKImageProvider
  @NSCopying var line2TextProvider: CLKTextProvider
  var highlightLine2: Bool
}
class CLKComplicationTemplateModularSmallColumnsText : CLKComplicationTemplate {
  @NSCopying var row1Column1TextProvider: CLKTextProvider
  @NSCopying var row1Column2TextProvider: CLKTextProvider
  @NSCopying var row2Column1TextProvider: CLKTextProvider
  @NSCopying var row2Column2TextProvider: CLKTextProvider
  var column2Alignment: CLKComplicationColumnAlignment
  var highlightColumn2: Bool
}
class CLKComplicationTemplateModularLargeStandardBody : CLKComplicationTemplate {
  @NSCopying var headerTextProvider: CLKTextProvider
  @NSCopying var body1TextProvider: CLKTextProvider
  @NSCopying var body2TextProvider: CLKTextProvider?
  @NSCopying var headerImageProvider: CLKImageProvider?
}
class CLKComplicationTemplateModularLargeTallBody : CLKComplicationTemplate {
  @NSCopying var headerTextProvider: CLKTextProvider
  @NSCopying var bodyTextProvider: CLKTextProvider
}
class CLKComplicationTemplateModularLargeTable : CLKComplicationTemplate {
  @NSCopying var headerTextProvider: CLKTextProvider
  @NSCopying var row1Column1TextProvider: CLKTextProvider
  @NSCopying var row1Column2TextProvider: CLKTextProvider
  @NSCopying var row2Column1TextProvider: CLKTextProvider
  @NSCopying var row2Column2TextProvider: CLKTextProvider
  @NSCopying var headerImageProvider: CLKImageProvider?
  var column2Alignment: CLKComplicationColumnAlignment
}
class CLKComplicationTemplateModularLargeColumns : CLKComplicationTemplate {
  @NSCopying var row1Column1TextProvider: CLKTextProvider
  @NSCopying var row1Column2TextProvider: CLKTextProvider
  @NSCopying var row2Column1TextProvider: CLKTextProvider
  @NSCopying var row2Column2TextProvider: CLKTextProvider
  @NSCopying var row3Column1TextProvider: CLKTextProvider
  @NSCopying var row3Column2TextProvider: CLKTextProvider
  @NSCopying var row1ImageProvider: CLKImageProvider?
  @NSCopying var row2ImageProvider: CLKImageProvider?
  @NSCopying var row3ImageProvider: CLKImageProvider?
  var column2Alignment: CLKComplicationColumnAlignment
}
class CLKComplicationTemplateUtilitarianSmallFlat : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
  @NSCopying var imageProvider: CLKImageProvider?
}
class CLKComplicationTemplateUtilitarianSmallSquare : CLKComplicationTemplate {
  @NSCopying var imageProvider: CLKImageProvider
}
class CLKComplicationTemplateUtilitarianSmallRingText : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
  var fillFraction: Float
  var ringStyle: CLKComplicationRingStyle
}
class CLKComplicationTemplateUtilitarianSmallRingImage : CLKComplicationTemplate {
  @NSCopying var imageProvider: CLKImageProvider
  var fillFraction: Float
  var ringStyle: CLKComplicationRingStyle
}
class CLKComplicationTemplateUtilitarianLargeFlat : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
  @NSCopying var imageProvider: CLKImageProvider?
}
class CLKComplicationTemplateCircularSmallSimpleText : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
}
class CLKComplicationTemplateCircularSmallSimpleImage : CLKComplicationTemplate {
  @NSCopying var imageProvider: CLKImageProvider
}
class CLKComplicationTemplateCircularSmallRingText : CLKComplicationTemplate {
  @NSCopying var textProvider: CLKTextProvider
  var fillFraction: Float
  var ringStyle: CLKComplicationRingStyle
}
class CLKComplicationTemplateCircularSmallRingImage : CLKComplicationTemplate {
  @NSCopying var imageProvider: CLKImageProvider
  var fillFraction: Float
  var ringStyle: CLKComplicationRingStyle
}
class CLKComplicationTemplateCircularSmallStackText : CLKComplicationTemplate {
  @NSCopying var line1TextProvider: CLKTextProvider
  @NSCopying var line2TextProvider: CLKTextProvider
}
class CLKComplicationTemplateCircularSmallStackImage : CLKComplicationTemplate {
  @NSCopying var line1ImageProvider: CLKImageProvider
  @NSCopying var line2TextProvider: CLKTextProvider
}
