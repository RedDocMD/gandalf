GdsBgnLib
  {GdsLibName, GdsUnits, GdsBgnStr}
GdsBgnStr
  {GdsStrName, GdsBoundary}
  {GdsStrName, GdsBoundary, GdsPath, GdsSref, GdsText}
  {GdsStrName, GdsBoundary, GdsPath, GdsText}
  {GdsStrName, GdsBoundary, GdsText}
GdsBoundary
  {GdsLayer, GdsDataType, GdsXy}
GdsPath
  {GdsLayer, GdsDataType, GdsWidth, GdsXy, GdsPathType}
  {GdsLayer, GdsDataType, GdsWidth, GdsXy, GdsPathType, GdsBgnExtn, GdsEndExtn}
GdsSref
  {GdsXy, GdsSname}
  {GdsXy, GdsSname, GdsStrans}
  {GdsXy, GdsSname, GdsStrans, GdsAngle}
GdsText
  {GdsLayer, GdsXy, GdsTextType, GdsPresentation, GdsString, GdsStrans, GdsMag}
  {GdsLayer, GdsXy, GdsTextType, GdsPresentation, GdsString, GdsStrans, GdsMag, GdsAngle}
  {GdsLayer, GdsXy, GdsTextType, GdsString}
  {GdsLayer, GdsXy, GdsTextType, GdsString, GdsStrans, GdsMag, GdsAngle}
