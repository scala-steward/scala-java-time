/*
 * Copyright (c) 2007-present, Stephen Colebourne & Michael Nascimento Santos
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of JSR-310 nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.threeten.bp.format.internal

import org.threeten.bp.temporal.ChronoField.AMPM_OF_DAY
import org.threeten.bp.temporal.ChronoField.DAY_OF_WEEK
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.ChronoField.MONTH_OF_YEAR
import java.lang.{ Long => JLong }
import java.text.DateFormatSymbols
import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Locale
import java.util.HashMap
import java.util.{ Map => JMap }

import org.threeten.bp.temporal.IsoFields
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.format.TextStyle

object TTBPSimpleDateTimeTextProvider {

  private def createLocaleStore(
    valueTextMap: Map[TextStyle, LocaleStore.Item]
  ): TTBPSimpleDateTimeTextProvider.LocaleStore = {
    val tmp1 =
      (valueTextMap + (TextStyle.FULL_STANDALONE -> valueTextMap
        .get(TextStyle.FULL)
        .orNull)) + (TextStyle.SHORT_STANDALONE  -> valueTextMap.get(TextStyle.SHORT).orNull)
    val tmp2 =
      if (
        valueTextMap.contains(TextStyle.NARROW) && !valueTextMap
          .contains(TextStyle.NARROW_STANDALONE)
      )
        tmp1 + (TextStyle.NARROW_STANDALONE -> valueTextMap.get(TextStyle.NARROW).orNull)
      else
        tmp1
    new TTBPSimpleDateTimeTextProvider.LocaleStore(tmp2)
  }

  /**
   * Stores the text for a single locale.
   *
   * Some fields have a textual representation, such as day-of-week or month-of-year. These textual
   * representations can be captured in this class for printing and parsing.
   *
   * This class is immutable and thread-safe.
   *
   * @constructor
   *
   * @param valueTextMap
   *   the map of values to text to store, assigned and not altered, not null
   */
  private[format] final class LocaleStore(
    private val valueTextMap: Map[TextStyle, LocaleStore.Item]
  ) {

    /** Parsable data. */
    private val parsable: (List[(String, Long)], Map[TextStyle, List[(String, Long)]]) = {
      val u = valueTextMap.foldLeft(
        (List.empty[(String, Long)], Map.empty[TextStyle, List[(String, Long)]])
      ) { case ((all, map), (style, entries)) =>
        val reverse =
          entries.sorted.foldLeft((true, Map.empty[String, (String, Long)])) {
            case (a @ (false, _), _)   => a
            case ((true, acc), (k, v)) =>
              val continue = !acc.contains(v)
              (continue, acc + (v -> (v -> k)))
          }
        val list    = reverse._2.values.toList.sortBy(x => -x._1.length)
        (all ::: list, map + (style -> list))
      }
      (u._1.sortBy(x => -x._1.length), u._2)
    }

    /**
     * Gets the text for the specified field value, locale and style for the purpose of printing.
     *
     * @param value
     *   the value to get text for, not null
     * @param style
     *   the style to get text for, not null
     * @return
     *   the text for the field value, null if no text found
     */
    def getText(value: Long, style: TextStyle): String =
      valueTextMap.get(style).flatMap(_.get(value)).orNull

    /**
     * Gets an iterator of text to field for the specified style for the purpose of parsing.
     *
     * The iterator must be returned in order from the longest text to the shortest.
     *
     * @param style
     *   the style to get text for, null for all parsable text
     * @return
     *   the iterator of text to field pairs, in order from longest text to shortest text, null if
     *   the style is not parsable
     */
    def getTextIterator(style: TextStyle): Iterator[(String, Long)] =
      parsable._2.getOrElse(style, parsable._1).iterator
  }

  private[format] object LocaleStore {
    sealed abstract class Item {
      def get(index: Long): Option[String]
      def sorted: List[(Long, String)]
    }

    final class MapItem(
      underlying: JMap[JLong, String]
    ) extends Item {
      def get(index: Long): Option[String] = Option(underlying.get(index))

      def sorted: List[(Long, String)] = {
        import scala.collection.JavaConverters._
        underlying
          .entrySet()
          .asScala
          .map(entry => (entry.getKey.toLong, entry.getValue))
          .toList
          .sortBy(_._1)
      }
    }

    class ArrayItem(
      array: Array[String],
      start: Int,
      end:   Int
    ) extends Item {
      def get(index: Long): Option[String] =
        if (index >= start && index < end) Some(processString(array(index.toInt - start))) else None

      def sorted: List[(Long, String)] = {
        var i   = end - 1
        var acc = List.empty[(Long, String)]
        while (i >= start) {
          val text = processString(array(i))
          i -= 1
          acc = (i.toLong, text) :: acc
        }
        acc
      }

      def processString(s: String): String = s
    }

    final class ArrayItemFirstChar(array: Array[String], start: Int, end: Int)
        extends ArrayItem(array, start, end) {
      override def processString(s: String): String = s.substring(0, 1)
    }
  }
}

/**
 * The Service Provider Implementation to obtain date-time text for a field.
 *
 * This implementation is based on extraction of data from a {@link DateFormatSymbols}.
 *
 * <h3>Specification for implementors</h3> This class is immutable and thread-safe.
 */
final class TTBPSimpleDateTimeTextProvider extends TTBPDateTimeTextProvider {

  /** Cache. */
  private val cache: JMap[(TemporalField, Locale), AnyRef] =
    new HashMap[(TemporalField, Locale), AnyRef]()

  override def getText(
    field:  TemporalField,
    value:  Long,
    style:  TextStyle,
    locale: Locale
  ): String =
    findStore(field, locale) match {
      case store: TTBPSimpleDateTimeTextProvider.LocaleStore => store.getText(value, style)
      case _                                                 => null
    }

  override def getTextIterator(
    field:  TemporalField,
    style:  TextStyle,
    locale: Locale
  ): Iterator[(String, Long)] =
    findStore(field, locale) match {
      case store: TTBPSimpleDateTimeTextProvider.LocaleStore => store.getTextIterator(style)
      case _                                                 => null
    }

  private def findStore(field: TemporalField, locale: Locale): AnyRef = {
    val key           = (field, locale)
    var store: AnyRef = cache.get(key)
    if (store == null) {
      store = createStore(field, locale)
      cache.put(key, store)
      store = cache.get(key)
    }
    store
  }

  private def createStore(field: TemporalField, locale: Locale): AnyRef =
    if (field eq MONTH_OF_YEAR) {
      val oldSymbols       = DateFormatSymbols.getInstance(locale)
      val monthsArray      = oldSymbols.getMonths
      val itemF            =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(monthsArray, 0, 12)
      val itemN            =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItemFirstChar(
          monthsArray,
          0,
          12
        )
      val shortMonthsArray = oldSymbols.getShortMonths
      val itemS            =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(shortMonthsArray, 0, 12)
      val styleMap         =
        Map(
          TextStyle.FULL   -> itemF,
          TextStyle.NARROW -> itemN,
          TextStyle.SHORT  -> itemS
        )
      TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    } else if (field eq DAY_OF_WEEK) {
      val oldSymbols: DateFormatSymbols = DateFormatSymbols.getInstance(locale)
      val weekdaysArray                 =
        oldSymbols.getWeekdays
      val itemF                         =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(weekdaysArray, 1, 8)
      val itemN                         =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItemFirstChar(weekdaysArray, 1, 8)
      val itemS                         =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(oldSymbols.getShortWeekdays, 1, 8)
      val styleMap                      = Map(
        TextStyle.FULL   -> itemF,
        TextStyle.NARROW -> itemN,
        TextStyle.SHORT  -> itemS
      )
      TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    } else if (field eq AMPM_OF_DAY) {
      val oldSymbols = DateFormatSymbols.getInstance(locale)
      val array      = oldSymbols.getAmPmStrings
      val item       = new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(array, 0, 2)
      val styleMap   =
        Map(TextStyle.FULL -> item, TextStyle.SHORT -> item)
      TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    } else if (field eq ERA) {
      val oldSymbols = DateFormatSymbols.getInstance(locale)
      val array      = oldSymbols.getEras
      val itemS      = new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(
        array,
        0,
        2
      )
      val itemF      =
        if (locale.getLanguage == Locale.ENGLISH.getLanguage)
          new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(
            Array("Before Christ", "Anno Domini"),
            0,
            2
          )
        else
          itemS
      val itemN      = new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItemFirstChar(
        array,
        0,
        2
      )
      val styleMap   =
        Map(
          TextStyle.SHORT  -> itemS,
          TextStyle.FULL   -> itemF,
          TextStyle.NARROW -> itemN
        )
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    } else if (field eq IsoFields.QUARTER_OF_YEAR) {
      val itemS    =
        new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(Array("Q1", "Q2", "Q3", "Q4"),
                                                                 1,
                                                                 5
        )
      val itemF    = new TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem(
        Array("1st quarter", "2nd quarter", "3rd quarter", "4th quarter"),
        1,
        5
      )
      val styleMap =
        Map[TextStyle, TTBPSimpleDateTimeTextProvider.LocaleStore.ArrayItem](
          TextStyle.SHORT -> itemS,
          TextStyle.FULL  -> itemF
        )
      return TTBPSimpleDateTimeTextProvider.createLocaleStore(styleMap)
    } else ""
}
