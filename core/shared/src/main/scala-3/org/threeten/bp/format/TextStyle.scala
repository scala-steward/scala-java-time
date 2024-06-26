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
package org.threeten.bp.format

/**
 * Enumeration of the style of text formatting and parsing.
 *
 * Text styles define three sizes for the formatted text - 'full', 'short' and 'narrow'. Each of
 * these three sizes is available in both 'standard' and 'stand-alone' variations.
 *
 * The difference between the three sizes is obvious in most languages. For example, in English the
 * 'full' month is 'January', the 'short' month is 'Jan' and the 'narrow' month is 'J'. Note that
 * the narrow size is often not unique. For example, 'January', 'June' and 'July' all have the
 * 'narrow' text 'J'.
 *
 * The difference between the 'standard' and 'stand-alone' forms is trickier to describe as there is
 * no difference in English. However, in other languages there is a difference in the word used when
 * the text is used alone, as opposed to in a complete date. For example, the word used for a month
 * when used alone in a date picker is different to the word used for month in association with a
 * day and year in a date.
 *
 * <h3>Specification for implementors</h3> This is immutable and thread-safe enum.
 */

enum TextStyle(name: String, ordinal: Int) extends java.lang.Enum[TextStyle] {
  case FULL              extends TextStyle("FULL", 0)
  case FULL_STANDALONE   extends TextStyle("FULL_STANDALONE", 1)
  case SHORT             extends TextStyle("SHORT", 2)
  case SHORT_STANDALONE  extends TextStyle("SHORT_STANDALONE", 3)
  case NARROW            extends TextStyle("NARROW", 4)
  case NARROW_STANDALONE extends TextStyle("NARROW_STANDALONE", 5)

  /**
   * Checks if the style is stand-alone.
   *
   * @return
   *   true if the style is stand-alone
   */
  def isStandalone: Boolean = (ordinal & 1) == 1

  /**
   * Converts the style to the equivalent stand-alone style.
   *
   * @return
   *   the matching stand-alone style
   */
  def asStandalone: TextStyle = TextStyle.values(ordinal | 1)

  /**
   * Converts the style to the equivalent normal style.
   *
   * @return
   *   the matching normal style
   */
  def asNormal: TextStyle = TextStyle.values(ordinal & ~1)
}
