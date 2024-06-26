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
package org.threeten.bp.chrono

import org.threeten.bp.DateTimeException
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField.ERA
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.UnsupportedTemporalTypeException
import org.threeten.bp.temporal.ValueRange

object HijrahEra {

  /**
   * The singleton instance for the era before the current one, 'Before Anno Hegirae', which has the
   * value 0.
   */
  lazy val BEFORE_AH = new HijrahEra("BEFORE_AH", 0)

  /** The singleton instance for the current era, 'Anno Hegirae', which has the value 1. */
  lazy val AH = new HijrahEra("AH", 1)

  lazy val values: Array[HijrahEra] = Array(BEFORE_AH, AH)

  /**
   * Obtains an instance of {@code HijrahEra} from a value.
   *
   * The current era (from ISO date 622-06-19 onwards) has the value 1 The previous era has the
   * value 0.
   *
   * @param hijrahEra
   *   the era to represent, from 0 to 1
   * @return
   *   the HijrahEra singleton, never null
   * @throws DateTimeException
   *   if the era is invalid
   */
  def of(hijrahEra: Int): HijrahEra =
    hijrahEra match {
      case 0 => BEFORE_AH
      case 1 => AH
      case _ => throw new DateTimeException("HijrahEra not valid")
    }

}

/**
 * An era in the Hijrah calendar system.
 *
 * The Hijrah calendar system has two eras. The date {@code 0001-01-01 (Hijrah)} is {@code 622-06-19
 * (ISO)}.
 *
 * <b>Do not use {@code ordinal()} to obtain the numeric representation of {@code HijrahEra}. Use
 * {@code getValue()} instead.</b>
 *
 * <h3>Specification for implementors</h3> This is an immutable and thread-safe enum.
 */
final class HijrahEra(name: String, ordinal: Int) extends Enum[HijrahEra](name, ordinal) with Era {

  /**
   * Gets the era numeric value.
   *
   * The current era (from ISO date 622-06-19 onwards) has the value 1. The previous era has the
   * value 0.
   *
   * @return
   *   the era value, from 0 (BEFORE_AH) to 1 (AH)
   */
  def getValue: Int = ordinal

  override def range(field: TemporalField): ValueRange =
    if (field eq ERA) ValueRange.of(1, 1)
    else if (field.isInstanceOf[ChronoField])
      throw new UnsupportedTemporalTypeException(s"Unsupported field: $field")
    else field.rangeRefinedBy(this)

  /**
   * Returns the proleptic year from this era and year of era.
   *
   * @param yearOfEra
   *   the year of Era
   * @return
   *   the computed prolepticYear
   */
  private[chrono] def prolepticYear(yearOfEra: Int): Int =
    if (this eq HijrahEra.AH) yearOfEra else 1 - yearOfEra
}
